const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const lang = @import("lang.zig");
const TypeId = lang.Value.TypeId;
const Node = lang.Node;
const Tree = lang.Tree;
const TokenList = lang.Token.List;
const TokenIndex = lang.Token.Index;
const RegRef = lang.RegRef;

pub const Error = error{CompileError} || Allocator.Error;

pub const Compiler = struct {
    tree: *Tree,
    arena: *Allocator,
    root_scope: Scope.Fn,
    cur_scope: *Scope,
    used_regs: RegRef = 0,
    code: *Code,
    module_code: Code,

    pub const Code = std.ArrayList(u8);

    fn registerAlloc(self: *Compiler) RegRef {
        const reg = self.used_regs;
        self.used_regs += 1;
        return reg;
    }

    fn registerFree(self: *Compiler, reg: RegRef) void {
        if (reg == self.used_regs - 1) {
            self.used_regs -= 1;
        }
    }

    // TODO improve?
    fn emitInstruction_1(self: *Compiler, op: lang.Op, A: RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{A})[0..]));
    }

    fn emitInstruction_1_1(self: *Compiler, op: lang.Op, A: RegRef, arg: var) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{A})[0..]));
        try self.code.appendSlice(@sliceToBytes(([_]@TypeOf(arg){arg})[0..]));
    }

    fn emitInstruction_1_2(self: *Compiler, op: lang.Op, A: RegRef, arg: var, arg2: var) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{A})[0..]));
        try self.code.appendSlice(@sliceToBytes(([_]@TypeOf(arg){arg})[0..]));
        try self.code.appendSlice(@sliceToBytes(([_]@TypeOf(arg2){arg2})[0..]));
    }

    fn emitInstruction_0_1(self: *Compiler, op: lang.Op, arg: var) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]@TypeOf(arg){arg})[0..]));
    }

    fn emitInstruction_2(self: *Compiler, op: lang.Op, A: RegRef, B: RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{ A, B })[0..]));
    }

    fn emitInstruction_2_1(self: *Compiler, op: lang.Op, A: RegRef, B: RegRef, arg: var) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{ A, B })[0..]));
        try self.code.appendSlice(@sliceToBytes(([_]@TypeOf(arg){arg})[0..]));
    }

    fn emitInstruction_3(self: *Compiler, op: lang.Op, A: RegRef, B: RegRef, C: RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{ A, B, C })[0..]));
    }

    const Scope = struct {
        id: Id,
        parent: ?*Scope,
        syms: Symbol.List,

        const Id = enum {
            Fn,
            Loop,
            Block,
            Capture,
        };

        const Fn = struct {
            base: Scope,
            code: Code,
        };

        fn declSymbol(self: *Scope, sym: Symbol) !void {
            try self.syms.push(sym);
        }

        fn getSymbol(self: *Scope, name: []const u8) ?*Symbol {
            var it = self.syms.iterator(self.syms.len);
            while (it.prev()) |sym| {
                if (mem.eql(u8, sym.name, name)) {
                    return sym;
                }
            }
            // TODO self.parent
            if (self.parent) |some| {
                if (self.id == .Fn) @panic("TODO: closures");
                return some.getSymbol(name);
            }
            return null;
        }
    };

    pub const Symbol = struct {
        name: []const u8,
        reg: RegRef,
        mutable: bool,

        pub const List = std.SegmentedList(Symbol, 4);
    };

    const Value = union(enum) {
        /// result of continue, break, return and assignmnet; cannot exist at runtime
        Empty,
        Rt: RegRef,

        None,
        Int: i64,
        Num: f64,
        Bool: bool,
        Str: []const u8,
    };

    fn makeRuntime(self: *Compiler, res: RegRef, val: Value) Error!void {
        return switch (val) {
            .Empty => unreachable,
            .Rt => |v| assert(v == res),
            .None => try self.emitInstruction_1_1(.ConstPrimitive, res, @as(u8, 0)),
            .Int => |v| if (v > std.math.minInt(i8) and v < std.math.maxInt(i8)) {
                try self.emitInstruction_1_1(.ConstInt8, res, @truncate(i8, v));
            } else if (v > std.math.minInt(i32) and v < std.math.maxInt(i32)) {
                try self.emitInstruction_1_1(.ConstInt32, res, @truncate(i32, v));
            } else {
                try self.emitInstruction_1_1(.ConstInt64, res, v);
            },
            .Num => |v| try self.emitInstruction_1_1(.ConstNum, res, v),
            .Bool => |v| try self.emitInstruction_1_1(.ConstPrimitive, res, @as(u8, @boolToInt(v)) + 1),
            // .Str => |v| try self.builder.constStr(res, v),
            else => unreachable,
        };
    }

    const Result = union(enum) {
        /// A runtime value is expected
        Rt: RegRef,

        /// Something assignable is expected
        Lval: union(enum) {
            Const: RegRef,
            Let: RegRef,
            Assign: RegRef,
            AugAssign,
        },

        /// A value, runtime or constant, is expected
        Value,

        /// No value is expected if some is given it will be discarded
        Discard,
    };

    pub fn compile(tree: *Tree, allocator: *Allocator) (Error || lang.Parser.Error)!lang.Module {
        const arena = &tree.arena_allocator.allocator;
        var compiler = Compiler{
            .tree = tree,
            .arena = arena,
            .root_scope = .{
                .base = .{
                    .id = .Fn,
                    .parent = null,
                    .syms = Symbol.List.init(arena),
                },
                .code = Code.init(arena),
            },
            .module_code = Code.init(allocator),
            .code = undefined,
            .cur_scope = undefined,
        };
        compiler.code = &compiler.root_scope.code;
        compiler.cur_scope = &compiler.root_scope.base;

        var it = tree.nodes.iterator(0);
        while (it.next()) |n| {
            try compiler.addLineInfo(n.*);

            const last = it.peek() == null;
            const res = if (last)
                Result{ .Value = {} }
            else
                Result{ .Discard = {} };

            const val = try compiler.genNode(n.*, res);
            if (last) {
                if (val == .Rt) {
                    try compiler.emitInstruction_1(.Return, val.Rt);
                    break;
                }
                const reg = compiler.registerAlloc();
                defer compiler.registerFree(reg);
                try compiler.makeRuntime(reg, val);
                try compiler.emitInstruction_1(.Return, reg);
            }
            if (val == .Rt) {
                // discard unused runtime value
                try compiler.emitInstruction_1(.Discard, val.Rt);
                compiler.registerFree(val.Rt);
            }
        }

        const start_index = compiler.module_code.len;
        try compiler.module_code.appendSlice(compiler.code.toSliceConst());
        return lang.Module{
            .name = "",
            .code = compiler.module_code.toOwnedSlice(),
            .strings = "",
            .start_index = @truncate(u32, start_index),
        };
    }

    pub fn compileRepl(compiler: *Compiler, node: *Node, module: *lang.Module) Error!usize {
        const start_len = compiler.module_code.len;
        try compiler.addLineInfo(node);
        const val = try compiler.genNode(node, .Value);
        if (val == .Rt) {
            try compiler.emitInstruction_1(.Discard, val.Rt);
        } else if (val != .Empty) {
            const reg = compiler.registerAlloc();
            defer compiler.registerFree(reg);
            try compiler.makeRuntime(reg, val);
            try compiler.emitInstruction_1(.Discard, reg);
        }
        const final_len = compiler.module_code.len;
        try compiler.module_code.appendSlice(compiler.code.toSliceConst());

        module.code = compiler.module_code.toSliceConst();
        compiler.module_code.resize(final_len) catch unreachable;
        return final_len - start_len;
    }

    fn genNode(self: *Compiler, node: *Node, res: Result) Error!Value {
        switch (node.id) {
            .Grouped => return self.genNode(@fieldParentPtr(Node.Grouped, "base", node).expr, res),
            .Literal => return self.genLiteral(@fieldParentPtr(Node.Literal, "base", node), res),
            .Block => return self.genBlock(@fieldParentPtr(Node.Block, "base", node), res),
            .Prefix => return self.genPrefix(@fieldParentPtr(Node.Prefix, "base", node), res),
            .Decl => return self.genDecl(@fieldParentPtr(Node.Decl, "base", node), res),
            .Identifier => return self.genIdentifier(@fieldParentPtr(Node.SingleToken, "base", node), res),
            .Infix => return self.genInfix(@fieldParentPtr(Node.Infix, "base", node), res),
            .If => return self.genIf(@fieldParentPtr(Node.If, "base", node), res),
            .Tuple => return self.genTuple(@fieldParentPtr(Node.ListTupleMap, "base", node), res),
            .Discard => return self.reportErr("'_' can only be used to discard unwanted tuple/list items in destructuring assignment", node.firstToken()),
            .TypeInfix => return self.genTypeInfix(@fieldParentPtr(Node.TypeInfix, "base", node), res),
            .Fn => return self.genFn(@fieldParentPtr(Node.Fn, "base", node), res),
            .Suffix => return self.genSuffix(@fieldParentPtr(Node.Suffix, "base", node), res),
            .Error => return self.genError(@fieldParentPtr(Node.Error, "base", node), res),
            .Import => return self.reportErr("TODO: Import", node.firstToken()),
            .List => return self.reportErr("TODO: List", node.firstToken()),
            .Map => return self.reportErr("TODO: Map", node.firstToken()),
            .Catch => return self.reportErr("TODO: Catch", node.firstToken()),
            .For => return self.reportErr("TODO: For", node.firstToken()),
            .While => return self.reportErr("TODO: While", node.firstToken()),
            .Match => return self.reportErr("TODO: Match", node.firstToken()),
            .Jump => return self.reportErr("TODO: Jump", node.firstToken()),
            .MapItem,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            => unreachable,
        }
    }

    fn genTuple(self: *Compiler, node: *Node.ListTupleMap, res: Result) Error!Value {
        if (res == .Lval) {
            switch (res.Lval) {
                .Const, .Let, .Assign => |reg| {
                    const index_reg = self.registerAlloc();
                    var sub_reg = self.registerAlloc();
                    var index_val = Value{
                        .Int = 0,
                    };

                    var it = node.values.iterator(0);
                    while (it.next()) |n| {
                        if (n.*.id == .Discard) {
                            index_val.Int += 1;
                            continue;
                        }
                        try self.makeRuntime(index_reg, index_val);
                        try self.emitInstruction_3(.Subscript, sub_reg, reg, index_reg);
                        const l_val = try self.genNode(n.*, switch (res.Lval) {
                            .Const => Result{ .Lval = .{ .Const = sub_reg } },
                            .Let => Result{ .Lval = .{ .Let = sub_reg } },
                            .Assign => Result{ .Lval = .{ .Assign = sub_reg } },
                            else => unreachable,
                        });
                        std.debug.assert(l_val == .Empty);
                        index_val.Int += 1;

                        // TODO this should probably be done in genIdentifier
                        if (it.peek() != null and res.Lval != .Assign) sub_reg = self.registerAlloc();
                    }
                    return Value.Empty;
                },
                .AugAssign => {
                    return self.reportErr("invalid left hand side to augmented assignment", node.r_tok);
                },
            }
        }
        const res_loc = if (res == .Rt) res else Result{ .Rt = self.registerAlloc() };
        const args = try self.arena.alloc(RegRef, node.values.len);
        for (args) |*a| {
            a.* = self.registerAlloc();
        }

        var i: u32 = 0;
        var it = node.values.iterator(0);
        while (it.next()) |n| {
            _ = try self.genNode(n.*, Result{ .Rt = args[i] });
            i += 1;
        }

        try self.emitInstruction_2_1(.BuildTuple, res_loc.Rt, args[0], @truncate(u16, args.len));
        return Value{ .Rt = res_loc.Rt };
    }

    fn genFn(self: *Compiler, node: *Node.Fn, res: Result) Error!Value {
        try self.assertNotLval(res, node.fn_tok);

        if (node.params.len > std.math.maxInt(u8)) {
            return self.reportErr("too many parameters", node.fn_tok);
        }

        const old_used_regs = self.used_regs;
        defer self.used_regs = old_used_regs;

        var fn_scope = Scope.Fn{
            .base = .{
                .id = .Block,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.arena),
            },
            .code = try Code.initCapacity(self.arena, 256),
        };
        defer fn_scope.code.deinit();
        self.cur_scope = &fn_scope.base;
        defer self.cur_scope = fn_scope.base.parent.?;

        // function body is emitted to a new arraylist and finally added to module_code
        const old_code = self.code;
        self.code = &fn_scope.code;

        // destructure parameters
        self.used_regs = @truncate(u16, node.params.len);
        var it = node.params.iterator(0);
        var i: RegRef = 0;
        while (it.next()) |n| {
            const param_res = try self.genNode(n.*, Result{
                .Lval = .{
                    .Let = i,
                },
            });
            std.debug.assert(param_res == .Empty);
            i += 1;
        }

        // gen body and return result
        const res_loc = if (res == .Rt) res else Result{ .Rt = self.registerAlloc() };
        try self.addLineInfo(node.body);
        const body_res = try self.genNode(node.body, .Value);
        // TODO if body_res == .Empty because last instruction was a return
        // then this return is not necessary
        if (body_res == .Empty or body_res == .None) {
            try self.code.append(@enumToInt(lang.Op.ReturnNone));
        } else if (body_res == .Rt) {
            try self.emitInstruction_1(.Return, body_res.Rt);
        } else {
            try self.makeRuntime(res_loc.Rt, body_res);
            try self.emitInstruction_1(.Return, res_loc.Rt);
        }

        self.code = old_code;
        try self.emitInstruction_1_2(
            .BuildFn,
            res_loc.Rt,
            @truncate(u8, node.params.len),
            @truncate(u32, self.module_code.len),
        );
        try self.module_code.appendSlice(fn_scope.code.toSlice());
        return Value{ .Rt = res_loc.Rt };
    }

    fn genBlock(self: *Compiler, node: *Node.Block, res: Result) Error!Value {
        try self.assertNotLval(res, node.stmts.at(0).*.firstToken());
        var block_scope = Scope{
            .id = .Block,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.arena),
        };
        self.cur_scope = &block_scope;
        defer self.cur_scope = block_scope.parent.?;

        var it = node.stmts.iterator(0);
        while (it.next()) |n| {
            try self.addLineInfo(n.*);

            // return value of last instruction if it is not discarded
            if (it.peek() == null and res != .Discard) {
                return self.genNode(n.*, res);
            }

            const val = try self.genNode(n.*, .Discard);
            if (val == .Rt) {
                // discard unused runtime value
                try self.emitInstruction_1(.Discard, val.Rt);
                self.registerFree(val.Rt);
            }
        }
        return Value{ .Empty = {} };
    }

    fn genIf(self: *Compiler, node: *Node.If, res: Result) Error!Value {
        try self.assertNotLval(res, node.if_tok);

        if (node.capture) |some| return self.reportErr("TODO if let", some.firstToken());

        const cond_val = try self.genNode(node.cond, .Value);
        if (cond_val != .Rt) {
            try self.assertBool(cond_val, node.cond.firstToken());

            if (cond_val.Bool) {
                return self.genNode(node.if_body, res);
            } else if (node.else_body) |some| {
                return self.genNode(some, res);
            }

            const res_val = Value{ .None = {} };
            if (res == .Rt) {
                try self.makeRuntime(res.Rt, res_val);
                return Value{ .Rt = res.Rt };
            } else return res_val;
        }
        const sub_res = switch (res) {
            .Rt, .Discard => res,
            else => Result{
                .Rt = self.registerAlloc(),
            },
        };

        // jump past if_body if cond == false
        try self.emitInstruction_1_1(.JumpFalse, cond_val.Rt, @as(u32, 0));
        const addr = self.code.len - @sizeOf(u32);
        const if_val = try self.genNode(node.if_body, sub_res);
        if (sub_res != .Rt and if_val == .Rt) {
            try self.emitInstruction_1(.Discard, if_val.Rt);
        }

        // jump past else_body since if_body was executed
        try self.emitInstruction_0_1(.Jump, @as(u32, 0));
        const addr2 = self.code.len - @sizeOf(u32);

        @ptrCast(*align(1) u32, self.code.toSlice()[addr..].ptr).* = @truncate(u32, self.code.len - addr - @sizeOf(u32));
        if (node.else_body) |some| {
            const else_val = try self.genNode(some, sub_res);
            if (sub_res != .Rt and else_val == .Rt) {
                try self.emitInstruction_1(.Discard, else_val.Rt);
            }
        } else if (sub_res == .Rt) {
            try self.emitInstruction_1_1(.ConstPrimitive, sub_res.Rt, @as(u8, 0));
        }

        @ptrCast(*align(1) u32, self.code.toSlice()[addr2..].ptr).* = @truncate(u32, self.code.len - addr2 - @sizeOf(u32));

        return if (sub_res == .Rt)
            Value{
                .Rt = sub_res.Rt,
            }
        else
            Value{ .Empty = {} };
    }

    fn assertNotLval(self: *Compiler, res: Result, tok: TokenIndex) !void {
        if (res == .Lval) {
            return self.reportErr("invalid left hand side to assignment", tok);
        }
    }

    fn assertNotEmpty(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val == .Empty) {
            return self.reportErr("expected a value", tok);
        }
    }

    fn assertBool(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val != .Bool) {
            return self.reportErr("expected a boolean", tok);
        }
    }

    fn assertInt(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val != .Int) {
            return self.reportErr("expected an integer", tok);
        }
    }

    fn assertNumeric(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val != .Int and val != .Num) {
            return self.reportErr("expected a number", tok);
        }
        if (val == .Num) {
            return self.reportErr("TODO operations on real numbers", tok);
        }
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);
        const r_val = try self.genNode(node.rhs, .Value);
        try self.assertNotEmpty(r_val, node.rhs.firstToken());
        if (r_val == .Rt) {
            const op_id = switch (node.op) {
                .BoolNot => .BoolNot,
                .BitNot => .BitNot,
                .Minus => .Negate,
                // TODO should unary + be a no-op
                .Plus => return r_val,
                .Try => lang.Op.Try,
            };
            // TODO r_val should be freed here
            if (res == .Rt) {
                try self.emitInstruction_2(op_id, res.Rt, r_val.Rt);
                return Value{ .Rt = res.Rt };
            } else {
                const reg = self.registerAlloc();
                try self.emitInstruction_2(op_id, reg, r_val.Rt);
                return Value{ .Rt = reg };
            }
        }
        const ret_val = switch (node.op) {
            .BoolNot => blk: {
                try self.assertBool(r_val, node.rhs.firstToken());
                break :blk Value{ .Bool = !r_val.Bool };
            },
            .BitNot => blk: {
                try self.assertInt(r_val, node.rhs.firstToken());
                break :blk Value{ .Int = ~r_val.Int };
            },
            .Minus => blk: {
                try self.assertNumeric(r_val, node.rhs.firstToken());
                if (r_val == .Int) {
                    // TODO check for overflow
                    break :blk Value{ .Int = -r_val.Int };
                } else {
                    break :blk Value{ .Num = -r_val.Num };
                }
            },
            .Plus => blk: {
                try self.assertNumeric(r_val, node.rhs.firstToken());
                break :blk r_val;
            },
            // errors are runtime only currently, so ret_val does not need to be checked
            // TODO should this be an error?
            .Try => r_val,
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .Value nothing needs to be done
        return ret_val;
    }

    fn genTypeInfix(self: *Compiler, node: *Node.TypeInfix, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);
        const l_val = try self.genNode(node.lhs, .Value);
        try self.assertNotEmpty(l_val, node.lhs.firstToken());

        const type_str = self.tokenSlice(node.type_tok);
        const type_id = if (mem.eql(u8, type_str, "none"))
            .None
        else if (mem.eql(u8, type_str, "int"))
            .Int
        else if (mem.eql(u8, type_str, "num"))
            .Num
        else if (mem.eql(u8, type_str, "bool"))
            .Bool
        else if (mem.eql(u8, type_str, "str"))
            .Str
        else if (mem.eql(u8, type_str, "tuple"))
            .Tuple
        else if (mem.eql(u8, type_str, "map"))
            .Map
        else if (mem.eql(u8, type_str, "list"))
            .List
        else if (mem.eql(u8, type_str, "error"))
            .Error
        else if (mem.eql(u8, type_str, "range"))
            .Range
        else if (mem.eql(u8, type_str, "fn"))
            lang.Value.TypeId.Fn
        else
            return self.reportErr("expected a type name", node.type_tok);

        if (l_val == .Rt) {
            const res_loc = if (res == .Rt) res else Result{ .Rt = self.registerAlloc() };
            const op: lang.Op = if (node.op == .As) .As else .Is;
            try self.emitInstruction_2_1(op, res_loc.Rt, l_val.Rt, type_id);
            return Value{ .Rt = res_loc.Rt };
        }

        const ret = switch (node.op) {
            .As => switch (type_id) {
                .None => Value{ .None = {} },
                .Int => Value{
                    .Int = switch (l_val) {
                        .Int => |val| val,
                        .Num => |val| @floatToInt(i64, val),
                        .Bool => |val| @boolToInt(val),
                        // .Str => parseInt
                        else => return self.reportErr("invalid cast to int", node.lhs.firstToken()),
                    },
                },
                .Num => Value{
                    .Num = switch (l_val) {
                        .Num => |val| val,
                        .Int => |val| @intToFloat(f64, val),
                        .Bool => |val| @intToFloat(f64, @boolToInt(val)),
                        // .Str => parseNum
                        else => return self.reportErr("invalid cast to num", node.lhs.firstToken()),
                    },
                },
                .Bool => Value{
                    .Bool = switch (l_val) {
                        .Int => |val| val != 0,
                        .Num => |val| val != 0,
                        .Bool => |val| val,
                        .Str => |val| if (mem.eql(u8, val, "true"))
                            true
                        else if (mem.eql(u8, val, "false"))
                            false
                        else
                            return self.reportErr("cannot cast string to bool", node.lhs.firstToken()),
                        else => return self.reportErr("invalid cast to bool", node.lhs.firstToken()),
                    },
                },
                .Str => Value{
                    .Str = switch (l_val) {
                        // .Int => |val| val != 0,
                        // .Num => |val| val != 0,
                        .Bool => |val| if (val) "true" else "false",
                        .Str => |val| val,
                        else => return self.reportErr("invalid cast to string", node.lhs.firstToken()),
                    },
                },
                .Fn => return self.reportErr("cannot cast to function", node.type_tok),
                .Error => return self.reportErr("cannot cast to error", node.type_tok),
                .Range => return self.reportErr("cannot cast to range", node.type_tok),
                .Tuple, .Map, .List => return self.reportErr("TODO Rt casts", node.tok),
                _ => unreachable,
            },
            .Is => Value{
                .Bool = switch (type_id) {
                    .None => l_val == .None,
                    .Int => l_val == .Int,
                    .Num => l_val == .Num,
                    .Bool => l_val == .Bool,
                    .Str => l_val == .Str,
                    else => false,
                },
            },
        };

        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret);
            return Value{ .Rt = res.Rt };
        }
        // if res == .Value nothing needs to be done
        return ret;
    }

    fn genSuffix(self: *Compiler, node: *Node.Suffix, res: Result) Error!Value {
        if (node.op == .Call) {
            try self.assertNotLval(res, node.r_tok);
        }
        const l_val = try self.genNode(node.lhs, .Value);
        if (l_val != .Rt) {
            return self.reportErr("Invalid left hand side to suffix op", node.lhs.firstToken());
        }
        switch (node.op) {
            .Call => |*args| {
                const res_loc = if (res == .Rt) res else Result{ .Rt = self.registerAlloc() };
                const len = if (args.len == 0) 1 else args.len;
                const arg_locs = try self.arena.alloc(RegRef, len);
                for (arg_locs) |*a| {
                    a.* = self.registerAlloc();
                }

                var i: u32 = 0;
                var it = args.iterator(0);
                while (it.next()) |n| {
                    _ = try self.genNode(n.*, Result{ .Rt = arg_locs[i] });
                    i += 1;
                }

                try self.emitInstruction_2_1(.Call, l_val.Rt, arg_locs[0], @truncate(u16, len));
                if (res == .Rt) {
                    // TODO probably should handle this better
                    try self.emitInstruction_2(.Move, res.Rt, arg_locs[0]);
                    return Value{ .Rt = res.Rt };
                }
                return Value{ .Rt = arg_locs[0] };
            },
            .Member => return self.reportErr("TODO: member access", node.l_tok),
            .Subscript => |val| {
                const res_reg = switch (res) {
                    .Rt => |r| r,
                    .Lval => |l| switch (l) {
                        .Let, .Const => return self.reportErr("cannot declare to subscript", node.l_tok),
                        .AugAssign => self.registerAlloc(),
                        else => return self.reportErr("TODO: assign to subscript", node.l_tok),
                    },
                    .Discard, .Value => self.registerAlloc(),
                };

                const val_res = try self.genNode(val, .Value);
                try self.assertNotEmpty(val_res, val.firstToken());

                const reg = self.registerAlloc();
                defer self.registerFree(reg);
                try self.makeRuntime(reg, val_res);

                try self.emitInstruction_3(.Subscript, res_reg, l_val.Rt, reg);
                return Value{ .Rt = res_reg };
            },
        }
    }

    fn genInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);
        switch (node.op) {
            .BoolOr,
            .BoolAnd,
            => {},

            .LessThan,
            .LessThanEqual,
            .GreaterThan,
            .GreaterThanEqual,
            .Equal,
            .NotEqual,
            .In,
            => {},

            .Range => {},

            .BitAnd,
            .BitOr,
            .BitXor,
            .LShift,
            .RShift,
            => {},

            .Add,
            .Sub,
            .Mul,
            .Div,
            .DivFloor,
            .Mod,
            .Pow,
            => return self.genNumericInfix(node, res),

            .Assign,
            .AddAssign,
            .SubAssign,
            .MulAssign,
            .PowAssign,
            .DivAssign,
            .DivFloorAssign,
            .ModAssign,
            .LShiftAssign,
            .RShfitAssign,
            .BitAndAssign,
            .BitOrAssign,
            .BitXOrAssign,
            => return self.genAssignInfix(node, res),
        }
        return self.reportErr("TODO more infix ops", node.tok);
    }

    fn genAssignInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        if (res == .Rt) {
            return self.reportErr("assignment produces no value", node.tok);
        }
        const reg = self.registerAlloc();
        defer self.registerFree(reg);
        const r_val = try self.genNode(node.rhs, Result{ .Rt = reg });
        try self.assertNotEmpty(r_val, node.rhs.firstToken());

        if (node.op == .Assign) {
            const l_val = try self.genNode(node.lhs, Result{ .Lval = .{ .Assign = reg } });
            std.debug.assert(l_val == .Empty);
            return l_val;
        }

        const l_val = try self.genNode(node.lhs, Result{ .Lval = .AugAssign });

        const op_id = switch (node.op) {
            .AddAssign => lang.Op.DirectAdd,
            .SubAssign => .DirectSub,
            .MulAssign => .DirectMul,
            .PowAssign => .DirectPow,
            .DivAssign => .DirectDiv,
            .DivFloorAssign => .DirectDivFloor,
            .ModAssign => .DirectMod,
            .LShiftAssign => .DirectLShift,
            .RShfitAssign => .DirectRShift,
            .BitAndAssign => .DirectBitAnd,
            .BitOrAssign => .DirectBitOr,
            .BitXOrAssign => .DirectBitXor,
            else => unreachable,
        };

        try self.emitInstruction_2(op_id, l_val.Rt, r_val.Rt);
        return Value.Empty;
    }

    fn genNumericInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNode(node.lhs, .Value);
        try self.assertNotEmpty(l_val, node.lhs.firstToken());

        var r_val = try self.genNode(node.rhs, .Value);
        try self.assertNotEmpty(r_val, node.rhs.firstToken());

        if (r_val == .Rt or l_val == .Rt) {
            if (r_val != .Rt) {
                try self.assertNumeric(r_val, node.tok);
                const reg = self.registerAlloc();
                try self.makeRuntime(reg, r_val);
                r_val = Value{ .Rt = reg };
            }
            if (l_val != .Rt) {
                try self.assertNumeric(l_val, node.tok);
                const reg = self.registerAlloc();
                try self.makeRuntime(reg, l_val);
                l_val = Value{ .Rt = reg };
            }
            const op_id = switch (node.op) {
                .Add => .Add,
                .Sub => .Sub,
                .Mul => .Mul,
                .Div => .Div,
                .DivFloor => .DivFloor,
                .Mod => .Mod,
                .Pow => lang.Op.Pow,
                else => unreachable,
            };
            // TODO r_val and l_val should be freed here
            if (res == .Rt) {
                try self.emitInstruction_3(op_id, res.Rt, l_val.Rt, r_val.Rt);
                return Value{ .Rt = res.Rt };
            } else {
                const reg = self.registerAlloc();
                try self.emitInstruction_3(op_id, reg, l_val.Rt, r_val.Rt);
                return Value{ .Rt = reg };
            }
        }
        try self.assertNumeric(r_val, node.tok);
        try self.assertNumeric(l_val, node.tok);

        // TODO makeRuntime if overflow
        // TODO decay to numeric
        const ret_val = switch (node.op) {
            .Add => blk: {
                break :blk Value{ .Int = l_val.Int + r_val.Int };
            },
            .Sub => blk: {
                break :blk Value{ .Int = l_val.Int - r_val.Int };
            },
            .Mul => blk: {
                break :blk Value{ .Int = l_val.Int * r_val.Int };
            },
            .Div => blk: {
                return self.reportErr("TODO division", node.tok);
                // break :blk Value{ .Num = std.math.div(l_val.Int, r_val.Int) };
            },
            .DivFloor => blk: {
                break :blk Value{ .Int = @divFloor(l_val.Int, r_val.Int) };
            },
            .Mod => blk: {
                return self.reportErr("TODO modulo", node.tok);
                // break :blk Value{ .Int =std.math.rem(i64, l_val.Int, r_val.Int) catch @panic("TODO") };
            },
            .Pow => blk: {
                break :blk Value{
                    .Int = std.math.powi(i64, l_val.Int, r_val.Int) catch
                        return self.reportErr("TODO integer overflow", node.tok),
                };
            },
            else => unreachable,
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .Value nothing needs to be done
        return ret_val;
    }

    fn genDecl(self: *Compiler, node: *Node.Decl, res: Result) Error!Value {
        assert(res != .Lval);
        const r_loc = self.registerAlloc();
        assert((try self.genNode(node.value, Result{ .Rt = r_loc })) == .Rt);

        const lval_kind = if (self.tree.tokens.at(node.let_const).id == .Keyword_let)
            Result{ .Lval = .{ .Let = r_loc } }
        else
            Result{ .Lval = .{ .Const = r_loc } };

        assert((try self.genNode(node.capture, lval_kind)) == .Empty);
        return Value.Empty;
    }

    fn genIdentifier(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        const name = self.tokenSlice(node.tok);
        if (res == .Lval) {
            switch (res.Lval) {
                .Let, .Const => |r| {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        return self.reportErr("redeclaration of identifier", node.tok);
                    }
                    // TODO this should copy r if r.refs > 1
                    try self.cur_scope.declSymbol(.{
                        .name = name,
                        .mutable = res.Lval == .Let,
                        .reg = r,
                    });
                    return Value.Empty;
                },
                .Assign => |r| {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        if (!sym.mutable) {
                            return self.reportErr("assignment to constant", node.tok);
                        }
                        // TODO this should copy r if r.refs > 1
                        // TODO this move can usually be avoided
                        try self.emitInstruction_2(.Move, sym.reg, r);
                        return Value.Empty;
                    }
                },
                .AugAssign => {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        if (!sym.mutable) {
                            return self.reportErr("assignment to constant", node.tok);
                        }
                        return Value{ .Rt = sym.reg };
                    }
                },
            }
        } else if (self.cur_scope.getSymbol(name)) |sym| {
            if (res == .Rt) {
                try self.emitInstruction_2(.Move, res.Rt, sym.reg);
                return Value{ .Rt = res.Rt };
            }
            return Value{ .Rt = sym.reg };
        }
        return self.reportErr("use of undeclared identifier", node.tok);
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);
        const ret_val: Value = switch (node.kind) {
            .Int => .{ .Int = try self.parseInt(node.tok) },
            .True => .{ .Bool = true },
            .False => .{ .Bool = false },
            .None => .None,
            .Str => return self.reportErr("TODO genLiteral string", node.tok),
            .Num => .{ .Num = try self.parseNum(node.tok) },
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .Value nothing needs to be done
        return ret_val;
    }

    fn genError(self: *Compiler, node: *Node.Error, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);

        const val = try self.genNode(node.value, .Value);
        try self.assertNotEmpty(val, node.value.firstToken());

        const res_loc = if (res == .Rt) res else Result{ .Rt = self.registerAlloc() };
        const reg = self.registerAlloc();
        defer self.registerFree(reg);
        try self.makeRuntime(reg, val);

        try self.emitInstruction_2(.BuildError, res_loc.Rt, reg);
        return Value{ .Rt = res_loc.Rt };
    }

    fn addLineInfo(self: *Compiler, node: *Node) !void {
        const token = node.firstToken();
        const tok = self.tree.tokens.at(token);

        try self.emitInstruction_0_1(.LineInfo, tok.start);
    }

    fn tokenSlice(self: *Compiler, token: TokenIndex) []const u8 {
        const tok = self.tree.tokens.at(token);
        return self.tree.source[tok.start..tok.end];
    }

    fn parseInt(self: *Compiler, tok: TokenIndex) !i64 {
        var buf = self.tokenSlice(tok);
        var radix: u8 = if (buf.len > 2) switch (buf[1]) {
            'x' => @as(u8, 16),
            'b' => 2,
            'o' => 8,
            else => 10,
        } else 10;
        if (radix != 10) buf = buf[2..];
        var x: i64 = 0;

        for (buf) |c| {
            const digit = switch (c) {
                '0'...'9' => c - '0',
                'A'...'Z' => c - 'A' + 10,
                'a'...'z' => c - 'a' + 10,
                '_' => continue,
                else => unreachable,
            };

            x = std.math.mul(i64, x, radix) catch {
                // try self.adderr("TODO bigint");
                return error.CompileError;
            };
            // why is this cast needed?
            x += @intCast(i32, digit);
        }

        return x;
    }

    fn parseNum(self: *Compiler, tok: TokenIndex) !f64 {
        var buf: [256]u8 = undefined;
        const slice = self.tokenSlice(tok);

        var i: u32 = 0;
        for (slice) |c| {
            if (c != '_') {
                buf[i] = c;
                i += 1;
            }
        }

        return std.fmt.parseFloat(f64, buf[0..i]) catch {
            // "invalid real number"
            return error.CompileError;
        };
    }

    fn reportErr(self: *Compiler, msg: []const u8, tok: TokenIndex) Error {
        try self.tree.errors.push(.{
            .msg = msg,
            .kind = .Error,
            .index = self.tree.tokens.at(tok).start,
        });
        return error.CompileError;
    }
};
