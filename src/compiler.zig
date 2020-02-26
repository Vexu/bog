const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const bog = @import("bog.zig");
const TypeId = bog.Value.TypeId;
const Node = bog.Node;
const Tree = bog.Tree;
const TokenList = bog.Token.List;
const TokenIndex = bog.Token.Index;
const RegRef = bog.RegRef;
const Errors = bog.Errors;

pub const Error = error{CompileError} || Allocator.Error;

pub const Compiler = struct {
    tree: *Tree,
    errors: *Errors,
    arena: *Allocator,
    root_scope: Scope.Fn,
    cur_scope: *Scope,
    used_regs: RegRef = 0,
    code: *Code,
    module_code: Code,
    strings: Code,

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

    fn emitInstruction(self: *Compiler, op: bog.Op, args: var) !void {
        try self.code.append(@enumToInt(op));
        inline for (std.meta.fields(@TypeOf(args))) |f| {
            try self.code.appendSlice(mem.asBytes(&@field(args, f.name)));
        }
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

        const Loop = struct {
            base: Scope,
            breaks: BreakList,
            cond_begin: u32,

            const BreakList = std.SegmentedList(u32, 4);
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

        /// reference to a variable
        Ref: RegRef,

        None,
        Int: i64,
        Num: f64,
        Bool: bool,
        Str: []u8,

        fn isRt(val: Value) bool {
            return switch (val) {
                .Rt, .Ref => true,
                else => false,
            };
        }

        fn maybeRt(val: Value, self: *Compiler, res: Result) !Value {
            if (res == .Rt) {
                try self.makeRuntime(res.Rt, val);
                return res.toVal();
            }
            return val;
        }

        fn free(val: Value, self: *Compiler, reg: RegRef) void {
            if (val != .Ref) {
                self.registerFree(reg);
            }
        }

        fn toRt(val: Value, self: *Compiler) !RegRef {
            switch (val) {
                .Rt, .Ref => |r| return r,
                .Empty => unreachable,
                else => {
                    const reg = self.registerAlloc();
                    try self.makeRuntime(reg, val);
                    return reg;
                },
            }
        }

        fn getRt(val: Value) RegRef {
            switch (val) {
                .Rt, .Ref => |r| return r,
                else => unreachable,
            }
        }

        fn getBool(val: Value, self: *Compiler, tok: TokenIndex) !bool {
            if (val != .Bool) {
                return self.reportErr("expected a boolean", tok);
            }
            return val.Bool;
        }

        fn getInt(val: Value, self: *Compiler, tok: TokenIndex) !i64 {
            if (val != .Int) {
                return self.reportErr("expected an integer", tok);
            }
            return val.Int;
        }

        fn getNum(val: Value) f64 {
            return switch (val) {
                .Int => |v| @intToFloat(f64, v),
                .Num => |v| v,
                else => unreachable,
            };
        }

        fn getStr(val: Value, self: *Compiler, tok: TokenIndex) ![]const u8 {
            if (val != .Str) {
                return self.reportErr("expected a string", tok);
            }
            return val.Str;
        }

        fn checkNum(val: Value, self: *Compiler, tok: TokenIndex) !void {
            if (val != .Int and val != .Num) {
                return self.reportErr("expected a number", tok);
            }
        }
    };

    fn makeRuntime(self: *Compiler, res: RegRef, val: Value) Error!void {
        return switch (val) {
            .Empty => unreachable,
            .Ref, .Rt => |v| assert(v == res),
            .None => try self.emitInstruction(.ConstPrimitive, .{ res, @as(u8, 0) }),
            .Int => |v| if (v > std.math.minInt(i8) and v < std.math.maxInt(i8)) {
                try self.emitInstruction(.ConstInt8, .{ res, @truncate(i8, v) });
            } else if (v > std.math.minInt(i32) and v < std.math.maxInt(i32)) {
                try self.emitInstruction(.ConstInt32, .{ res, @truncate(i32, v) });
            } else {
                try self.emitInstruction(.ConstInt64, .{ res, v });
            },
            .Num => |v| try self.emitInstruction(.ConstNum, .{ res, v }),
            .Bool => |v| try self.emitInstruction(.ConstPrimitive, .{ res, @as(u8, @boolToInt(v)) + 1 }),
            .Str => |v| try self.emitInstruction(.ConstString, .{ res, try self.putString(v) }),
        };
    }

    fn putString(self: *Compiler, str: []const u8) !u32 {
        const len = @intCast(u32, self.strings.len);
        try self.strings.appendSlice(mem.asBytes(&@intCast(u32, str.len)));
        try self.strings.appendSlice(str);
        return len;
    }

    const Result = union(enum) {
        /// A runtime value is expected
        Rt: RegRef,

        /// Something assignable is expected
        Lval: union(enum) {
            Const: *const Value,
            Let: *const Value,
            Assign: *const Value,
            AugAssign,
        },

        /// A value, runtime or constant, is expected
        Value,

        /// No value is expected if some is given it will be discarded
        Discard,

        fn toRt(res: Result, compiler: *Compiler) Result {
            return if (res == .Rt) res else Result{ .Rt = compiler.registerAlloc() };
        }

        fn toVal(res: Result) Value {
            return .{ .Rt = res.Rt };
        }

        fn notLval(res: Result, self: *Compiler, tok: TokenIndex) !void {
            if (res == .Lval) {
                return self.reportErr("invalid left hand side to assignment", tok);
            }
        }
    };

    pub fn compile(allocator: *Allocator, source: []const u8, errors: *Errors) (Error || bog.Parser.Error || bog.Tokenizer.Error)!bog.Module {
        var tree = try bog.parse(allocator, source, errors);
        const arena = &tree.arena_allocator.allocator;
        var compiler = Compiler{
            .errors = errors,
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
            .strings = Code.init(allocator),
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
                const reg = try val.toRt(&compiler);
                defer val.free(&compiler, reg);

                try compiler.emitInstruction(.Return, .{reg});
            } else if (val.isRt()) {
                const reg = val.getRt();
                defer val.free(&compiler, reg);
                // discard unused runtime value
                try compiler.emitInstruction(.Discard, .{reg});
            }
        }

        const start_index = compiler.module_code.len;
        try compiler.module_code.appendSlice(compiler.code.toSliceConst());
        return bog.Module{
            .name = "",
            .code = compiler.module_code.toOwnedSlice(),
            .strings = compiler.strings.toOwnedSlice(),
            .start_index = @truncate(u32, start_index),
        };
    }

    pub fn compileRepl(self: *Compiler, node: *Node, module: *bog.Module) Error!usize {
        const start_len = self.module_code.len;
        try self.addLineInfo(node);
        const val = try self.genNode(node, .Discard);
        if (val != .Empty) {
            const reg = try val.toRt(self);
            defer val.free(self, reg);

            try self.emitInstruction(.Discard, .{reg});
        }
        const final_len = self.module_code.len;
        try self.module_code.appendSlice(self.code.toSliceConst());

        module.code = self.module_code.toSliceConst();
        self.module_code.resize(final_len) catch unreachable;
        module.strings = self.strings.toSliceConst();
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
            .Tuple => return self.genTupleList(@fieldParentPtr(Node.ListTupleMap, "base", node), res),
            .Discard => return self.reportErr("'_' can only be used to discard unwanted tuple/list items in destructuring assignment", node.firstToken()),
            .TypeInfix => return self.genTypeInfix(@fieldParentPtr(Node.TypeInfix, "base", node), res),
            .Fn => return self.genFn(@fieldParentPtr(Node.Fn, "base", node), res),
            .Suffix => return self.genSuffix(@fieldParentPtr(Node.Suffix, "base", node), res),
            .Error => return self.genError(@fieldParentPtr(Node.Error, "base", node), res),
            .While => return self.genWhile(@fieldParentPtr(Node.While, "base", node), res),
            .Jump => return self.genJump(@fieldParentPtr(Node.Jump, "base", node), res),
            .List => return self.genTupleList(@fieldParentPtr(Node.ListTupleMap, "base", node), res),
            .Catch => return self.genCatch(@fieldParentPtr(Node.Catch, "base", node), res),
            .Import => return self.genImport(@fieldParentPtr(Node.Import, "base", node), res),
            .Native => return self.genNative(@fieldParentPtr(Node.Native, "base", node), res),

            .Map => return self.reportErr("TODO: Map", node.firstToken()),
            .For => return self.reportErr("TODO: For", node.firstToken()),
            .Match => return self.reportErr("TODO: Match", node.firstToken()),
            .MapItem => return self.reportErr("TODO: MapItem", node.firstToken()),
            .MatchCatchAll => return self.reportErr("TODO: MatchCatchAll", node.firstToken()),
            .MatchLet => return self.reportErr("TODO: MatchLet", node.firstToken()),
            .MatchCase => return self.reportErr("TODO: MatchCase", node.firstToken()),
        }
    }

    fn genNodeNonEmpty(self: *Compiler, node: *Node, res: Result) Error!Value {
        const val = try self.genNode(node, res);

        if (val == .Empty) {
            return self.reportErr("expected a value", node.firstToken());
        }
        return val;
    }

    fn genTupleList(self: *Compiler, node: *Node.ListTupleMap, res: Result) Error!Value {
        if (res == .Lval) {
            switch (res.Lval) {
                .Const, .Let, .Assign => |val| {
                    if (!val.isRt()) {
                        return self.reportErr("expected a tuple", node.base.firstToken());
                    }
                    const reg = val.getRt();
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
                        try self.emitInstruction(.Get, .{ sub_reg, reg, index_reg });
                        const rt_val = Value{ .Rt = sub_reg };
                        const l_val = try self.genNode(n.*, switch (res.Lval) {
                            .Const => Result{ .Lval = .{ .Const = &rt_val } },
                            .Let => Result{ .Lval = .{ .Let = &rt_val } },
                            .Assign => Result{ .Lval = .{ .Assign = &rt_val } },
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
        const sub_res = res.toRt(self);
        const start = self.used_regs;
        self.used_regs += @intCast(RegRef, node.values.len);

        var it = node.values.iterator(0);
        var i = start;
        while (it.next()) |n| {
            _ = try self.genNode(n.*, Result{ .Rt = i });
            i += 1;
        }

        const command = switch (node.base.id) {
            .Tuple => .BuildTuple,
            .List => bog.Op.BuildList,
            else => unreachable,
        };
        try self.emitInstruction(command, .{ sub_res.Rt, start, @intCast(u16, node.values.len) });
        return sub_res.toVal();
    }

    fn genFn(self: *Compiler, node: *Node.Fn, res: Result) Error!Value {
        try res.notLval(self, node.fn_tok);

        if (node.params.len > std.math.maxInt(u8)) {
            return self.reportErr("too many parameters", node.fn_tok);
        }

        const old_used_regs = self.used_regs;

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
        self.used_regs = @truncate(RegRef, node.params.len);
        var it = node.params.iterator(0);
        var i: RegRef = 0;
        while (it.next()) |n| {
            const param_res = try self.genNode(n.*, Result{
                .Lval = .{
                    .Let = &Value{ .Rt = i },
                },
            });
            std.debug.assert(param_res == .Empty);
            i += 1;
        }

        // gen body and return result
        try self.addLineInfo(node.body);
        const body_val = try self.genNode(node.body, .Value);
        // TODO if body_val == .Empty because last instruction was a return
        // then this return is not necessary
        if (body_val == .Empty or body_val == .None) {
            try self.code.append(@enumToInt(bog.Op.ReturnNone));
        } else {
            const reg = try body_val.toRt(self);
            defer body_val.free(self, reg);

            try self.emitInstruction(.Return, .{reg});
        }

        // reset regs after generating body
        self.used_regs = old_used_regs;
        const sub_res = res.toRt(self);

        self.code = old_code;
        try self.emitInstruction(.BuildFn, .{
            sub_res.Rt,
            @truncate(u8, node.params.len),
            @truncate(u32, self.module_code.len),
        });
        try self.module_code.appendSlice(fn_scope.code.toSlice());
        return sub_res.toVal();
    }

    fn genBlock(self: *Compiler, node: *Node.Block, res: Result) Error!Value {
        try res.notLval(self, node.stmts.at(0).*.firstToken());
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
            if (val.isRt()) {
                const reg = val.getRt();
                defer val.free(self, reg);

                // discard unused runtime value
                try self.emitInstruction(.Discard, .{reg});
            }
        }
        return Value{ .Empty = {} };
    }

    fn genIf(self: *Compiler, node: *Node.If, res: Result) Error!Value {
        try res.notLval(self, node.if_tok);

        if (node.capture) |some| return self.reportErr("TODO if let", some.firstToken());

        const cond_val = try self.genNodeNonEmpty(node.cond, .Value);
        if (!cond_val.isRt()) {
            const bool_val = try cond_val.getBool(self, node.cond.firstToken());

            if (bool_val) {
                return self.genNode(node.if_body, res);
            } else if (node.else_body) |some| {
                return self.genNode(some, res);
            }

            const res_val = Value{ .None = {} };
            return res_val.maybeRt(self, res);
        }
        const sub_res = switch (res) {
            .Rt, .Discard => res,
            else => Result{
                .Rt = self.registerAlloc(),
            },
        };

        // jump past if_body if cond == false
        try self.emitInstruction(.JumpFalse, .{ cond_val.getRt(), @as(u32, 0) });
        const addr = self.code.len;
        const if_val = try self.genNode(node.if_body, sub_res);
        if (sub_res != .Rt and if_val.isRt()) {
            try self.emitInstruction(.Discard, .{if_val.getRt()});
        }

        // jump past else_body since if_body was executed
        // TODO this jump is unnecessary if res != .Rt and else_body == null
        try self.emitInstruction(.Jump, .{@as(u32, 0)});
        const addr2 = self.code.len;

        @ptrCast(*align(1) u32, self.code.toSlice()[addr - @sizeOf(u32) ..].ptr).* =
            @truncate(u32, self.code.len - addr);
        if (node.else_body) |some| {
            const else_val = try self.genNode(some, sub_res);
            if (sub_res != .Rt and else_val.isRt()) {
                try self.emitInstruction(.Discard, .{else_val.getRt()});
            }
        } else if (sub_res == .Rt) {
            try self.emitInstruction(.ConstPrimitive, .{ sub_res.Rt, @as(u8, 0) });
        }

        @ptrCast(*align(1) u32, self.code.toSlice()[addr2 - @sizeOf(u32) ..].ptr).* =
            @truncate(u32, self.code.len - addr2);

        return if (sub_res == .Rt)
            Value{ .Rt = sub_res.Rt }
        else
            Value{ .Empty = {} };
    }

    fn genJump(self: *Compiler, node: *Node.Jump, res: Result) Error!Value {
        if (res != .Discard) {
            return self.reportErr("jump expression produces no value", node.tok);
        }
        if (node.op == .Return) {
            if (node.op.Return) |some| {
                const reg = self.registerAlloc();
                defer self.registerFree(reg);
                _ = try self.genNode(some, Result{ .Rt = reg });
                try self.emitInstruction(.Return, .{reg});
            } else {
                try self.code.append(@enumToInt(bog.Op.ReturnNone));
            }
            return Value{ .Empty = {} };
        }

        // find inner most loop
        const loop_scope = blk: {
            var scope = self.cur_scope;
            while (true) switch (scope.id) {
                .Fn => return self.reportErr(if (node.op == .Continue)
                    "continue outside of loop"
                else
                    "break outside of loop", node.tok),
                .Loop => break,
                else => scope = scope.parent.?,
            };
            break :blk @fieldParentPtr(Scope.Loop, "base", scope);
        };
        if (node.op == .Continue) {
            try self.emitInstruction(.Jump, .{
                @truncate(i32, -@intCast(isize, self.code.len - loop_scope.cond_begin)),
            });
        } else {
            try self.emitInstruction(.Jump, .{@as(u32, 0)});
            try loop_scope.breaks.push(@intCast(u32, self.code.len));
        }

        return Value{ .Empty = {} };
    }

    fn genWhile(self: *Compiler, node: *Node.While, res: Result) Error!Value {
        try res.notLval(self, node.while_tok);

        var loop_scope = Scope.Loop{
            .base = .{
                .id = .Loop,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.arena),
            },
            .breaks = Scope.Loop.BreakList.init(self.arena),
            .cond_begin = @intCast(u32, self.code.len),
        };
        self.cur_scope = &loop_scope.base;
        defer self.cur_scope = loop_scope.base.parent.?;

        if (node.capture) |some| return self.reportErr("TODO while let", some.firstToken());

        // beginning of condition
        var cond_jump: ?usize = null;

        const cond_val = try self.genNode(node.cond, .Value);
        if (cond_val.isRt()) {
            try self.emitInstruction(.JumpFalse, .{ cond_val.getRt(), @as(u32, 0) });
            cond_jump = self.code.len;
        } else {
            const bool_val = try cond_val.getBool(self, node.cond.firstToken());
            if (bool_val == false) {
                // never executed
                const res_val = Value{ .None = {} };
                return res_val.maybeRt(self, res);
            }
        }

        const sub_res = switch (res) {
            .Discard => res,
            else => return self.reportErr("TODO while expr", node.while_tok),
        };

        const body_val = try self.genNode(node.body, sub_res);
        if (sub_res != .Rt and body_val.isRt()) {
            try self.emitInstruction(.Discard, .{body_val.getRt()});
        }

        // jump back to condition
        try self.emitInstruction(.Jump, .{
            @truncate(i32, -@intCast(isize, self.code.len + @sizeOf(bog.Op) + @sizeOf(u32) - loop_scope.cond_begin)),
        });

        // exit loop if cond == false
        if (cond_jump) |some| {
            @ptrCast(*align(1) u32, self.code.toSlice()[some - @sizeOf(u32) ..].ptr).* =
                @truncate(u32, self.code.len - some);
        }
        while (loop_scope.breaks.pop()) |some| {
            @ptrCast(*align(1) u32, self.code.toSlice()[some - @sizeOf(u32) ..].ptr).* =
                @truncate(u32, self.code.len - some);
        }

        return if (sub_res == .Rt)
            Value{ .Rt = sub_res.Rt }
        else
            Value{ .Empty = {} };
    }

    fn genCatch(self: *Compiler, node: *Node.Catch, res: Result) Error!Value {
        try res.notLval(self, node.tok);

        var sub_res = switch (res) {
            .Rt => res,
            .Discard => .Value,
            .Value => Result{ .Rt = self.registerAlloc() },
            .Lval => unreachable,
        };
        const l_val = try self.genNodeNonEmpty(node.lhs, sub_res);
        if (!l_val.isRt()) {
            return l_val;
        }
        sub_res = .{
            .Rt = try l_val.toRt(self),
        };

        try self.emitInstruction(.JumpNotError, .{ sub_res.Rt, @as(u32, 0) });
        const addr = self.code.len;

        if (node.capture) |some| {
            return self.reportErr("TODO: capture value", some.firstToken());
        }

        const r_val = try self.genNode(node.rhs, sub_res);

        @ptrCast(*align(1) u32, self.code.toSlice()[addr - @sizeOf(u32) ..].ptr).* =
            @truncate(u32, self.code.len - addr);
        return sub_res.toVal();
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const r_val = try self.genNodeNonEmpty(node.rhs, .Value);

        if (r_val.isRt()) {
            const op_id = switch (node.op) {
                .BoolNot => .BoolNot,
                .BitNot => .BitNot,
                .Minus => .Negate,
                // TODO should unary + be a no-op
                .Plus => return r_val,
                .Try => bog.Op.Try,
            };
            const reg = r_val.getRt();
            defer r_val.free(self, reg);

            const sub_res = res.toRt(self);
            try self.emitInstruction(op_id, .{ sub_res.Rt, reg });
            return sub_res.toVal();
        }
        const ret_val: Value = switch (node.op) {
            .BoolNot => .{ .Bool = !try r_val.getBool(self, node.rhs.firstToken()) },
            .BitNot => .{ .Int = ~try r_val.getInt(self, node.rhs.firstToken()) },
            .Minus => blk: {
                try r_val.checkNum(self, node.rhs.firstToken());
                if (r_val == .Int) {
                    // TODO check for overflow
                    break :blk Value{ .Int = -r_val.Int };
                } else {
                    break :blk Value{ .Num = -r_val.Num };
                }
            },
            .Plus => blk: {
                try r_val.checkNum(self, node.rhs.firstToken());
                break :blk r_val;
            },
            // errors are runtime only currently, so ret_val does not need to be checked
            // TODO should this be an error?
            .Try => r_val,
        };
        return ret_val.maybeRt(self, res);
    }

    fn genTypeInfix(self: *Compiler, node: *Node.TypeInfix, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const l_val = try self.genNodeNonEmpty(node.lhs, .Value);

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
            bog.Value.TypeId.Fn
        else
            return self.reportErr("expected a type name", node.type_tok);

        if (l_val.isRt()) {
            const sub_res = res.toRt(self);
            const reg = l_val.getRt();
            defer l_val.free(self, reg);

            const op: bog.Op = if (node.op == .As) .As else .Is;
            try self.emitInstruction(op, .{ sub_res.Rt, reg, type_id });
            return sub_res.toVal();
        }

        const ret_val = switch (node.op) {
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
                        .Int => |val| try std.fmt.allocPrint(self.arena, "{}", .{val}),
                        .Num => |val| try std.fmt.allocPrint(self.arena, "{d}", .{val}),
                        .Bool => |val| try mem.dupe(self.arena, u8, if (val) "true" else "false"),
                        .Str => |val| val,
                        else => return self.reportErr("invalid cast to string", node.lhs.firstToken()),
                    },
                },
                .Fn => return self.reportErr("cannot cast to function", node.type_tok),
                .Error => return self.reportErr("cannot cast to error", node.type_tok),
                .Range => return self.reportErr("cannot cast to range", node.type_tok),
                .Tuple, .Map, .List => return self.reportErr("invalid cast", node.type_tok),
                .Native => unreachable,
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

        return ret_val.maybeRt(self, res);
    }

    fn genSuffix(self: *Compiler, node: *Node.Suffix, res: Result) Error!Value {
        if (node.op == .Call) {
            try res.notLval(self, node.r_tok);
        }
        const l_val = try self.genNode(node.lhs, .Value);
        if (!l_val.isRt()) {
            return self.reportErr("Invalid left hand side to suffix op", node.lhs.firstToken());
        }
        switch (node.op) {
            .Call => |*args| {
                const sub_res = res.toRt(self);
                const start = self.used_regs;
                self.used_regs += @intCast(RegRef, args.len);

                var it = args.iterator(0);
                var i = start;
                while (it.next()) |n| {
                    _ = try self.genNode(n.*, Result{ .Rt = i });
                    i += 1;
                }

                try self.emitInstruction(.Call, .{ sub_res.Rt, l_val.getRt(), start, @truncate(u16, args.len) });
                return sub_res.toVal();
            },
            .Member => return self.reportErr("TODO: member access", node.l_tok),
            .Subscript => |val| {
                const index_val = try self.genNodeNonEmpty(val, .Value);
                const index_reg = try index_val.toRt(self);
                defer index_val.free(self, index_reg);

                const res_reg = switch (res) {
                    .Rt => |r| r,
                    .Lval => |l| switch (l) {
                        .Let, .Const => return self.reportErr("cannot declare to subscript", node.l_tok),
                        .AugAssign => self.registerAlloc(),
                        .Assign => |r_val| {
                            const r_reg = try r_val.toRt(self);
                            defer r_val.free(self, r_reg);
                            try self.emitInstruction(.Set, .{ l_val.getRt(), index_reg, r_reg });
                            return Value.Empty;
                        },
                    },
                    .Discard, .Value => self.registerAlloc(),
                };

                try self.emitInstruction(.Get, .{ res_reg, l_val.getRt(), index_reg });
                return Value{ .Rt = res_reg };
            },
        }
    }

    fn genInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        switch (node.op) {
            .BoolOr,
            .BoolAnd,
            => return self.genBoolInfix(node, res),

            .LessThan,
            .LessThanEqual,
            .GreaterThan,
            .GreaterThanEqual,
            .Equal,
            .NotEqual,
            .In,
            => return self.genComparisionInfix(node, res),

            .Range => return self.reportErr("TODO ranges", node.tok),

            .BitAnd,
            .BitOr,
            .BitXor,
            .LShift,
            .RShift,
            => return self.genIntInfix(node, res),

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
    }

    fn genAssignInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        if (res == .Rt) {
            return self.reportErr("assignment produces no value", node.tok);
        }
        const r_val = try self.genNodeNonEmpty(node.rhs, .Value);

        if (node.op == .Assign) {
            const l_val = try self.genNode(node.lhs, Result{ .Lval = .{ .Assign = &r_val } });
            std.debug.assert(l_val == .Empty);
            return l_val;
        }

        const l_val = try self.genNode(node.lhs, Result{ .Lval = .AugAssign });
        if (!r_val.isRt()) switch (node.op) {
            .AddAssign,
            .SubAssign,
            .MulAssign,
            .PowAssign,
            .DivAssign,
            .DivFloorAssign,
            .ModAssign,
            => try r_val.checkNum(self, node.rhs.firstToken()),

            .LShiftAssign,
            .RShfitAssign,
            .BitAndAssign,
            .BitOrAssign,
            .BitXOrAssign,
            => _ = try r_val.getInt(self, node.rhs.firstToken()),
            else => unreachable,
        };

        const op_id = switch (node.op) {
            .AddAssign => bog.Op.Add,
            .SubAssign => .Sub,
            .MulAssign => .Mul,
            .PowAssign => .Pow,
            .DivAssign => .Div,
            .DivFloorAssign => .DivFloor,
            .ModAssign => .Mod,
            .LShiftAssign => .LShift,
            .RShfitAssign => .RShift,
            .BitAndAssign => .BitAnd,
            .BitOrAssign => .BitOr,
            .BitXOrAssign => .BitXor,
            else => unreachable,
        };

        const reg = try r_val.toRt(self);
        defer r_val.free(self, reg);

        try self.emitInstruction(op_id, .{ l_val.getRt(), l_val.getRt(), reg });
        return Value.Empty;
    }

    fn needNum(a: Value, b: Value) bool {
        return a == .Num or b == .Num;
    }

    fn genNumericInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .Value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .Value);

        if (r_val.isRt() or l_val.isRt()) {
            const sub_res = res.toRt(self);

            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id = switch (node.op) {
                .Add => .Add,
                .Sub => .Sub,
                .Mul => .Mul,
                .Div => .Div,
                .DivFloor => .DivFloor,
                .Mod => .Mod,
                .Pow => bog.Op.Pow,
                else => unreachable,
            };

            try self.emitInstruction(op_id, .{ sub_res.Rt, l_reg, r_reg });
            return sub_res.toVal();
        }
        try l_val.checkNum(self, node.lhs.firstToken());
        try r_val.checkNum(self, node.rhs.firstToken());

        // TODO makeRuntime if overflow
        const ret_val = switch (node.op) {
            .Add => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .Num = l_val.getNum() + r_val.getNum() };
                }
                break :blk Value{ .Int = l_val.Int + r_val.Int };
            },
            .Sub => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .Num = l_val.getNum() - r_val.getNum() };
                }
                break :blk Value{ .Int = l_val.Int - r_val.Int };
            },
            .Mul => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .Num = l_val.getNum() * r_val.getNum() };
                }
                break :blk Value{ .Int = l_val.Int * r_val.Int };
            },
            .Div => Value{ .Num = l_val.getNum() / r_val.getNum() },
            .DivFloor => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .Int = @floatToInt(i64, @divFloor(l_val.getNum(), r_val.getNum())) };
                }
                break :blk Value{ .Int = @divFloor(l_val.Int, r_val.Int) };
            },
            .Mod => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .Num = @rem(l_val.getNum(), r_val.getNum()) };
                }
                break :blk Value{ .Int = std.math.rem(i64, l_val.Int, r_val.Int) catch @panic("TODO") };
            },
            .Pow => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .Num = std.math.pow(f64, l_val.getNum(), r_val.getNum()) };
                }
                break :blk Value{
                    .Int = std.math.powi(i64, l_val.Int, r_val.Int) catch
                        return self.reportErr("TODO integer overflow", node.tok),
                };
            },
            else => unreachable,
        };

        return ret_val.maybeRt(self, res);
    }

    fn genComparisionInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .Value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .Value);

        if (r_val.isRt() or l_val.isRt()) {
            const sub_res = res.toRt(self);

            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id = switch (node.op) {
                .LessThan => .LessThan,
                .LessThanEqual => .LessThanEqual,
                .GreaterThan => .GreaterThan,
                .GreaterThanEqual => .GreaterThanEqual,
                .Equal => .Equal,
                .NotEqual => .NotEqual,
                .In => bog.Op.In,
                else => unreachable,
            };
            try self.emitInstruction(op_id, .{ sub_res.Rt, l_reg, r_reg });
            return sub_res.toVal();
        }

        // order comparisions are only allowed on numbers
        switch (node.op) {
            .In, .Equal, .NotEqual => {},
            else => {
                try l_val.checkNum(self, node.lhs.firstToken());
                try r_val.checkNum(self, node.rhs.firstToken());
            },
        }

        const ret_val: Value = switch (node.op) {
            .LessThan => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() < r_val.getNum()
                else
                    l_val.Int < r_val.Int,
            },
            .LessThanEqual => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() <= r_val.getNum()
                else
                    l_val.Int <= r_val.Int,
            },
            .GreaterThan => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() > r_val.getNum()
                else
                    l_val.Int > r_val.Int,
            },
            .GreaterThanEqual => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() >= r_val.getNum()
                else
                    l_val.Int >= r_val.Int,
            },
            .Equal, .NotEqual => blk: {
                const eql = switch (l_val) {
                    .None => |a_val| switch (r_val) {
                        .None => true,
                        else => false,
                    },
                    .Int => |a_val| switch (r_val) {
                        .Int => |b_val| a_val == b_val,
                        .Num => |b_val| @intToFloat(f64, a_val) == b_val,
                        else => false,
                    },
                    .Num => |a_val| switch (r_val) {
                        .Int => |b_val| a_val == @intToFloat(f64, b_val),
                        .Num => |b_val| a_val == b_val,
                        else => false,
                    },
                    .Bool => |a_val| switch (r_val) {
                        .Bool => |b_val| a_val == b_val,
                        else => false,
                    },
                    .Str => |a_val| switch (r_val) {
                        .Str => |b_val| mem.eql(u8, a_val, b_val),
                        else => false,
                    },
                    .Empty, .Rt, .Ref => unreachable,
                };
                // broken LLVM module found: Terminator found in the middle of a basic block!
                // break :blk Value{ .Bool = if (node.op == .Equal) eql else !eql };
                const copy = if (node.op == .Equal) eql else !eql;
                break :blk Value{ .Bool = copy };
            },
            .In => .{
                .Bool = mem.indexOf(
                    u8,
                    try l_val.getStr(self, node.lhs.firstToken()),
                    try r_val.getStr(self, node.rhs.firstToken()),
                ) != null,
            },
            else => unreachable,
        };
        return ret_val.maybeRt(self, res);
    }

    fn genBoolInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .Value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .Value);

        if (l_val.isRt() or r_val.isRt()) {
            const sub_res = res.toRt(self);

            // TODO short-circuit evaluation
            // const jump_op = if (node.op == .BoolAnd) .JumpFalse else bog.Op.JumpTrue;
            // try self.emitInstruction(jump_op, .{l_val.getRt(), @as(u32, 0)});
            // const addr = self.code.len;
            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id = if (node.op == .BoolAnd) .BoolAnd else bog.Op.BoolOr;
            try self.emitInstruction(op_id, .{ sub_res.Rt, l_reg, r_reg });
            return sub_res.toVal();
        }
        const l_bool = try l_val.getBool(self, node.lhs.firstToken());
        const r_bool = try r_val.getBool(self, node.rhs.firstToken());

        const ret_val = Value{
            .Bool = if (node.op == .BoolAnd)
                l_bool and r_bool
            else
                l_bool or r_bool,
        };

        return ret_val.maybeRt(self, res);
    }

    fn genIntInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .Value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .Value);

        if (l_val.isRt() or r_val.isRt()) {
            const sub_res = res.toRt(self);

            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id = switch (node.op) {
                .BitAnd => .BitAnd,
                .BitOr => .BitOr,
                .BitXor => .BitXor,
                .LShift => .LShift,
                .RShift => bog.Op.RShift,
                else => unreachable,
            };
            try self.emitInstruction(op_id, .{ sub_res.Rt, l_reg, r_reg });
            return sub_res.toVal();
        }
        const l_int = try l_val.getInt(self, node.lhs.firstToken());
        const r_int = try r_val.getInt(self, node.rhs.firstToken());

        const ret_val: Value = switch (node.op) {
            .BitAnd => .{ .Int = l_int & r_int },
            .BitOr => .{ .Int = l_int | r_int },
            .BitXor => .{ .Int = l_int ^ r_int },
            .LShift => blk: {
                if (r_int < 0)
                    return self.reportErr("shift by negative amount", node.rhs.firstToken());
                const val = if (r_int > std.math.maxInt(u6)) 0 else l_int << @intCast(u6, r_int);
                break :blk Value{ .Int = val };
            },
            .RShift => blk: {
                if (r_int < 0)
                    return self.reportErr("shift by negative amount", node.rhs.firstToken());
                const val = if (r_int > std.math.maxInt(u6)) 0 else l_int >> @intCast(u6, r_int);
                break :blk Value{ .Int = val };
            },
            else => unreachable,
        };

        return ret_val.maybeRt(self, res);
    }

    fn genDecl(self: *Compiler, node: *Node.Decl, res: Result) Error!Value {
        assert(res != .Lval);
        const r_val = try self.genNodeNonEmpty(node.value, .Value);

        const lval_kind = if (self.tree.tokens.at(node.let_const).id == .Keyword_let)
            Result{ .Lval = .{ .Let = &r_val } }
        else
            Result{ .Lval = .{ .Const = &r_val } };

        assert((try self.genNode(node.capture, lval_kind)) == .Empty);
        return Value.Empty;
    }

    fn genIdentifier(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        const name = self.tokenSlice(node.tok);
        if (res == .Lval) {
            switch (res.Lval) {
                .Let, .Const => |val| {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        return self.reportErr("redeclaration of identifier", node.tok);
                    }
                    var reg = try val.toRt(self);

                    if (val.* == .Ref and res.Lval == .Let) {
                        // copy on assign
                        const copy_reg = self.registerAlloc();
                        try self.emitInstruction(.Copy, .{ copy_reg, reg });
                        reg = copy_reg;
                    }
                    try self.cur_scope.declSymbol(.{
                        .name = name,
                        .mutable = res.Lval == .Let,
                        .reg = reg,
                    });
                    return Value.Empty;
                },
                .Assign => |val| {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        if (!sym.mutable) {
                            return self.reportErr("assignment to constant", node.tok);
                        }
                        if (val.* == .Ref) {
                            try self.emitInstruction(.Copy, .{ sym.reg, val.getRt() });
                        } else if (val.isRt()) {
                            try self.emitInstruction(.Move, .{ sym.reg, val.getRt() });
                        } else {
                            try self.makeRuntime(sym.reg, val.*);
                        }
                        return Value.Empty;
                    }
                },
                .AugAssign => {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        if (!sym.mutable) {
                            return self.reportErr("assignment to constant", node.tok);
                        }
                        return Value{ .Ref = sym.reg };
                    }
                },
            }
        } else if (self.cur_scope.getSymbol(name)) |sym| {
            if (res == .Rt) {
                const op_id = if (sym.mutable) .Move else bog.Op.Copy;
                try self.emitInstruction(op_id, .{ res.Rt, sym.reg });
                return res.toVal();
            }
            return Value{ .Ref = sym.reg };
        }
        return self.reportErr("use of undeclared identifier", node.tok);
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const ret_val: Value = switch (node.kind) {
            .Int => .{ .Int = try self.parseInt(node.tok) },
            .True => .{ .Bool = true },
            .False => .{ .Bool = false },
            .None => .None,
            .Str => .{ .Str = try self.parseStr(node.tok) },
            .Num => .{ .Num = self.parseNum(node.tok) },
        };
        return ret_val.maybeRt(self, res);
    }

    fn genImport(self: *Compiler, node: *Node.Import, res: Result) Error!Value {
        try res.notLval(self, node.tok);

        const sub_res = res.toRt(self);
        const str = try self.parseStr(node.str_tok);
        const str_loc = try self.putString(str);

        try self.emitInstruction(.Import, .{ sub_res.Rt, str_loc });
        return sub_res.toVal();
    }

    fn genNative(self: *Compiler, node: *Node.Native, res: Result) Error!Value {
        try res.notLval(self, node.tok);

        const sub_res = res.toRt(self);
        const name = try self.parseStr(node.name_tok);
        const name_loc = try self.putString(name);
        if (node.lib_tok) |some| {
            const lib = try self.parseStr(some);
            const lib_loc = try self.putString(lib);

            try self.emitInstruction(.NativeExtern, .{ sub_res.Rt, lib_loc, name_loc });
        } else {
            try self.emitInstruction(.Native, .{ sub_res.Rt, name_loc });
        }

        return sub_res.toVal();
    }

    fn genError(self: *Compiler, node: *Node.Error, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const val = try self.genNodeNonEmpty(node.value, .Value);

        const sub_res = res.toRt(self);
        const reg = try val.toRt(self);
        defer val.free(self, reg);

        try self.emitInstruction(.BuildError, .{ sub_res.Rt, reg });
        return sub_res.toVal();
    }

    fn addLineInfo(self: *Compiler, node: *Node) !void {
        const token = node.firstToken();
        const tok = self.tree.tokens.at(token);

        try self.emitInstruction(.LineInfo, .{tok.start});
    }

    fn tokenSlice(self: *Compiler, token: TokenIndex) []const u8 {
        const tok = self.tree.tokens.at(token);
        return self.tree.source[tok.start..tok.end];
    }

    fn parseStr(self: *Compiler, tok: TokenIndex) ![]u8 {
        var slice = self.tokenSlice(tok);
        slice = slice[1 .. slice.len - 1];

        var buf = try self.arena.alloc(u8, slice.len);
        var slice_i: u32 = 0;
        var i: u32 = 0;
        while (slice_i < slice.len) : (slice_i += 1) {
            const c = slice[slice_i];
            switch (c) {
                '\\' => {
                    slice_i += 1;
                    buf[i] = switch (slice[slice_i]) {
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\'' => '\'',
                        '"' => '"',
                        'x', 'u' => return self.reportErr("TODO: more escape sequences", tok),
                        else => unreachable,
                    };
                },
                else => buf[i] = c,
            }
            i += 1;
        }
        return buf[0..i];
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

            x = std.math.mul(i64, x, radix) catch
                return self.reportErr("TODO: bigint", tok);
            // why is this cast needed?
            x += @intCast(i32, digit);
        }

        return x;
    }

    fn parseNum(self: *Compiler, tok: TokenIndex) f64 {
        var buf: [256]u8 = undefined;
        const slice = self.tokenSlice(tok);

        var i: u32 = 0;
        for (slice) |c| {
            if (c != '_') {
                buf[i] = c;
                i += 1;
            }
        }

        return std.fmt.parseFloat(f64, buf[0..i]) catch unreachable;
    }

    fn reportErr(self: *Compiler, msg: []const u8, tok: TokenIndex) Error {
        try self.errors.add(msg, self.tree.tokens.at(tok).start, .Error);
        return error.CompileError;
    }
};
