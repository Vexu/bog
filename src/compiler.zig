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
    module_scope: Scope,
    used_regs: RegRef = 0,
    code: Code,

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

    fn emitInstruction_2(self: *Compiler, op: lang.Op, A: RegRef, B: RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{A})[0..]));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{B})[0..]));
    }

    const Scope = struct {
        id: Id,
        parent: ?*Scope,
        syms: Symbol.List,

        const Id = enum {
            Module,
            Fn,
            Loop,
            Block,
            Capture,
        };

        const Fn = struct {
            base: Scope,
            // code: ArrayList(Code),
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
            Assign,
        },

        /// A value, runtime or constant, is expected
        Value,
    };

    pub fn compile(tree: *Tree, allocator: *Allocator) (Error || lang.Parser.Error)!lang.Module {
        const arena = &tree.arena_allocator.allocator;
        var compiler = Compiler{
            .tree = tree,
            .arena = arena,
            .module_scope = .{
                .id = .Module,
                .parent = null,
                .syms = Symbol.List.init(arena),
            },
            .code = Code.init(allocator),
        };
        var it = tree.nodes.iterator(0);
        while (it.next()) |n| {
            const val = try compiler.genNode(n.*, .Value);
            if (val == .Rt) {
                // discard unused runtime value
                try compiler.emitInstruction_1(.Discard, val.Rt);
                compiler.registerFree(val.Rt);
            } else if (it.peek() == null and val != .Empty) {
                const reg = compiler.registerAlloc();
                defer compiler.registerFree(reg);
                try compiler.makeRuntime(reg, val);
                try compiler.emitInstruction_1(.Discard, reg);
            }
        }
        return lang.Module{
            .name = "",
            .code = compiler.code.toOwnedSlice(),
            .strings = "",
            .start_index = 0,
        };
    }

    pub fn compileRepl(compiler: *Compiler, node: *Node, module: *lang.Module) Error!void {
        const val = try compiler.genNode(node, .Value);
        if (val == .Rt) {
            try compiler.emitInstruction_1(.Discard, val.Rt);
        } else if (val != .Empty) {
            const reg = compiler.registerAlloc();
            defer compiler.registerFree(reg);
            try compiler.makeRuntime(reg, val);
            try compiler.emitInstruction_1(.Discard, reg);
        }
        module.code = compiler.code.toSliceConst();
    }

    fn genNode(self: *Compiler, node: *Node, res: Result) Error!Value {
        switch (node.id) {
            .Grouped => return self.genNode(@fieldParentPtr(Node.Grouped, "base", node).expr, res),
            .Literal => return self.genLiteral(@fieldParentPtr(Node.Literal, "base", node), res),
            .Block => return self.genBlock(@fieldParentPtr(Node.ListTupleMapBlock, "base", node), res),
            .Prefix => return self.genPrefix(@fieldParentPtr(Node.Prefix, "base", node), res),
            .Decl => return self.genDecl(@fieldParentPtr(Node.Decl, "base", node), res),
            .Identifier => return self.genIdentifier(@fieldParentPtr(Node.SingleToken, "base", node), res),
            .Fn => @panic("TODO: Fn"),
            .Infix => @panic("TODO: Infix"),
            .TypeInfix => @panic("TODO: TypeInfix"),
            .Suffix => @panic("TODO: Suffix"),
            .Import => @panic("TODO: Import"),
            .Error => @panic("TODO: Error"),
            .List => @panic("TODO: List"),
            .Tuple => @panic("TODO: Tuple"),
            .Map => @panic("TODO: Map"),
            .Catch => @panic("TODO: Catch"),
            .If => @panic("TODO: If"),
            .For => @panic("TODO: For"),
            .While => @panic("TODO: While"),
            .Match => @panic("TODO: Match"),
            .Jump => @panic("TODO: Jump"),
            .MapItem,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            .Discard,
            => unreachable,
        }
    }

    fn genBlock(self: *Compiler, node: *Node.ListTupleMapBlock, res: Result) Error!Value {
        if (res == .Lval) {
            // try adderr("cannot assign to block")
            return error.CompileError;
        }
        var it = node.values.iterator(0);
        while (it.next()) |n| {
            if (it.peek() == null) {
                return self.genNode(n.*, res);
            }
            const val = try self.genNode(n.*, .Value);
            if (val == .Rt) {
                // discard unused runtime value
                try self.emitInstruction_1(.Discard, val.Rt);
                self.registerFree(val.Rt);
            }
        }
        unreachable;
    }

    fn assertNotLval(self: *Compiler, res: Result) !void {
        if (res == .Lval) {
            // try adderr("invalid left hand side to assignment")
            return error.CompileError;
        }
    }

    fn assertNotEmpty(self: *Compiler, val: Value) !void {
        if (val == .Empty) {
            // try adderr("expected value")
            return error.CompileError;
        }
    }

    fn assertBool(self: *Compiler, val: Value) !void {
        if (val != .Bool) {
            // try adderr("expected boolean value")
            return error.CompileError;
        }
    }

    fn assertInt(self: *Compiler, val: Value) !void {
        if (val != .Int) {
            // try adderr("expected integer value")
            return error.CompileError;
        }
    }

    fn assertNumeric(self: *Compiler, val: Value) !void {
        if (val != .Int and val != .Num) {
            // try adderr("expected numeric value")
            return error.CompileError;
        }
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        try self.assertNotLval(res);
        const r_val = try self.genNode(node.rhs, .Value);
        try self.assertNotEmpty(r_val);
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
                try self.assertBool(r_val);
                break :blk Value{ .Bool = !r_val.Bool };
            },
            .BitNot => blk: {
                try self.assertInt(r_val);
                break :blk Value{ .Int = ~r_val.Int };
            },
            .Minus => blk: {
                try self.assertNumeric(r_val);
                if (r_val == .Int) {
                    // TODO check for overflow
                    break :blk Value{ .Int = -r_val.Int };
                } else {
                    break :blk Value{ .Num = -r_val.Num };
                }
            },
            .Plus => blk: {
                try self.assertNumeric(r_val);
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
        // if res == .NoVal or .Value nothing needs to be done
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
        // TODO cur_scope instead of module_scope
        const name = self.tokenSlice(node.tok);
        if (res == .Lval) {
            switch (res.Lval) {
                .Let, .Const => |r| {
                    if (self.module_scope.getSymbol(name)) |sym| {
                        // adderr "redeclaration of identifier 'name'"
                        return error.CompileError;
                    }
                    try self.module_scope.declSymbol(.{
                        .name = name,
                        .mutable = res.Lval == .Let,
                        .reg = r,
                    });
                    return Value.Empty;
                },
                .Assign => @panic("TODO"),
            }
        } else if (self.module_scope.getSymbol(name)) |sym| {
            return Value{ .Rt = sym.reg };
        } else {
            // adderr "use of undeclared identifier 'name'"
            return error.CompileError;
        }
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) Error!Value {
        try self.assertNotLval(res);
        const ret_val: Value = switch (node.kind) {
            .Int => .{ .Int = try self.parseInt(node.tok) },
            .True => .{ .Bool = true },
            .False => .{ .Bool = false },
            .None => .None,
            .Str => @panic("TODO: genStr"),
            .Num => @panic("TODO: genNum"),
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .NoVal or .Value nothing needs to be done
        return ret_val;
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
};
