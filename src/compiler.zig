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
    root_scope: Scope,
    cur_scope: *Scope,
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

    fn emitInstruction_0_1(self: *Compiler, op: lang.Op, arg: var) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]@TypeOf(arg){arg})[0..]));
    }

    fn emitInstruction_2(self: *Compiler, op: lang.Op, A: RegRef, B: RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{ A, B })[0..]));
    }

    fn emitInstruction_3(self: *Compiler, op: lang.Op, A: RegRef, B: RegRef, C: RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{ A, B, C })[0..]));
    }

    fn emitInstruction_1_s(self: *Compiler, op: lang.Op, A: RegRef, args: []RegRef) !void {
        try self.code.append(@enumToInt(op));
        try self.code.appendSlice(@sliceToBytes(([_]RegRef{A})[0..]));
        try self.code.appendSlice(@sliceToBytes(([_]u16{@truncate(u16, args.len)})[0..]));
        try self.code.appendSlice(@sliceToBytes(args));
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
    };

    pub fn compile(tree: *Tree, allocator: *Allocator) (Error || lang.Parser.Error)!lang.Module {
        const arena = &tree.arena_allocator.allocator;
        var compiler = Compiler{
            .tree = tree,
            .arena = arena,
            .root_scope = .{
                .id = .Fn,
                .parent = null,
                .syms = Symbol.List.init(arena),
            },
            .code = Code.init(allocator),
            .cur_scope = undefined,
        };
        compiler.cur_scope = &compiler.root_scope;
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
            .Infix => return self.genInfix(@fieldParentPtr(Node.Infix, "base", node), res),
            .If => return self.genIf(@fieldParentPtr(Node.If, "base", node), res),
            .Tuple => return self.genTuple(@fieldParentPtr(Node.ListTupleMapBlock, "base", node), res),
            .Fn => @panic("TODO: Fn"),
            .TypeInfix => @panic("TODO: TypeInfix"),
            .Suffix => @panic("TODO: Suffix"),
            .Import => @panic("TODO: Import"),
            .Error => @panic("TODO: Error"),
            .List => @panic("TODO: List"),
            .Map => @panic("TODO: Map"),
            .Catch => @panic("TODO: Catch"),
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

    fn genTuple(self: *Compiler, node: *Node.ListTupleMapBlock, res: Result) Error!Value {
        if (res == .Lval) {
            @panic("TODO destructuring assignment");
        }
        const res_loc = if (res == .Rt) res else Result{ .Rt = self.registerAlloc() };
        const args = try self.arena.alloc(RegRef, node.values.len);

        var i: u32 = 0;
        var it = node.values.iterator(0);
        while (it.next()) |n| {
            args[i] = self.registerAlloc();
            _ = try self.genNode(n.*, Result{ .Rt = args[i] });

            i += 1;
        }

        try self.emitInstruction_1_s(.BuildTuple, res_loc.Rt, args);
        return Value{ .Rt = res_loc.Rt };
    }

    fn genBlock(self: *Compiler, node: *Node.ListTupleMapBlock, res: Result) Error!Value {
        try self.assertNotLval(res, node.r_tok);
        var block_scope = Scope{
            .id = .Block,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.arena),
        };
        self.cur_scope = &block_scope;
        defer self.cur_scope = block_scope.parent.?;
        const start_reg_count = self.used_regs;
        defer self.used_regs = start_reg_count;

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

    fn genIf(self: *Compiler, node: *Node.If, res: Result) Error!Value {
        try self.assertNotLval(res, node.if_tok);

        if (node.capture != null) @panic("TODO if let");

        const cond_val = try self.genNode(node.cond, .Value);
        if (cond_val == .Bool) {
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
        } else if (cond_val != .Rt) {
            // TODO node.cond.firstToken()
            return self.reportErr(.ExpectedBoolean, node.if_tok);
        }
        const res_loc = if (res == .Rt) res else Result{
            .Rt = self.registerAlloc(),
        };

        // jump past if_body if cond == false
        try self.emitInstruction_1_1(.JumpFalse, cond_val.Rt, @as(u32, 0));
        const addr = self.code.len - @sizeOf(u32);
        _ = try self.genNode(node.if_body, res_loc);

        // jump past else_body since if_body was executed
        try self.emitInstruction_0_1(.Jump, @as(u32, 0));
        const addr2 = self.code.len - @sizeOf(u32);

        @ptrCast(*align(1) u32, self.code.toSlice()[addr..].ptr).* = @truncate(u32, self.code.len - addr - @sizeOf(u32));
        if (node.else_body) |some| {
            _ = try self.genNode(some, res_loc);
        } else {
            try self.emitInstruction_1_1(.ConstPrimitive, res_loc.Rt, @as(u8, 0));
        }

        @ptrCast(*align(1) u32, self.code.toSlice()[addr2..].ptr).* = @truncate(u32, self.code.len - addr2 - @sizeOf(u32));
        return Value{
            .Rt = res_loc.Rt,
        };
    }

    fn assertNotLval(self: *Compiler, res: Result, tok: TokenIndex) !void {
        if (res == .Lval) {
            return self.reportErr(.InvalidLval, tok);
        }
    }

    fn assertNotEmpty(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val == .Empty) {
            return self.reportErr(.InvalidEmpty, tok);
        }
    }

    fn assertBool(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val != .Bool) {
            return self.reportErr(.ExpectedBoolean, tok);
        }
    }

    fn assertInt(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val != .Int) {
            return self.reportErr(.ExpectedInt, tok);
        }
    }

    fn assertNumeric(self: *Compiler, val: Value, tok: TokenIndex) !void {
        if (val != .Int and val != .Num) {
            return self.reportErr(.ExpectedNumeric, tok);
        }
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);
        const r_val = try self.genNode(node.rhs, .Value);
        try self.assertNotEmpty(r_val, node.tok);
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
                try self.assertBool(r_val, node.tok);
                break :blk Value{ .Bool = !r_val.Bool };
            },
            .BitNot => blk: {
                try self.assertInt(r_val, node.tok);
                break :blk Value{ .Int = ~r_val.Int };
            },
            .Minus => blk: {
                try self.assertNumeric(r_val, node.tok);
                if (r_val == .Int) {
                    // TODO check for overflow
                    break :blk Value{ .Int = -r_val.Int };
                } else {
                    break :blk Value{ .Num = -r_val.Num };
                }
            },
            .Plus => blk: {
                try self.assertNumeric(r_val, node.tok);
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
        @panic("TODO");
    }

    fn genAssignInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        if (res == .Rt) {
            return self.reportErr(.InvalidLval, node.tok);
        }
        const reg = self.registerAlloc();
        defer self.registerFree(reg);
        const r_val = try self.genNode(node.rhs, Result{ .Rt = reg });
        try self.assertNotEmpty(r_val, node.tok);

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
        try self.assertNotEmpty(l_val, node.tok);

        var r_val = try self.genNode(node.rhs, .Value);
        try self.assertNotEmpty(r_val, node.tok);

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
                @panic("TODO");
                // break :blk Value{ .Num = std.math.div(l_val.Int, r_val.Int) };
            },
            .DivFloor => blk: {
                break :blk Value{ .Int = @divFloor(l_val.Int, r_val.Int) };
            },
            .Mod => blk: {
                @panic("TODO");
                // break :blk Value{ .Int =std.math.rem(i64, l_val.Int, r_val.Int) catch @panic("TODO") };
            },
            .Pow => blk: {
                break :blk Value{ .Int = std.math.powi(i64, l_val.Int, r_val.Int) catch @panic("TODO") };
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
                        return self.reportErr(.Redeclaration, node.tok);
                    }
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
                            return self.reportErr(.AssignToConst, node.tok);
                        }
                        // TODO this move can usually be avoided
                        try self.emitInstruction_2(.Move, sym.reg, r);
                        return Value.Empty;
                    }
                },
                .AugAssign => {
                    if (self.cur_scope.getSymbol(name)) |sym| {
                        if (!sym.mutable) {
                            return self.reportErr(.AssignToConst, node.tok);
                        }
                        return Value{ .Rt = sym.reg };
                    }
                },
            }
        } else if (self.cur_scope.getSymbol(name)) |sym| {
            return Value{ .Rt = sym.reg };
        }
        return self.reportErr(.Undeclared, node.tok);
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) Error!Value {
        try self.assertNotLval(res, node.tok);
        const ret_val: Value = switch (node.kind) {
            .Int => .{ .Int = try self.parseInt(node.tok) },
            .True => .{ .Bool = true },
            .False => .{ .Bool = false },
            .None => .None,
            .Str => @panic("TODO: genStr"),
            .Num => .{ .Num = try self.parseNum(node.tok) },
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .Value nothing needs to be done
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

    fn reportErr(self: *Compiler, kind: lang.ErrorMsg.Kind, tok: TokenIndex) Error {
        try self.tree.errors.push(.{
            .kind = kind,
            .index = self.tree.tokens.at(tok).start,
        });
        return error.CompileError;
    }
};
