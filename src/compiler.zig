const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const bog = @import("bog.zig");
const Node = bog.Node;
const Tree = bog.Tree;
const TokenList = bog.Token.List;
const TokenIndex = bog.Token.Index;
const RegRef = bog.RegRef;
const Errors = bog.Errors;
const util = @import("util.zig");

pub const Error = error{CompileError} || Allocator.Error;

pub const Compiler = struct {
    tree: *Tree,
    errors: *Errors,
    arena: *Allocator,
    root_scope: Scope.Module,
    cur_scope: *Scope,
    used_regs: RegRef = 0,
    code: *Code,
    module_code: Code,
    strings: Code,
    string_interner: std.StringHashMap(u32),

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
            module,
            func,
            loop,
            block,
            capture,
        };

        const Fn = struct {
            base: Scope,
            code: Code,
            captures: *Symbol.List,

            fn emitInstruction(scope: *Fn, self: *Compiler, op: bog.Op, args: var) !void {
                try scope.code.append(@enumToInt(op));
                inline for (std.meta.fields(@TypeOf(args))) |f| {
                    try scope.code.appendSlice(mem.asBytes(&@field(args, f.name)));
                }
            }
        };

        const Module = struct {
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

        fn isDeclared(scope: *Scope, name: []const u8) ?*Symbol {
            var cur: ?*Scope = scope;
            while (cur) |some| {
                var it = some.syms.iterator(some.syms.len);
                while (it.prev()) |sym| {
                    if (mem.eql(u8, sym.name, name)) {
                        return sym;
                    }
                }
                cur = some.parent;
            }
            return null;
        }

        fn getSymbol(scope: *Scope, self: *Compiler, name: []const u8, tok: TokenIndex) !RegAndMut {
            return try scope.getSymbolTail(self, name, null, tok);
        }

        fn getSymbolTail(scope: *Scope, self: *Compiler, name: []const u8, func: ?*Fn, tok: TokenIndex) Error!RegAndMut {
            var cur: ?*Scope = scope;
            blk: while (cur) |some| {
                var it = some.syms.iterator(some.syms.len);
                while (it.prev()) |sym| {
                    if (mem.eql(u8, sym.name, name)) {
                        if (func) |parent| {
                            try parent.captures.push(sym.*);
                            return RegAndMut{
                                .reg = @truncate(RegRef, parent.captures.len - 1),
                                .mutable = sym.mutable,
                            };
                        }
                        return RegAndMut{
                            .reg = sym.reg,
                            .mutable = sym.mutable,
                        };
                    }
                }
                if (some.id == .func) {
                    const new_func = @fieldParentPtr(Fn, "base", some);
                    const res = self.registerAlloc();

                    // check if we have already captured this variable
                    it = new_func.captures.iterator(new_func.captures.len);
                    while (it.prev()) |sym| {
                        if (!mem.eql(u8, sym.name, name)) continue;

                        try new_func.emitInstruction(self, .LoadCapture, .{
                            res,
                            @truncate(u8, sym.reg),
                        });
                        return RegAndMut{
                            .reg = res,
                            .mutable = sym.mutable,
                        };
                    }
                    const parent = some.parent orelse break :blk;
                    const sub = try parent.getSymbolTail(self, name, new_func, tok);
                    try new_func.emitInstruction(self, .LoadCapture, .{
                        res,
                        @truncate(u8, sub.reg),
                    });

                    // forward captured symbol
                    if (func) |parent_fn| {
                        try parent_fn.captures.push(.{
                            .name = name,
                            .reg = res,
                            .mutable = sub.mutable,
                        });
                    }
                    return RegAndMut{
                        .reg = res,
                        .mutable = sub.mutable,
                    };
                }
                cur = some.parent;
            }
            return self.reportErr("use of undeclared identifier", tok);
        }

        const RegAndMut = struct {
            reg: RegRef,
            mutable: bool,
        };
    };

    pub const Symbol = struct {
        name: []const u8,
        reg: RegRef,
        mutable: bool,

        forward_decl: bool = false,

        pub const List = std.SegmentedList(Symbol, 4);
    };

    const Value = union(enum) {
        /// result of continue, break, return and assignment; cannot exist at runtime
        empty,
        rt: RegRef,

        /// reference to a variable
        ref: RegRef,

        none,
        int: i64,
        num: f64,
        Bool: bool,
        str: []const u8,
        func: struct {
            params: u8,
            offset: u32,
            captures: *Symbol.List,
        },

        fn isRt(val: Value) bool {
            return switch (val) {
                .rt, .ref => true,
                else => false,
            };
        }

        fn maybeRt(val: Value, self: *Compiler, res: Result) !Value {
            if (res == .rt) {
                try self.makeRuntime(res.rt, val);
                return res.toVal();
            }
            return val;
        }

        fn free(val: Value, self: *Compiler, reg: RegRef) void {
            if (val != .ref) {
                self.registerFree(reg);
            }
        }

        fn toRt(val: Value, self: *Compiler) !RegRef {
            switch (val) {
                .rt, .ref => |r| return r,
                .empty => unreachable,
                else => {
                    const reg = self.registerAlloc();
                    try self.makeRuntime(reg, val);
                    return reg;
                },
            }
        }

        fn getRt(val: Value) RegRef {
            switch (val) {
                .rt, .ref => |r| return r,
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
            if (val != .int) {
                return self.reportErr("expected an integer", tok);
            }
            return val.int;
        }

        fn getNum(val: Value) f64 {
            return switch (val) {
                .int => |v| @intToFloat(f64, v),
                .num => |v| v,
                else => unreachable,
            };
        }

        fn getStr(val: Value, self: *Compiler, tok: TokenIndex) ![]const u8 {
            if (val != .str) {
                return self.reportErr("expected a string", tok);
            }
            return val.str;
        }

        fn checkNum(val: Value, self: *Compiler, tok: TokenIndex) !void {
            if (val != .int and val != .num) {
                return self.reportErr("expected a number", tok);
            }
        }
    };

    fn makeRuntime(self: *Compiler, res: RegRef, val: Value) Error!void {
        return switch (val) {
            .empty => unreachable,
            .ref, .rt => |v| assert(v == res),
            .none => try self.emitInstruction(.ConstPrimitive, .{ res, @as(u8, 0) }),
            .int => |v| if (v > std.math.minInt(i8) and v < std.math.maxInt(i8)) {
                try self.emitInstruction(.ConstInt8, .{ res, @truncate(i8, v) });
            } else if (v > std.math.minInt(i32) and v < std.math.maxInt(i32)) {
                try self.emitInstruction(.ConstInt32, .{ res, @truncate(i32, v) });
            } else {
                try self.emitInstruction(.ConstInt64, .{ res, v });
            },
            .num => |v| try self.emitInstruction(.ConstNum, .{ res, v }),
            .Bool => |v| try self.emitInstruction(.ConstPrimitive, .{ res, @as(u8, @boolToInt(v)) + 1 }),
            .str => |v| try self.emitInstruction(.ConstString, .{ res, try self.putString(v) }),
            .func => |v| {
                try self.emitInstruction(.BuildFn, .{ res, v.params, @truncate(u8, v.captures.len), v.offset });

                var capture_it = v.captures.iterator(0);
                while (capture_it.next()) |capture| {
                    try self.emitInstruction(.StoreCapture, .{
                        res,
                        capture.reg,
                        @truncate(u8, capture_it.index - 1),
                    });
                }
            },
        };
    }

    fn putString(self: *Compiler, str: []const u8) !u32 {
        if (self.string_interner.getValue(str)) |some| return some;
        const offset = @intCast(u32, self.strings.items.len);
        try self.strings.appendSlice(mem.asBytes(&@intCast(u32, str.len)));
        try self.strings.appendSlice(str);

        _ = try self.string_interner.put(str, offset);
        return offset;
    }

    const Result = union(enum) {
        /// A runtime value is expected
        rt: RegRef,

        /// Something assignable is expected
        lval: union(enum) {
            Const: *const Value,
            let: *const Value,
            assign: *const Value,
            aug_assign,
        },

        /// A value, runtime or constant, is expected
        value,

        /// No value is expected if some is given it will be discarded
        discard,

        fn toRt(res: Result, compiler: *Compiler) Result {
            return if (res == .rt) res else .{ .rt = compiler.registerAlloc() };
        }

        /// returns .empty if res != .rt
        fn toVal(res: Result) Value {
            return if (res != .rt)
                .empty
            else
                .{ .rt = res.rt };
        }

        fn notLval(res: Result, self: *Compiler, tok: TokenIndex) !void {
            if (res == .lval) {
                return self.reportErr("invalid left hand side to assignment", tok);
            }
        }
    };

    pub fn compile(allocator: *Allocator, source: []const u8, errors: *Errors) (Error || bog.Parser.Error || bog.Tokenizer.Error)!*bog.Module {
        var tree = try bog.parse(allocator, source, errors);
        const arena = &tree.arena_allocator.allocator;
        var compiler = Compiler{
            .errors = errors,
            .tree = tree,
            .arena = arena,
            .root_scope = .{
                .base = .{
                    .id = .module,
                    .parent = null,
                    .syms = Symbol.List.init(arena),
                },
                .code = Code.init(arena),
            },
            .module_code = Code.init(allocator),
            .strings = Code.init(allocator),
            .code = undefined,
            .cur_scope = undefined,
            .string_interner = std.StringHashMap(u32).init(allocator),
        };
        compiler.code = &compiler.root_scope.code;
        compiler.cur_scope = &compiler.root_scope.base;

        var it = tree.nodes.iterator(0);
        while (it.next()) |n| {
            try compiler.autoForwardDecl(n.*);
        }
        it = tree.nodes.iterator(0);
        while (it.next()) |n| {
            try compiler.addLineInfo(n.*);

            const val = try compiler.genNode(n.*, .discard);
            if (val.isRt()) {
                const reg = val.getRt();
                defer val.free(&compiler, reg);
                // discard unused runtime value
                try compiler.emitInstruction(.Discard, .{reg});
            }
        }

        const entry = compiler.module_code.items.len;
        try compiler.module_code.appendSlice(compiler.code.items);
        const mod = try allocator.create(bog.Module);
        mod.* = .{
            .name = "",
            .code = compiler.module_code.toOwnedSlice(),
            .strings = compiler.strings.toOwnedSlice(),
            .entry = @truncate(u32, entry),
        };
        return mod;
    }

    pub fn compileRepl(self: *Compiler, node: *Node, module: *bog.Module) Error!usize {
        try self.autoForwardDecl(node);

        const start_len = self.module_code.items.len;
        try self.addLineInfo(node);
        const val = try self.genNode(node, .discard);
        if (val != .empty) {
            const reg = try val.toRt(self);
            defer val.free(self, reg);

            try self.emitInstruction(.Discard, .{reg});
        }
        const final_len = self.module_code.items.len;
        try self.module_code.appendSlice(self.code.items);

        module.code = self.module_code.items;
        self.module_code.resize(final_len) catch unreachable;
        module.strings = self.strings.items;
        return final_len - start_len;
    }

    fn autoForwardDecl(self: *Compiler, node: *Node) error{OutOfMemory}!void {
        if (node.id != .Decl) return;
        const decl = @fieldParentPtr(Node.Decl, "base", node);

        // only forward declarations like
        // `const IDENTIFIER = fn ...`
        if (self.tree.tokens.at(decl.let_const).id == .Keyword_const and
            decl.capture.id != .Identifier or decl.value.id != .Fn)
            return;

        const ident = @fieldParentPtr(Node.SingleToken, "base", decl.capture);
        const reg = self.registerAlloc();

        try self.cur_scope.declSymbol(.{
            .name = self.tokenSlice(ident.tok),
            .mutable = false,
            .reg = reg,
            .forward_decl = true,
        });
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
            .For => return self.genFor(@fieldParentPtr(Node.For, "base", node), res),
            .Map => return self.genMap(@fieldParentPtr(Node.ListTupleMap, "base", node), res),
            .MapItem => unreachable,
            .This => return self.genThis(@fieldParentPtr(Node.SingleToken, "base", node), res),

            .Match => return self.reportErr("TODO: Match", node.firstToken()),
            .MatchCatchAll => return self.reportErr("TODO: MatchCatchAll", node.firstToken()),
            .MatchLet => return self.reportErr("TODO: MatchLet", node.firstToken()),
            .MatchCase => return self.reportErr("TODO: MatchCase", node.firstToken()),
        }
    }

    fn genNodeNonEmpty(self: *Compiler, node: *Node, res: Result) Error!Value {
        const val = try self.genNode(node, res);

        if (val == .empty) {
            return self.reportErr("expected a value", node.firstToken());
        }
        return val;
    }

    fn genMap(self: *Compiler, node: *Node.ListTupleMap, res: Result) Error!Value {
        if (res == .lval) {
            switch (res.lval) {
                .Const, .let, .assign => |val| {
                    if (!val.isRt()) {
                        return self.reportErr("expected a map", node.base.firstToken());
                    }
                    const reg = val.getRt();
                    const index_reg = self.registerAlloc();
                    var sub_reg = self.registerAlloc();

                    var it = node.values.iterator(0);
                    while (it.next()) |n| {
                        const item = @fieldParentPtr(Node.MapItem, "base", n.*);

                        if (item.key) |some| {
                            const last_node = self.getLastNode(some, false);
                            if (last_node.id == .Identifier) {
                                // `oldname: newname` is equal to `"oldname": newname`
                                const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                                const str_loc = try self.putString(self.tokenSlice(ident.tok));
                                try self.emitInstruction(.ConstString, .{ index_reg, str_loc });
                            } else {
                                _ = try self.genNode(some, .{ .rt = index_reg });
                            }
                        } else {
                            const last_node = self.getLastNode(item.value, false);
                            if (last_node.id != .Identifier) {
                                return self.reportErr("expected a key", item.value.firstToken());
                            }
                            // `oldname` is equal to `"oldname": oldname`
                            const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                            const str_loc = try self.putString(self.tokenSlice(ident.tok));
                            try self.emitInstruction(.ConstString, .{ index_reg, str_loc });
                        }

                        try self.emitInstruction(.Get, .{ sub_reg, reg, index_reg });
                        const rt_val = Value{ .rt = sub_reg };
                        const l_val = try self.genNode(item.value, switch (res.lval) {
                            .Const => .{ .lval = .{ .Const = &rt_val } },
                            .let => .{ .lval = .{ .let = &rt_val } },
                            .assign => .{ .lval = .{ .assign = &rt_val } },
                            else => unreachable,
                        });
                        std.debug.assert(l_val == .empty);

                        if (it.peek() != null and res.lval != .assign) sub_reg = self.registerAlloc();
                    }
                    return Value.empty;
                },
                .aug_assign => {
                    return self.reportErr("invalid left hand side to augmented assignment", node.r_tok);
                },
            }
        }
        const sub_res = res.toRt(self);
        const start = self.used_regs;
        self.used_regs += @intCast(RegRef, node.values.len * 2);

        var it = node.values.iterator(0);
        var i = start;
        while (it.next()) |n| {
            const item = @fieldParentPtr(Node.MapItem, "base", n.*);

            if (item.key) |some| {
                const last_node = self.getLastNode(some, false);
                if (last_node.id == .Identifier) {
                    // `ident: value` is equal to `"ident": value`
                    const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                    const str_loc = try self.putString(self.tokenSlice(ident.tok));
                    try self.emitInstruction(.ConstString, .{ i, str_loc });
                } else {
                    _ = try self.genNode(some, .{ .rt = i });
                }
            } else {
                const last_node = self.getLastNode(item.value, false);
                if (last_node.id != .Identifier) {
                    return self.reportErr("expected a key", item.value.firstToken());
                }
                // `ident` is equal to `"ident": ident`
                const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                const str_loc = try self.putString(self.tokenSlice(ident.tok));
                try self.emitInstruction(.ConstString, .{ i, str_loc });
            }

            _ = try self.genNode(item.value, .{ .rt = i + 1 });
            i += 2;
        }

        try self.emitInstruction(.BuildMap, .{ sub_res.rt, start, @intCast(u16, node.values.len * 2) });
        return sub_res.toVal();
    }

    fn genTupleList(self: *Compiler, node: *Node.ListTupleMap, res: Result) Error!Value {
        if (res == .lval) {
            switch (res.lval) {
                .Const, .let, .assign => |val| {
                    if (!val.isRt()) {
                        return self.reportErr("expected a tuple/list", node.base.firstToken());
                    }
                    const reg = val.getRt();
                    const index_reg = self.registerAlloc();
                    var sub_reg = self.registerAlloc();
                    var index_val = Value{
                        .int = 0,
                    };

                    var it = node.values.iterator(0);
                    while (it.next()) |n| {
                        if (n.*.id == .Discard) {
                            index_val.int += 1;
                            continue;
                        }
                        try self.makeRuntime(index_reg, index_val);
                        try self.emitInstruction(.Get, .{ sub_reg, reg, index_reg });
                        const rt_val = Value{ .rt = sub_reg };
                        const l_val = try self.genNode(n.*, switch (res.lval) {
                            .Const => .{ .lval = .{ .Const = &rt_val } },
                            .let => .{ .lval = .{ .let = &rt_val } },
                            .assign => .{ .lval = .{ .assign = &rt_val } },
                            else => unreachable,
                        });
                        std.debug.assert(l_val == .empty);
                        index_val.int += 1;

                        if (it.peek() != null and res.lval != .assign) sub_reg = self.registerAlloc();
                    }
                    return Value.empty;
                },
                .aug_assign => {
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
            _ = try self.genNode(n.*, .{ .rt = i });
            i += 1;
        }

        const op_id: bog.Op = switch (node.base.id) {
            .Tuple => .BuildTuple,
            .List => .BuildList,
            else => unreachable,
        };
        try self.emitInstruction(op_id, .{ sub_res.rt, start, @intCast(u16, node.values.len) });
        return sub_res.toVal();
    }

    fn genFn(self: *Compiler, node: *Node.Fn, res: Result) Error!Value {
        try res.notLval(self, node.fn_tok);

        if (node.params.len > std.math.maxInt(u8)) {
            return self.reportErr("too many parameters", node.fn_tok);
        }
        const param_count = @truncate(u8, node.params.len);

        const old_used_regs = self.used_regs;

        const captures = try self.arena.create(Symbol.List);
        captures.* = Symbol.List.init(self.arena);

        var fn_scope = Scope.Fn{
            .base = .{
                .id = .func,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.arena),
            },
            .code = try Code.initCapacity(self.arena, 256),
            .captures = captures,
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
            const param_res = try self.genNode(n.*, .{
                .lval = .{
                    .let = &Value{ .rt = i },
                },
            });
            std.debug.assert(param_res == .empty);
            i += 1;
        }

        // gen body and return result
        try self.addLineInfo(node.body);

        const last_node = self.getLastNode(node.body, true);
        const last_is_ret = last_node.id == .Jump and
            @fieldParentPtr(Node.Jump, "base", last_node).op == .Return;

        const should_discard = switch (last_node.id) {
            // these will generate different code if value is discarded
            .If, .While, .For, .Match, .Block => last_is_ret,
            else => true,
        };

        // if last node is not a return then we expect some value we can return
        const body_val = try self.genNode(node.body, if (should_discard) .discard else .value);

        if (last_is_ret) {
            std.debug.assert(body_val == .empty);
        } else if (body_val == .empty or body_val == .none) {
            try self.code.append(@enumToInt(bog.Op.ReturnNone));
        } else {
            const reg = try body_val.toRt(self);
            defer body_val.free(self, reg);

            try self.emitInstruction(.Return, .{reg});
        }

        // reset regs after generating body
        self.used_regs = old_used_regs;
        self.code = old_code;

        const offset = @truncate(u32, self.module_code.items.len);
        try self.module_code.appendSlice(fn_scope.code.items);

        const ret_val = Value{
            .func = .{
                .params = param_count,
                .offset = offset,
                .captures = captures,
            },
        };
        return ret_val.maybeRt(self, res);
    }

    fn genBlock(self: *Compiler, node: *Node.Block, res: Result) Error!Value {
        try res.notLval(self, node.stmts.at(0).*.firstToken());
        var block_scope = Scope{
            .id = .block,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.arena),
        };
        self.cur_scope = &block_scope;
        defer self.cur_scope = block_scope.parent.?;

        var it = node.stmts.iterator(0);
        while (it.next()) |n| {
            try self.addLineInfo(n.*);

            // return value of last instruction if it is not discarded
            if (it.peek() == null and res != .discard) {
                return self.genNode(n.*, res);
            }

            const val = try self.genNode(n.*, .discard);
            if (val.isRt()) {
                const reg = val.getRt();
                defer val.free(self, reg);

                // discard unused runtime value
                try self.emitInstruction(.Discard, .{reg});
            }
        }
        return Value{ .empty = {} };
    }

    fn genIf(self: *Compiler, node: *Node.If, res: Result) Error!Value {
        try res.notLval(self, node.if_tok);

        var capture_scope = Scope{
            .id = .capture,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.arena),
        };
        var if_skip: usize = undefined;

        const cond_val = try self.genNodeNonEmpty(node.cond, .value);
        if (node.capture != null) {
            // TODO handle cond_val.isRt()
            const cond_reg = try cond_val.toRt(self);
            // jump past if_body if cond == .none
            try self.emitInstruction(.JumpNone, .{ cond_reg, @as(u32, 0) });
            if_skip = self.code.items.len;

            self.cur_scope = &capture_scope;
            const lval_res = if (self.tree.tokens.at(node.let_const.?).id == .Keyword_let)
                Result{ .lval = .{ .let = &Value{ .rt = cond_reg } } }
            else
                Result{ .lval = .{ .Const = &Value{ .rt = cond_reg } } };

            assert((try self.genNode(node.capture.?, lval_res)) == .empty);
        } else if (!cond_val.isRt()) {
            const bool_val = try cond_val.getBool(self, node.cond.firstToken());

            if (bool_val) {
                return self.genNode(node.if_body, res);
            } else if (node.else_body) |some| {
                return self.genNode(some, res);
            }

            const res_val = Value{ .none = {} };
            return res_val.maybeRt(self, res);
        } else {
            // jump past if_body if cond == false
            try self.emitInstruction(.JumpFalse, .{ cond_val.getRt(), @as(u32, 0) });
            if_skip = self.code.items.len;
        }
        const sub_res = switch (res) {
            .rt, .discard => res,
            .value => Result{
                // value is only known at runtime
                .rt = self.registerAlloc(),
            },
            .lval => unreachable,
        };

        const if_val = try self.genNode(node.if_body, sub_res);
        if (sub_res != .rt and if_val.isRt()) {
            try self.emitInstruction(.Discard, .{if_val.getRt()});
        }

        // jump past else_body since if_body was executed
        const else_skip_needed = node.else_body != null or sub_res == .rt;
        if (else_skip_needed) {
            try self.emitInstruction(.Jump, .{@as(u32, 0)});
        }
        const else_skip = self.code.items.len;

        self.finishJump(if_skip);
        // end capture scope
        self.cur_scope = capture_scope.parent.?;

        if (node.else_body) |some| {
            const else_val = try self.genNode(some, sub_res);
            if (sub_res != .rt and else_val.isRt()) {
                try self.emitInstruction(.Discard, .{else_val.getRt()});
            }
        } else if (sub_res == .rt) {
            try self.emitInstruction(.ConstPrimitive, .{ sub_res.rt, @as(u8, 0) });
        }

        if (else_skip_needed) {
            self.finishJump(else_skip);
        }

        return sub_res.toVal();
    }

    fn genJump(self: *Compiler, node: *Node.Jump, res: Result) Error!Value {
        if (res != .discard) {
            return self.reportErr("jump expression produces no value", node.tok);
        }
        if (node.op == .Return) {
            if (node.op.Return) |some| {
                const val = try self.genNode(some, .value);
                const reg = try val.toRt(self);
                defer val.free(self, reg);

                try self.emitInstruction(.Return, .{reg});
            } else {
                try self.code.append(@enumToInt(bog.Op.ReturnNone));
            }
            return Value{ .empty = {} };
        }

        // find inner most loop
        const loop_scope = blk: {
            var scope = self.cur_scope;
            while (true) switch (scope.id) {
                .func => return self.reportErr(if (node.op == .Continue)
                    "continue outside of loop"
                else
                    "break outside of loop", node.tok),
                .loop => break,
                else => scope = scope.parent.?,
            };
            break :blk @fieldParentPtr(Scope.Loop, "base", scope);
        };
        if (node.op == .Continue) {
            try self.emitInstruction(.Jump, .{
                @truncate(i32, -@intCast(isize, self.code.items.len - loop_scope.cond_begin)),
            });
        } else {
            try self.emitInstruction(.Jump, .{@as(u32, 0)});
            try loop_scope.breaks.push(@intCast(u32, self.code.items.len));
        }

        return Value{ .empty = {} };
    }

    fn genWhile(self: *Compiler, node: *Node.While, res: Result) Error!Value {
        try res.notLval(self, node.while_tok);

        var loop_scope = Scope.Loop{
            .base = .{
                .id = .loop,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.arena),
            },
            .breaks = Scope.Loop.BreakList.init(self.arena),
            .cond_begin = @intCast(u32, self.code.items.len),
        };
        self.cur_scope = &loop_scope.base;
        defer self.cur_scope = loop_scope.base.parent.?;

        // beginning of condition
        var cond_jump: ?usize = null;

        const cond_val = try self.genNode(node.cond, .value);
        if (node.capture != null) {
            // TODO handle cond_val.isRt()
            const cond_reg = try cond_val.toRt(self);
            // jump past exit loop if cond == .none
            try self.emitInstruction(.JumpNone, .{ cond_reg, @as(u32, 0) });
            cond_jump = self.code.items.len;

            const lval_res = if (self.tree.tokens.at(node.let_const.?).id == .Keyword_let)
                Result{ .lval = .{ .let = &Value{ .rt = cond_reg } } }
            else
                Result{ .lval = .{ .Const = &Value{ .rt = cond_reg } } };

            assert((try self.genNode(node.capture.?, lval_res)) == .empty);
        } else if (cond_val.isRt()) {
            try self.emitInstruction(.JumpFalse, .{ cond_val.getRt(), @as(u32, 0) });
            cond_jump = self.code.items.len;
        } else {
            const bool_val = try cond_val.getBool(self, node.cond.firstToken());
            if (bool_val == false) {
                // never executed
                const res_val = Value{ .none = {} };
                return res_val.maybeRt(self, res);
            }
        }

        const sub_res = switch (res) {
            .discard => res,
            .lval => unreachable,
            .value, .rt => return self.reportErr("TODO while expr", node.while_tok),
        };

        const body_val = try self.genNode(node.body, sub_res);
        if (sub_res != .rt and body_val.isRt()) {
            try self.emitInstruction(.Discard, .{body_val.getRt()});
        }

        // jump back to condition
        try self.emitInstruction(.Jump, .{
            @truncate(i32, -@intCast(isize, self.code.items.len + @sizeOf(bog.Op) + @sizeOf(u32) - loop_scope.cond_begin)),
        });

        // exit loop if cond == false
        if (cond_jump) |some| {
            self.finishJump(some);
        }
        while (loop_scope.breaks.pop()) |some| {
            self.finishJump(some);
        }

        return sub_res.toVal();
    }

    fn genFor(self: *Compiler, node: *Node.For, res: Result) Error!Value {
        try res.notLval(self, node.for_tok);

        var loop_scope = Scope.Loop{
            .base = .{
                .id = .loop,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.arena),
            },
            .breaks = Scope.Loop.BreakList.init(self.arena),
            .cond_begin = undefined,
        };
        self.cur_scope = &loop_scope.base;
        defer self.cur_scope = loop_scope.base.parent.?;

        const cond_val = try self.genNode(node.cond, .value);
        if (!cond_val.isRt() and cond_val != .str)
            return self.reportErr("expected iterable value", node.cond.firstToken());

        const cond_reg = try cond_val.toRt(self);
        defer cond_val.free(self, cond_reg);

        const iter_reg = self.registerAlloc();
        defer self.registerFree(iter_reg);

        // initialize the iterator
        try self.emitInstruction(.IterInit, .{ iter_reg, cond_reg });

        const iter_val_reg = self.registerAlloc();
        defer self.registerFree(iter_val_reg);

        // loop condition
        loop_scope.cond_begin = @intCast(u32, self.code.items.len);
        try self.emitInstruction(.IterNext, .{ iter_val_reg, iter_reg });

        // exit loop
        try self.emitInstruction(.JumpNone, .{ iter_val_reg, @as(u32, 0) });
        const exit_jump = self.code.items.len;

        if (node.capture != null) {
            const lval_res = if (self.tree.tokens.at(node.let_const.?).id == .Keyword_let)
                Result{ .lval = .{ .let = &Value{ .rt = iter_val_reg } } }
            else
                Result{ .lval = .{ .Const = &Value{ .rt = iter_val_reg } } };

            assert((try self.genNode(node.capture.?, lval_res)) == .empty);
        }

        const sub_res = switch (res) {
            .discard => res,
            .lval => unreachable,
            .value, .rt => return self.reportErr("TODO for expr", node.for_tok),
        };

        const body_val = try self.genNode(node.body, sub_res);
        if (sub_res != .rt and body_val.isRt()) {
            try self.emitInstruction(.Discard, .{body_val.getRt()});
        }

        // jump to the start of the loop
        try self.emitInstruction(.Jump, .{
            @truncate(i32, -@intCast(isize, self.code.items.len + @sizeOf(bog.Op) + @sizeOf(u32) - loop_scope.cond_begin)),
        });

        // exit loop when IterNext results in None
        self.finishJump(exit_jump);

        while (loop_scope.breaks.pop()) |some| {
            self.finishJump(some);
        }

        return sub_res.toVal();
    }

    fn genCatch(self: *Compiler, node: *Node.Catch, res: Result) Error!Value {
        try res.notLval(self, node.tok);

        var sub_res = switch (res) {
            .rt => res,
            .value, .discard => .value,
            .lval => unreachable,
        };
        const l_val = try self.genNodeNonEmpty(node.lhs, sub_res);
        if (!l_val.isRt()) {
            // not an error return as is
            return l_val.maybeRt(self, res);
        }
        sub_res = .{
            .rt = l_val.getRt(),
        };

        try self.emitInstruction(.JumpNotError, .{ sub_res.rt, @as(u32, 0) });
        const catch_skip = self.code.items.len;

        var capture_scope = Scope{
            .id = .capture,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.arena),
        };

        if (node.capture) |some| {
            const unwrap_reg = self.registerAlloc();
            try self.emitInstruction(.UnwrapError, .{ unwrap_reg, sub_res.rt });

            self.cur_scope = &capture_scope;
            const lval_res = if (self.tree.tokens.at(node.let_const.?).id == .Keyword_let)
                Result{ .lval = .{ .let = &Value{ .rt = unwrap_reg } } }
            else
                Result{ .lval = .{ .Const = &Value{ .rt = unwrap_reg } } };

            assert((try self.genNode(node.capture.?, lval_res)) == .empty);
        }

        const r_val = try self.genNode(node.rhs, sub_res);

        // end capture scope
        self.cur_scope = capture_scope.parent.?;
        self.finishJump(catch_skip);
        return sub_res.toVal();
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (r_val.isRt()) {
            const op_id: bog.Op = switch (node.op) {
                .boolNot => .BoolNot,
                .bitNot => .BitNot,
                .minus => .Negate,
                // TODO should unary + be a no-op
                .plus => return r_val,
                .Try => .Try,
            };
            const reg = r_val.getRt();
            defer r_val.free(self, reg);

            const sub_res = res.toRt(self);
            try self.emitInstruction(op_id, .{ sub_res.rt, reg });
            return sub_res.toVal();
        }
        const ret_val: Value = switch (node.op) {
            .boolNot => .{ .Bool = !try r_val.getBool(self, node.rhs.firstToken()) },
            .bitNot => .{ .int = ~try r_val.getInt(self, node.rhs.firstToken()) },
            .minus => blk: {
                try r_val.checkNum(self, node.rhs.firstToken());
                if (r_val == .int) {
                    // TODO check for overflow
                    break :blk Value{ .int = -r_val.int };
                } else {
                    break :blk Value{ .num = -r_val.num };
                }
            },
            .plus => blk: {
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
        const l_val = try self.genNodeNonEmpty(node.lhs, .value);

        const type_str = self.tokenSlice(node.type_tok);
        const type_id = if (mem.eql(u8, type_str, "none"))
            .none
        else if (mem.eql(u8, type_str, "int"))
            .int
        else if (mem.eql(u8, type_str, "num"))
            .num
        else if (mem.eql(u8, type_str, "bool"))
            .bool
        else if (mem.eql(u8, type_str, "str"))
            .str
        else if (mem.eql(u8, type_str, "tuple"))
            .tuple
        else if (mem.eql(u8, type_str, "map"))
            .map
        else if (mem.eql(u8, type_str, "list"))
            .list
        else if (mem.eql(u8, type_str, "error"))
            .err
        else if (mem.eql(u8, type_str, "range"))
            .range
        else if (mem.eql(u8, type_str, "fn"))
            bog.Type.func
        else
            return self.reportErr("expected a type name", node.type_tok);

        if (l_val.isRt()) {
            const sub_res = res.toRt(self);
            const reg = l_val.getRt();
            defer l_val.free(self, reg);

            const op: bog.Op = if (node.op == .as) .As else .Is;
            try self.emitInstruction(op, .{ sub_res.rt, reg, type_id });
            return sub_res.toVal();
        }

        const ret_val = switch (node.op) {
            .as => switch (type_id) {
                .none => Value{ .none = {} },
                .int => Value{
                    .int = switch (l_val) {
                        .int => |val| val,
                        .num => |val| @floatToInt(i64, val),
                        .Bool => |val| @boolToInt(val),
                        .str => |str| util.parseInt(str) catch
                            return self.reportErr("invalid cast to int", node.lhs.firstToken()),
                        else => return self.reportErr("invalid cast to int", node.lhs.firstToken()),
                    },
                },
                .num => Value{
                    .num = switch (l_val) {
                        .num => |val| val,
                        .int => |val| @intToFloat(f64, val),
                        .Bool => |val| @intToFloat(f64, @boolToInt(val)),
                        .str => |str| util.parseNum(str) catch
                            return self.reportErr("invalid cast to num", node.lhs.firstToken()),
                        else => return self.reportErr("invalid cast to num", node.lhs.firstToken()),
                    },
                },
                .bool => Value{
                    .Bool = switch (l_val) {
                        .int => |val| val != 0,
                        .num => |val| val != 0,
                        .Bool => |val| val,
                        .str => |val| if (mem.eql(u8, val, "true"))
                            true
                        else if (mem.eql(u8, val, "false"))
                            false
                        else
                            return self.reportErr("cannot cast string to bool", node.lhs.firstToken()),
                        else => return self.reportErr("invalid cast to bool", node.lhs.firstToken()),
                    },
                },
                .str => Value{
                    .str = switch (l_val) {
                        .int => |val| try std.fmt.allocPrint(self.arena, "{}", .{val}),
                        .num => |val| try std.fmt.allocPrint(self.arena, "{d}", .{val}),
                        .Bool => |val| if (val) "true" else "false",
                        .str => |val| val,
                        else => return self.reportErr("invalid cast to string", node.lhs.firstToken()),
                    },
                },
                .func => return self.reportErr("cannot cast to function", node.type_tok),
                .err => return self.reportErr("cannot cast to error", node.type_tok),
                .range => return self.reportErr("cannot cast to range", node.type_tok),
                .tuple, .map, .list => return self.reportErr("invalid cast", node.type_tok),
                .native, .iterator => unreachable,
                _ => unreachable,
            },
            .is => Value{
                .Bool = switch (type_id) {
                    .none => l_val == .none,
                    .int => l_val == .int,
                    .num => l_val == .num,
                    .bool => l_val == .Bool,
                    .str => l_val == .str,
                    else => false,
                },
            },
        };

        return ret_val.maybeRt(self, res);
    }

    fn genSuffix(self: *Compiler, node: *Node.Suffix, res: Result) Error!Value {
        if (node.op == .call) {
            try res.notLval(self, node.r_tok);
        }
        const l_val = try self.genNode(node.lhs, .value);
        if (l_val != .str and !l_val.isRt()) {
            return self.reportErr("invalid left hand side to suffix op", node.lhs.firstToken());
        }
        const l_reg = try l_val.toRt(self);
        defer l_val.free(self, l_reg);

        const index_val = switch (node.op) {
            .call => |*args| {
                const sub_res = res.toRt(self);
                const start = self.used_regs;
                self.used_regs += @intCast(RegRef, args.len);

                var it = args.iterator(0);
                var i = start;
                while (it.next()) |n| {
                    _ = try self.genNode(n.*, .{ .rt = i });
                    i += 1;
                }

                try self.emitInstruction(.Call, .{ sub_res.rt, l_reg, start, @truncate(u16, args.len) });
                return sub_res.toVal();
            },
            .member => Value{ .str = self.tokenSlice(node.r_tok) },
            .subscript => |val| try self.genNodeNonEmpty(val, .value),
        };
        const index_reg = try index_val.toRt(self);
        defer index_val.free(self, index_reg);

        const res_reg = switch (res) {
            .rt => |r| r,
            .lval => |l| switch (l) {
                .let, .Const => return self.reportErr("cannot declare to subscript", node.l_tok),
                .aug_assign => self.registerAlloc(),
                .assign => |r_val| {
                    const r_reg = try r_val.toRt(self);
                    defer r_val.free(self, r_reg);
                    try self.emitInstruction(.Set, .{ l_reg, index_reg, r_reg });
                    return Value.empty;
                },
            },
            .discard, .value => self.registerAlloc(),
        };

        try self.emitInstruction(.Get, .{ res_reg, l_reg, index_reg });
        return Value{ .rt = res_reg };
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
            => return self.genComparisonInfix(node, res),

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
            .RShiftAssign,
            .BitAndAssign,
            .BitOrAssign,
            .BitXOrAssign,
            => return self.genAssignInfix(node, res),
        }
    }

    fn genAssignInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        if (res != .discard) {
            return self.reportErr("assignment produces no value", node.tok);
        }
        const r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (node.op == .Assign) {
            const l_val = try self.genNode(node.lhs, .{ .lval = .{ .assign = &r_val } });
            std.debug.assert(l_val == .empty);
            return l_val;
        }

        const l_val = try self.genNode(node.lhs, .{ .lval = .aug_assign });
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
            .RShiftAssign,
            .BitAndAssign,
            .BitOrAssign,
            .BitXOrAssign,
            => _ = try r_val.getInt(self, node.rhs.firstToken()),
            else => unreachable,
        };

        const op_id: bog.Op = switch (node.op) {
            .AddAssign => .Add,
            .SubAssign => .Sub,
            .MulAssign => .Mul,
            .PowAssign => .Pow,
            .DivAssign => .Div,
            .DivFloorAssign => .DivFloor,
            .ModAssign => .Mod,
            .LShiftAssign => .LShift,
            .RShiftAssign => .RShift,
            .BitAndAssign => .BitAnd,
            .BitOrAssign => .BitOr,
            .BitXOrAssign => .BitXor,
            else => unreachable,
        };

        const reg = try r_val.toRt(self);
        defer r_val.free(self, reg);

        try self.emitInstruction(op_id, .{ l_val.getRt(), l_val.getRt(), reg });
        return Value.empty;
    }

    fn needNum(a: Value, b: Value) bool {
        return a == .num or b == .num;
    }

    fn genNumericInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (r_val.isRt() or l_val.isRt()) {
            const sub_res = res.toRt(self);

            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id: bog.Op = switch (node.op) {
                .Add => .Add,
                .Sub => .Sub,
                .Mul => .Mul,
                .Div => .Div,
                .DivFloor => .DivFloor,
                .Mod => .Mod,
                .Pow => .Pow,
                else => unreachable,
            };

            try self.emitInstruction(op_id, .{ sub_res.rt, l_reg, r_reg });
            return sub_res.toVal();
        }
        try l_val.checkNum(self, node.lhs.firstToken());
        try r_val.checkNum(self, node.rhs.firstToken());

        // TODO makeRuntime if overflow
        const ret_val = switch (node.op) {
            .Add => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = l_val.getNum() + r_val.getNum() };
                }
                break :blk Value{ .int = l_val.int + r_val.int };
            },
            .Sub => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = l_val.getNum() - r_val.getNum() };
                }
                break :blk Value{ .int = l_val.int - r_val.int };
            },
            .Mul => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = l_val.getNum() * r_val.getNum() };
                }
                break :blk Value{ .int = l_val.int * r_val.int };
            },
            .Div => Value{ .num = l_val.getNum() / r_val.getNum() },
            .DivFloor => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .int = @floatToInt(i64, @divFloor(l_val.getNum(), r_val.getNum())) };
                }
                break :blk Value{ .int = @divFloor(l_val.int, r_val.int) };
            },
            .Mod => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = @rem(l_val.getNum(), r_val.getNum()) };
                }
                break :blk Value{ .int = std.math.rem(i64, l_val.int, r_val.int) catch @panic("TODO") };
            },
            .Pow => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = std.math.pow(f64, l_val.getNum(), r_val.getNum()) };
                }
                break :blk Value{
                    .int = std.math.powi(i64, l_val.int, r_val.int) catch
                        return self.reportErr("TODO integer overflow", node.tok),
                };
            },
            else => unreachable,
        };

        return ret_val.maybeRt(self, res);
    }

    fn genComparisonInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (r_val.isRt() or l_val.isRt()) {
            const sub_res = res.toRt(self);

            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id: bog.Op = switch (node.op) {
                .LessThan => .LessThan,
                .LessThanEqual => .LessThanEqual,
                .GreaterThan => .GreaterThan,
                .GreaterThanEqual => .GreaterThanEqual,
                .Equal => .Equal,
                .NotEqual => .NotEqual,
                .In => .In,
                else => unreachable,
            };
            try self.emitInstruction(op_id, .{ sub_res.rt, l_reg, r_reg });
            return sub_res.toVal();
        }

        // order comparisons are only allowed on numbers
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
                    l_val.int < r_val.int,
            },
            .LessThanEqual => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() <= r_val.getNum()
                else
                    l_val.int <= r_val.int,
            },
            .GreaterThan => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() > r_val.getNum()
                else
                    l_val.int > r_val.int,
            },
            .GreaterThanEqual => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() >= r_val.getNum()
                else
                    l_val.int >= r_val.int,
            },
            .Equal, .NotEqual => blk: {
                const eql = switch (l_val) {
                    .none => |a_val| switch (r_val) {
                        .none => true,
                        else => false,
                    },
                    .int => |a_val| switch (r_val) {
                        .int => |b_val| a_val == b_val,
                        .num => |b_val| @intToFloat(f64, a_val) == b_val,
                        else => false,
                    },
                    .num => |a_val| switch (r_val) {
                        .int => |b_val| a_val == @intToFloat(f64, b_val),
                        .num => |b_val| a_val == b_val,
                        else => false,
                    },
                    .Bool => |a_val| switch (r_val) {
                        .Bool => |b_val| a_val == b_val,
                        else => false,
                    },
                    .str => |a_val| switch (r_val) {
                        .str => |b_val| mem.eql(u8, a_val, b_val),
                        else => false,
                    },
                    .empty, .rt, .ref, .func => unreachable,
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
        var l_val = try self.genNodeNonEmpty(node.lhs, .value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .value);

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

            const op_id: bog.Op = if (node.op == .BoolAnd) .BoolAnd else .BoolOr;
            try self.emitInstruction(op_id, .{ sub_res.rt, l_reg, r_reg });
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
        var l_val = try self.genNodeNonEmpty(node.lhs, .value);
        var r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (l_val.isRt() or r_val.isRt()) {
            const sub_res = res.toRt(self);

            const l_reg = try l_val.toRt(self);
            const r_reg = try r_val.toRt(self);
            defer {
                r_val.free(self, r_reg);
                l_val.free(self, l_reg);
            }

            const op_id: bog.Op = switch (node.op) {
                .BitAnd => .BitAnd,
                .BitOr => .BitOr,
                .BitXor => .BitXor,
                .LShift => .LShift,
                .RShift => .RShift,
                else => unreachable,
            };
            try self.emitInstruction(op_id, .{ sub_res.rt, l_reg, r_reg });
            return sub_res.toVal();
        }
        const l_int = try l_val.getInt(self, node.lhs.firstToken());
        const r_int = try r_val.getInt(self, node.rhs.firstToken());

        const ret_val: Value = switch (node.op) {
            .BitAnd => .{ .int = l_int & r_int },
            .BitOr => .{ .int = l_int | r_int },
            .BitXor => .{ .int = l_int ^ r_int },
            .LShift => blk: {
                if (r_int < 0)
                    return self.reportErr("shift by negative amount", node.rhs.firstToken());
                const val = if (r_int > std.math.maxInt(u6)) 0 else l_int << @intCast(u6, r_int);
                break :blk Value{ .int = val };
            },
            .RShift => blk: {
                if (r_int < 0)
                    return self.reportErr("shift by negative amount", node.rhs.firstToken());
                const val = if (r_int > std.math.maxInt(u6)) 0 else l_int >> @intCast(u6, r_int);
                break :blk Value{ .int = val };
            },
            else => unreachable,
        };

        return ret_val.maybeRt(self, res);
    }

    fn genDecl(self: *Compiler, node: *Node.Decl, res: Result) Error!Value {
        assert(res != .lval);
        const r_val = try self.genNodeNonEmpty(node.value, .value);

        const lval_kind = if (self.tree.tokens.at(node.let_const).id == .Keyword_let)
            Result{ .lval = .{ .let = &r_val } }
        else
            Result{ .lval = .{ .Const = &r_val } };

        assert((try self.genNode(node.capture, lval_kind)) == .empty);
        return Value.empty;
    }

    fn genIdentifier(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        const name = self.tokenSlice(node.tok);
        if (res == .lval) {
            switch (res.lval) {
                .let, .Const => |val| {
                    const sym = self.cur_scope.isDeclared(name);
                    if (sym != null and !sym.?.forward_decl) {
                        return self.reportErr("redeclaration of identifier", node.tok);
                    }
                    if (sym) |some| {
                        some.forward_decl = false;

                        // only functions can be forward declared
                        assert(val.* == .func);
                        try self.makeRuntime(some.reg, val.*);

                        return Value.empty;
                    }
                    var reg = try val.toRt(self);

                    if (val.* == .ref and res.lval == .let) {
                        // copy on assign
                        const copy_reg = self.registerAlloc();
                        try self.emitInstruction(.Copy, .{ copy_reg, reg });
                        reg = copy_reg;
                    }
                    try self.cur_scope.declSymbol(.{
                        .name = name,
                        .mutable = res.lval == .let,
                        .reg = reg,
                    });
                    return Value.empty;
                },
                .assign => |val| {
                    const sym = try self.cur_scope.getSymbol(self, name, node.tok);
                    if (!sym.mutable) {
                        return self.reportErr("assignment to constant", node.tok);
                    }
                    if (val.* == .ref) {
                        try self.emitInstruction(.Copy, .{ sym.reg, val.getRt() });
                    } else if (val.isRt()) {
                        try self.emitInstruction(.Move, .{ sym.reg, val.getRt() });
                    } else {
                        try self.makeRuntime(sym.reg, val.*);
                    }
                    return Value.empty;
                },
                .aug_assign => {
                    const sym = try self.cur_scope.getSymbol(self, name, node.tok);
                    if (!sym.mutable) {
                        return self.reportErr("assignment to constant", node.tok);
                    }
                    return Value{ .ref = sym.reg };
                },
            }
        }
        const sym = try self.cur_scope.getSymbol(self, name, node.tok);
        if (res == .rt) {
            const op_id: bog.Op = if (sym.mutable) .Move else .Copy;
            try self.emitInstruction(op_id, .{ res.rt, sym.reg });
            return res.toVal();
        }
        return Value{ .ref = sym.reg };
    }

    fn genThis(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        // `this` cannot be assigned to
        try res.notLval(self, node.tok);

        const sub_res = res.toRt(self);
        try self.emitInstruction(.LoadThis, .{sub_res.rt});
        return sub_res.toVal();
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const ret_val: Value = switch (node.kind) {
            .int => .{
                .int = util.parseInt(self.tokenSlice(node.tok)) catch
                    return self.reportErr("TODO big int", node.tok),
            },
            .True => .{ .Bool = true },
            .False => .{ .Bool = false },
            .none => .none,
            .str => .{ .str = try self.parseStr(node.tok) },
            .num => .{ .num = util.parseNum(self.tokenSlice(node.tok)) catch unreachable },
        };
        return ret_val.maybeRt(self, res);
    }

    fn genImport(self: *Compiler, node: *Node.Import, res: Result) Error!Value {
        try res.notLval(self, node.tok);

        const sub_res = res.toRt(self);
        const str = try self.parseStr(node.str_tok);
        const str_loc = try self.putString(str);

        try self.emitInstruction(.Import, .{ sub_res.rt, str_loc });
        return sub_res.toVal();
    }

    fn genNative(self: *Compiler, node: *Node.Native, res: Result) Error!Value {
        try res.notLval(self, node.tok);

        const sub_res = res.toRt(self);
        const str = try self.parseStr(node.str_tok);
        if (mem.indexOfScalar(u8, str, '.') == null) {
            return self.reportErr("invalid namespace", node.str_tok);
        }
        const str_loc = try self.putString(str);
        try self.emitInstruction(.BuildNative, .{ sub_res.rt, str_loc });
        return sub_res.toVal();
    }

    fn genError(self: *Compiler, node: *Node.Error, res: Result) Error!Value {
        if (res == .lval) switch (res.lval) {
            .Const, .let, .assign => |val| {
                if (!val.isRt()) {
                    return self.reportErr("expected an error", node.base.firstToken());
                }
                const unwrap_reg = self.registerAlloc();
                try self.emitInstruction(.UnwrapError, .{ unwrap_reg, val.getRt() });
                const r_val = Value{ .rt = unwrap_reg };
                const l_val = try self.genNode(node.value, switch (res.lval) {
                    .Const => Result{ .lval = .{ .Const = &r_val } },
                    .let => Result{ .lval = .{ .let = &r_val } },
                    .assign => Result{ .lval = .{ .assign = &r_val } },
                    else => unreachable,
                });
                std.debug.assert(l_val == .empty);
                return Value.empty;
            },
            .aug_assign => {
                return self.reportErr("invalid left hand side to augmented assignment", node.tok);
            },
        };
        const val = try self.genNodeNonEmpty(node.value, .value);

        const sub_res = res.toRt(self);
        const reg = try val.toRt(self);
        defer val.free(self, reg);

        try self.emitInstruction(.BuildError, .{ sub_res.rt, reg });
        return sub_res.toVal();
    }

    fn addLineInfo(self: *Compiler, node: *Node) !void {
        const token = node.firstToken();
        const tok = self.tree.tokens.at(token);

        try self.emitInstruction(.LineInfo, .{tok.start});
    }

    fn finishJump(self: *Compiler, jump_addr: usize) void {
        @ptrCast(*align(1) u32, self.code.items[jump_addr - @sizeOf(u32) ..].ptr).* =
            @truncate(u32, self.code.items.len - jump_addr);
    }

    fn getLastNode(self: *Compiler, first_node: *Node, allow_block: bool) *Node {
        var node = first_node;
        while (true) {
            switch (node.id) {
                .Grouped => node = @fieldParentPtr(Node.Grouped, "base", node).expr,
                .Block => {
                    if (!allow_block) return node;
                    const blk = @fieldParentPtr(Node.Block, "base", node);
                    node = blk.stmts.at(blk.stmts.len - 1).*;
                },
                else => return node,
            }
        }
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

    fn reportErr(self: *Compiler, msg: []const u8, tok: TokenIndex) Error {
        try self.errors.add(msg, self.tree.tokens.at(tok).start, .err);
        return error.CompileError;
    }
};
