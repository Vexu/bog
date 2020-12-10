const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const bog = @import("bog.zig");
const Node = bog.Node;
const Tree = bog.Tree;
const TokenIndex = bog.Token.Index;
const RegRef = bog.RegRef;
const Errors = bog.Errors;
const util = @import("util.zig");

pub const max_params = 32;

// made pub here to not expose it from bog.zig
pub const ModuleScope = Compiler.Scope.Module;

pub fn compile(gpa: *Allocator, source: []const u8, errors: *Errors) (Compiler.Error || bog.Parser.Error || bog.Tokenizer.Error)!*bog.Module {
    var tree = try bog.parse(gpa, source, errors);
    defer tree.deinit();

    // TODO reduce usage of arenas
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const arena = &arena_state.allocator;

    var root_scope: ModuleScope = .{
        .base = .{
            .syms = Compiler.Symbol.List.init(arena),
            .id = .module,
            .parent = null,
        },
        .code = Compiler.Code.init(arena),
    };
    var compiler = Compiler{
        .errors = errors,
        .tokens = tree.tokens,
        .source = source,
        .arena = arena,
        .module_code = Compiler.Code.init(gpa),
        .strings = std.ArrayList(u8).init(gpa),
        .code = &root_scope.code,
        .cur_scope = &root_scope.base,
        .string_interner = std.StringHashMap(u32).init(gpa),
    };
    defer {
        compiler.module_code.deinit();
        compiler.strings.deinit();
        compiler.string_interner.deinit();
    }

    for (tree.nodes) |node| {
        try compiler.autoForwardDecl(node);
    }
    for (tree.nodes) |node| {
        try compiler.addLineInfo(node);

        const val = try compiler.genNode(node, .discard);
        if (val.isRt()) {
            const reg = val.getRt();
            defer val.free(&compiler, reg);
            // discard unused runtime value
            try compiler.emitSingle(.discard_single, reg);
        }
    }

    const entry = compiler.module_code.items.len;
    try compiler.module_code.appendSlice(compiler.code.items);
    const mod = try gpa.create(bog.Module);
    mod.* = .{
        .name = "",
        .code = compiler.module_code.toOwnedSlice(),
        .strings = compiler.strings.toOwnedSlice(),
        .entry = @intCast(u32, entry),
    };
    return mod;
}

pub fn compileRepl(repl: *@import("repl.zig").Repl, node: *Node) Compiler.Error!bog.Module {
    var compiler = Compiler{
        .tokens = repl.tokenizer.tokens.items,
        .source = repl.buffer.items,
        .errors = &repl.vm.errors,
        .arena = &repl.arena.allocator,
        .module_code = repl.module_code,
        .strings = repl.strings,
        .code = &repl.root_scope.code,
        .cur_scope = &repl.root_scope.base,
        .string_interner = repl.string_interner,
        .used_regs = repl.used_regs,
    };
    defer {
        repl.used_regs = compiler.used_regs;
        repl.strings = compiler.strings;
        repl.string_interner = compiler.string_interner;
    }
    const start_len = repl.module_code.items.len;

    try compiler.autoForwardDecl(node);
    try compiler.addLineInfo(node);
    const val = try compiler.genNode(node, .discard);
    if (val != .empty) {
        const reg = try val.toRt(&compiler);
        defer val.free(&compiler, reg);

        try compiler.emitSingle(.discard_single, reg);
    }
    try compiler.module_code.appendSlice(compiler.code.items);
    compiler.code.resize(0) catch unreachable;

    return bog.Module{
        .name = "<stdin>",
        .code = compiler.module_code.items,
        .strings = compiler.strings.items,
        .entry = @intCast(u32, start_len),
    };
}

pub const Compiler = struct {
    tokens: []const bog.Token,
    source: []const u8,
    errors: *Errors,
    arena: *Allocator,
    cur_scope: *Scope,
    used_regs: RegRef = 0,
    code: *Code,
    module_code: Code,
    strings: std.ArrayList(u8),
    string_interner: std.StringHashMap(u32),

    pub const Code = std.ArrayList(bog.Instruction);

    pub const Error = error{CompileError} || Allocator.Error;

    fn registerAlloc(self: *Compiler) RegRef {
        defer self.used_regs += 1;
        if (self.used_regs == 0xff) @panic("TODO: ran out of registers");
        return self.used_regs;
    }

    fn registerFree(self: *Compiler, reg: RegRef) void {
        if (reg == self.used_regs - 1) {
            self.used_regs -= 1;
        }
    }

    fn emitSingle(self: *Compiler, op: bog.Op, arg: RegRef) !void {
        try self.code.append(.{
            .single = .{
                .op = op,
                .arg = arg,
            },
        });
    }

    fn emitDouble(self: *Compiler, op: bog.Op, res: RegRef, arg: RegRef) !void {
        try self.code.append(.{
            .double = .{
                .op = op,
                .res = res,
                .arg = arg,
            },
        });
    }

    fn emitTriple(self: *Compiler, op: bog.Op, res: RegRef, lhs: RegRef, rhs: RegRef) !void {
        try self.code.append(.{
            .triple = .{
                .op = op,
                .res = res,
                .lhs = lhs,
                .rhs = rhs,
            },
        });
    }

    fn emitOff(self: *Compiler, op: bog.Op, res: RegRef, off: u32) !void {
        const long = off >= 0xFFFF;
        try self.code.append(.{
            .off = .{
                .op = op,
                .res = res,
                .off = if (long) 0xFFFF else @truncate(u16, off),
            },
        });
        if (long) try self.code.append(.{ .bare = off });
    }

    fn emitJump(self: *Compiler, op: bog.Op, arg: ?RegRef) !u32 {
        try self.code.append(.{ .jump = .{ .op = op, .arg = arg orelse 0 } });
        try self.code.append(undefined);
        return @intCast(u32, self.code.items.len - 1);
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
            try_catch,
        };

        const Fn = struct {
            base: Scope,
            code: Code,
            captures: *Symbol.List,
        };

        const Module = struct {
            base: Scope,
            code: Code,
        };

        const Loop = struct {
            base: Scope,
            breaks: BreakList,
            cond_begin: u32,

            const BreakList = std.ArrayList(u32);
        };

        const Try = struct {
            base: Scope,
            jumps: JumpList,
            err_reg: RegRef,

            const JumpList = std.ArrayList(u32);
        };

        fn declSymbol(self: *Scope, sym: Symbol) !void {
            try self.syms.append(sym);
        }

        fn isDeclared(scope: *Scope, name: []const u8) ?*Symbol {
            var cur: ?*Scope = scope;
            while (cur) |some| {
                var i = some.syms.items.len;
                while (i > 0) {
                    i -= 1;
                    const sym = &some.syms.items[i];
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
                var i = some.syms.items.len;
                while (i > 0) {
                    i -= 1;
                    const sym = some.syms.items[i];
                    if (mem.eql(u8, sym.name, name)) {
                        if (func) |parent| {
                            try parent.captures.append(sym);
                            return RegAndMut{
                                .reg = @intCast(RegRef, parent.captures.items.len - 1),
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
                    i = new_func.captures.items.len;
                    while (i > 0) {
                        i -= 1;
                        const sym = new_func.captures.items[i];
                        if (!mem.eql(u8, sym.name, name)) continue;

                        try new_func.code.append(.{
                            .double = .{
                                .op = .load_capture_double,
                                .res = res,
                                .arg = @intCast(u8, sym.reg),
                            },
                        });
                        return RegAndMut{
                            .reg = res,
                            .mutable = sym.mutable,
                        };
                    }
                    const parent = some.parent orelse break :blk;
                    const sub = try parent.getSymbolTail(self, name, new_func, tok);
                    try new_func.code.append(.{
                        .double = .{
                            .op = .load_capture_double,
                            .res = res,
                            .arg = @intCast(u8, sub.reg),
                        },
                    });

                    // forward captured symbol
                    if (func) |parent_fn| {
                        try parent_fn.captures.append(.{
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

        fn getTry(scope: *Scope) ?*Scope.Try {
            var cur: ?*Scope = scope;
            while (cur) |some| {
                switch (some.id) {
                    .try_catch => return @fieldParentPtr(Try, "base", some),
                    .func, .module => return null,
                    else => {},
                }
                cur = some.parent;
            }
            unreachable;
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

        pub const List = std.ArrayList(Symbol);
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
            .none => try self.code.append(.{
                .primitive = .{ .res = res, .kind = .none },
            }),
            .int => |v| if (v >= std.math.minInt(i15) and v <= std.math.maxInt(i15)) {
                try self.code.append(.{ .int = .{ .res = res, .long = false, .arg = @truncate(i15, v) } });
            } else {
                try self.code.append(.{ .int = .{ .res = res, .long = true, .arg = 0 } });
                const arr = @bitCast([2]u32, v);
                try self.code.append(.{ .bare = arr[0] });
                try self.code.append(.{ .bare = arr[1] });
            },
            .num => |v| {
                try self.code.append(.{ .single = .{ .op = .const_num, .arg = res } });
                const arr = @bitCast([2]u32, v);
                try self.code.append(.{ .bare = arr[0] });
                try self.code.append(.{ .bare = arr[1] });
            },
            .Bool => |v| try self.code.append(.{ .primitive = .{ .res = res, .kind = if (v) .True else .False } }),
            .str => |v| try self.emitOff(.const_string_off, res, try self.putString(v)),
            .func => |v| {
                try self.code.append(.{
                    .func = .{
                        .res = res,
                        .arg_count = v.params,
                        .capture_count = @intCast(u8, v.captures.items.len),
                    },
                });
                try self.code.append(.{ .bare = v.offset });

                for (v.captures.items) |capture, i| {
                    try self.emitTriple(
                        .store_capture_triple,
                        res,
                        capture.reg,
                        @intCast(u8, i),
                    );
                }
            },
        };
    }

    fn putString(self: *Compiler, str: []const u8) !u32 {
        if (self.string_interner.get(str)) |some| return some;
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

    fn autoForwardDecl(self: *Compiler, node: *Node) error{OutOfMemory}!void {
        if (node.id != .Decl) return;
        const decl = @fieldParentPtr(Node.Decl, "base", node);

        // only forward declarations like
        // `const IDENTIFIER = fn ...`
        if (self.tokens[decl.let_const].id == .Keyword_const and
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
            .Import => return self.genImport(@fieldParentPtr(Node.Import, "base", node), res),
            .For => return self.genFor(@fieldParentPtr(Node.For, "base", node), res),
            .Map => return self.genMap(@fieldParentPtr(Node.ListTupleMap, "base", node), res),
            .MapItem => unreachable,
            .This => return self.genThis(@fieldParentPtr(Node.SingleToken, "base", node), res),
            .Tagged => return self.genTagged(@fieldParentPtr(Node.Tagged, "base", node), res),
            .Range => return self.genRange(@fieldParentPtr(Node.Range, "base", node), res),
            .FormatString => return self.genFormatString(@fieldParentPtr(Node.FormatString, "base", node), res),
            .Try => return self.genTry(@fieldParentPtr(Node.Try, "base", node), res),
            .Catch => unreachable, // hanlded in genTry
            .Match => return self.genMatch(@fieldParentPtr(Node.Match, "base", node), res),
            .MatchCatchAll => unreachable, // hanlded in genMatch
            .MatchLet => unreachable, // hanlded in genMatch
            .MatchCase => unreachable, // hanlded in genMatch
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
        if (node.values.len > std.math.maxInt(u32)) {
            return self.reportErr("too many items", node.base.firstToken());
        }

        if (res == .lval) {
            switch (res.lval) {
                .Const, .let, .assign => |lval| {
                    if (!lval.isRt()) {
                        return self.reportErr("expected a map", node.base.firstToken());
                    }
                    const container_reg = lval.getRt();
                    const index_reg = self.registerAlloc();
                    var result_reg = self.registerAlloc();

                    for (node.values) |val, i| {
                        const item = @fieldParentPtr(Node.MapItem, "base", val);

                        if (item.key) |some| {
                            const last_node = self.getLastNode(some);
                            if (last_node.id == .Identifier) {
                                // `oldname: newname` is equal to `"oldname": newname`
                                const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                                const str_loc = try self.putString(self.tokenSlice(ident.tok));
                                try self.emitOff(.const_string_off, index_reg, str_loc);
                            } else {
                                _ = try self.genNode(some, .{ .rt = index_reg });
                            }
                        } else {
                            const last_node = self.getLastNode(item.value);
                            if (last_node.id != .Identifier) {
                                return self.reportErr("expected a key", item.value.firstToken());
                            }
                            // `oldname` is equal to `"oldname": oldname`
                            const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                            const str_loc = try self.putString(self.tokenSlice(ident.tok));
                            try self.emitOff(.const_string_off, index_reg, str_loc);
                        }

                        try self.emitTriple(.get_triple, result_reg, container_reg, index_reg);
                        const rt_val = Value{ .rt = result_reg };
                        const l_val = try self.genNode(item.value, switch (res.lval) {
                            .Const => .{ .lval = .{ .Const = &rt_val } },
                            .let => .{ .lval = .{ .let = &rt_val } },
                            .assign => .{ .lval = .{ .assign = &rt_val } },
                            else => unreachable,
                        });
                        std.debug.assert(l_val == .empty);

                        if (i + 1 != node.values.len and res.lval != .assign) result_reg = self.registerAlloc();
                    }
                    return Value.empty;
                },
                .aug_assign => {
                    return self.reportErr("invalid left hand side to augmented assignment", node.r_tok);
                },
            }
        }

        const sub_res = res.toRt(self);
        try self.emitOff(.build_map_off, sub_res.rt, @intCast(u32, node.values.len));

        // prepare registers
        const container_reg = sub_res.rt;
        const index_reg = self.registerAlloc();
        defer self.registerFree(index_reg);
        const result_reg = self.registerAlloc();
        defer self.registerFree(result_reg);

        for (node.values) |val| {
            const item = @fieldParentPtr(Node.MapItem, "base", val);

            if (item.key) |some| {
                const last_node = self.getLastNode(some);
                if (last_node.id == .Identifier) {
                    // `ident: value` is equal to `"ident": value`
                    const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                    const str_loc = try self.putString(self.tokenSlice(ident.tok));
                    try self.emitOff(.const_string_off, index_reg, str_loc);
                } else {
                    _ = try self.genNode(some, .{ .rt = index_reg });
                }
            } else {
                const last_node = self.getLastNode(item.value);
                if (last_node.id != .Identifier) {
                    return self.reportErr("expected a key", item.value.firstToken());
                }
                // `ident` is equal to `"ident": ident`
                const ident = @fieldParentPtr(Node.SingleToken, "base", last_node);
                const str_loc = try self.putString(self.tokenSlice(ident.tok));
                try self.emitOff(.const_string_off, index_reg, str_loc);
            }

            _ = try self.genNode(item.value, .{ .rt = result_reg });
            try self.emitTriple(.set_triple, container_reg, index_reg, result_reg);
        }

        return sub_res.toVal();
    }

    fn genTupleList(self: *Compiler, node: *Node.ListTupleMap, res: Result) Error!Value {
        if (node.values.len > std.math.maxInt(u32)) {
            return self.reportErr("too many items", node.base.firstToken());
        }
        if (res == .lval) {
            switch (res.lval) {
                .Const, .let, .assign => |lval| {
                    if (!lval.isRt()) {
                        return self.reportErr("expected a tuple/list", node.base.firstToken());
                    }

                    // prepare registers
                    const container_reg = lval.getRt();
                    const index_reg = self.registerAlloc();
                    var result_reg = self.registerAlloc();

                    var index = Value{ .int = 0 };
                    for (node.values) |val, i| {
                        if (val.id == .Discard) {
                            index.int += 1;
                            continue;
                        }
                        try self.makeRuntime(index_reg, index);
                        try self.emitTriple(.get_triple, result_reg, container_reg, index_reg);
                        const rt_val = Value{ .rt = result_reg };
                        const l_val = try self.genNode(val, switch (res.lval) {
                            .Const => .{ .lval = .{ .Const = &rt_val } },
                            .let => .{ .lval = .{ .let = &rt_val } },
                            .assign => .{ .lval = .{ .assign = &rt_val } },
                            else => unreachable,
                        });
                        std.debug.assert(l_val == .empty);
                        index.int += 1;

                        if (i + 1 != node.values.len and res.lval != .assign) result_reg = self.registerAlloc();
                    }
                    return Value.empty;
                },
                .aug_assign => {
                    return self.reportErr("invalid left hand side to augmented assignment", node.r_tok);
                },
            }
        }

        const sub_res = res.toRt(self);
        try self.emitOff(switch (node.base.id) {
            .Tuple => .build_tuple_off,
            .List => .build_list_off,
            else => unreachable,
        }, sub_res.rt, @intCast(u32, node.values.len));

        // prepare registers
        const container_reg = sub_res.rt;
        const index_reg = self.registerAlloc();
        defer self.registerFree(index_reg);
        const result_reg = self.registerAlloc();
        defer self.registerFree(result_reg);

        var index = Value{ .int = 0 };
        for (node.values) |val| {
            _ = try self.genNode(val, .{ .rt = result_reg });

            try self.makeRuntime(index_reg, index);
            try self.emitTriple(.set_triple, container_reg, index_reg, result_reg);
            index.int += 1;
        }

        return sub_res.toVal();
    }

    fn genFn(self: *Compiler, node: *Node.Fn, res: Result) Error!Value {
        try res.notLval(self, node.fn_tok);

        if (node.params.len > max_params) {
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
                .syms = Symbol.List.init(self.errors.list.allocator),
            },
            .code = try Code.initCapacity(self.arena, 256),
            .captures = captures,
        };
        self.cur_scope = &fn_scope.base;
        defer {
            fn_scope.code.deinit();
            fn_scope.base.syms.deinit();
            self.cur_scope = fn_scope.base.parent.?;
        }

        // function body is emitted to a new arraylist and finally added to module_code
        const old_code = self.code;
        self.code = &fn_scope.code;

        // destructure parameters
        self.used_regs = param_count;
        var i: RegRef = 0;
        for (node.params) |param| {
            const param_res = try self.genNode(param, .{
                .lval = .{
                    .let = &Value{ .rt = i },
                },
            });
            std.debug.assert(param_res == .empty);
            i += 1;
        }

        // gen body and return result
        try self.addLineInfo(node.body);

        // for one liner functions return the value of the expression,
        // otherwise require an explicit return statement
        const last = self.getLastNode(node.body);
        const should_discard = switch (last.id) {
            .Block => true,
            .Infix => switch (@fieldParentPtr(Node.Infix, "base", last).op) {
                .assign, .add_assign, .sub_assign, .mul_assign, .pow_assign, // -
                .div_assign, .div_floor_assign, .mod_assign, .l_shift_assign, // -
                .r_shift_assign, .bit_and_assign, .bit_or_assign, .bit_x_or_assign => true,
                else => false,
            },
            else => false,
        };
        const body_val = try self.genNode(node.body, if (should_discard) .discard else .value);

        if (body_val == .empty or body_val == .none) {
            try self.code.append(.{ .op = .{ .op = .return_none } });
        } else {
            const reg = try body_val.toRt(self);
            defer body_val.free(self, reg);

            try self.emitSingle(.return_single, reg);
        }

        // reset regs after generating body
        self.used_regs = old_used_regs;
        self.code = old_code;

        const offset = @intCast(u32, self.module_code.items.len);
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
        try res.notLval(self, node.stmts[0].firstToken());
        var block_scope = Scope{
            .id = .block,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.errors.list.allocator),
        };
        self.cur_scope = &block_scope;
        defer {
            block_scope.syms.deinit();
            self.cur_scope = block_scope.parent.?;
        }

        for (node.stmts) |stmt, i| {
            try self.addLineInfo(stmt);

            // return value of last instruction if it is not discarded
            if (i + 1 == node.stmts.len and res != .discard) {
                return self.genNode(stmt, res);
            }

            const val = try self.genNode(stmt, .discard);
            if (val.isRt()) {
                const reg = val.getRt();
                defer val.free(self, reg);

                // discard unused runtime value
                try self.emitSingle(.discard_single, reg);
            }
        }
        return Value{ .empty = {} };
    }

    fn genIf(self: *Compiler, node: *Node.If, res: Result) Error!Value {
        try res.notLval(self, node.if_tok);

        var capture_scope = Scope{
            .id = .capture,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.errors.list.allocator),
        };
        defer capture_scope.syms.deinit();
        var if_skip: usize = undefined;

        const cond_val = try self.genNodeNonEmpty(node.cond, .value);
        if (node.capture != null) {
            // TODO handle cond_val.isRt()
            const cond_reg = try cond_val.toRt(self);
            // jump past if_body if cond == .none
            if_skip = try self.emitJump(.jump_none, cond_reg);

            self.cur_scope = &capture_scope;
            const lval_res = if (self.tokens[node.let_const.?].id == .Keyword_let)
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
            if_skip = try self.emitJump(.jump_false, cond_val.getRt());
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
            try self.emitSingle(.discard_single, if_val.getRt());
        }

        // jump past else_body since if_body was executed
        const else_skip = if (node.else_body != null or sub_res == .rt)
            try self.emitJump(.jump, null)
        else
            null;

        self.finishJump(if_skip);
        // end capture scope
        self.cur_scope = capture_scope.parent.?;

        if (node.else_body) |some| {
            const else_val = try self.genNode(some, sub_res);
            if (sub_res != .rt and else_val.isRt()) {
                try self.emitSingle(.discard_single, else_val.getRt());
            }
        } else if (sub_res == .rt) {
            try self.code.append(.{
                .primitive = .{ .res = sub_res.rt, .kind = .none },
            });
        }

        if (else_skip) |some| {
            self.finishJump(some);
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

                try self.emitSingle(.return_single, reg);
            } else {
                try self.code.append(.{ .op = .{ .op = .return_none } });
            }
            return Value{ .empty = {} };
        }

        // find inner most loop
        const loop_scope = blk: {
            var scope = self.cur_scope;
            while (true) switch (scope.id) {
                .module, .func => return self.reportErr(if (node.op == .Continue)
                    "continue outside of loop"
                else
                    "break outside of loop", node.tok),
                .loop => break,
                else => scope = scope.parent.?,
            };
            break :blk @fieldParentPtr(Scope.Loop, "base", scope);
        };
        if (node.op == .Continue) {
            self.code.items[try self.emitJump(.jump, null)] = .{
                .bare_signed = @intCast(i32, -@intCast(isize, self.code.items.len - loop_scope.cond_begin)),
            };
        } else {
            try loop_scope.breaks.append(try self.emitJump(.jump, null));
        }

        return Value{ .empty = {} };
    }

    fn createListComprehension(self: *Compiler, reg: ?RegRef) !Result {
        const list = reg orelse self.registerAlloc();
        try self.emitOff(.build_list_off, list, 0);
        return Result{ .rt = list };
    }

    fn genWhile(self: *Compiler, node: *Node.While, res: Result) Error!Value {
        try res.notLval(self, node.while_tok);

        const sub_res = switch (res) {
            .discard => res,
            .lval => unreachable,
            .value => try self.createListComprehension(null),
            .rt => |reg| try self.createListComprehension(reg),
        };

        var loop_scope = Scope.Loop{
            .base = .{
                .id = .loop,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.errors.list.allocator),
            },
            .breaks = Scope.Loop.BreakList.init(self.errors.list.allocator),
            .cond_begin = @intCast(u32, self.code.items.len),
        };
        self.cur_scope = &loop_scope.base;
        defer {
            self.cur_scope = loop_scope.base.parent.?;
            loop_scope.base.syms.deinit();
            loop_scope.breaks.deinit();
        }

        // beginning of condition
        var cond_jump: ?usize = null;

        const cond_val = try self.genNode(node.cond, .value);
        if (node.capture != null) {
            // TODO handle cond_val.isRt()
            const cond_reg = try cond_val.toRt(self);
            // jump past exit loop if cond == .none
            cond_jump = try self.emitJump(.jump_none, cond_reg);

            const lval_res = if (self.tokens[node.let_const.?].id == .Keyword_let)
                Result{ .lval = .{ .let = &Value{ .rt = cond_reg } } }
            else
                Result{ .lval = .{ .Const = &Value{ .rt = cond_reg } } };

            assert((try self.genNode(node.capture.?, lval_res)) == .empty);
        } else if (cond_val.isRt()) {
            cond_jump = try self.emitJump(.jump_false, cond_val.getRt());
        } else {
            const bool_val = try cond_val.getBool(self, node.cond.firstToken());
            if (bool_val == false) {
                // never executed
                const res_val = Value{ .none = {} };
                return res_val.maybeRt(self, res);
            }
        }

        switch (sub_res) {
            .discard => {
                const body_val = try self.genNode(node.body, res);
                if (body_val.isRt()) {
                    try self.emitSingle(.discard_single, body_val.getRt());
                }
            },
            .rt => |list| {
                const body_val = try self.genNodeNonEmpty(node.body, .value);
                const reg = try body_val.toRt(self);
                defer self.registerFree(reg);
                try self.emitDouble(.append_double, list, reg);
            },
            else => unreachable,
        }

        // jump back to condition
        const end = try self.emitJump(.jump, null);
        self.code.items[end] = .{
            .bare_signed = @truncate(i32, -@intCast(isize, end - loop_scope.cond_begin)),
        };

        // exit loop if cond == false
        if (cond_jump) |some| {
            self.finishJump(some);
        }
        for (loop_scope.breaks.items) |jump| {
            self.finishJump(jump);
        }

        return sub_res.toVal();
    }

    fn genFor(self: *Compiler, node: *Node.For, res: Result) Error!Value {
        try res.notLval(self, node.for_tok);

        var loop_scope = Scope.Loop{
            .base = .{
                .id = .loop,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.errors.list.allocator),
            },
            .breaks = Scope.Loop.BreakList.init(self.errors.list.allocator),
            .cond_begin = undefined,
        };
        self.cur_scope = &loop_scope.base;
        defer {
            self.cur_scope = loop_scope.base.parent.?;
            loop_scope.base.syms.deinit();
            loop_scope.breaks.deinit();
        }

        const sub_res = switch (res) {
            .discard => res,
            .lval => unreachable,
            .value => try self.createListComprehension(null),
            .rt => |reg| try self.createListComprehension(reg),
        };

        const cond_val = try self.genNode(node.cond, .value);
        if (!cond_val.isRt() and cond_val != .str)
            return self.reportErr("expected iterable value", node.cond.firstToken());

        const cond_reg = try cond_val.toRt(self);
        defer cond_val.free(self, cond_reg);

        const iter_reg = self.registerAlloc();
        defer self.registerFree(iter_reg);

        // initialize the iterator
        try self.emitDouble(.iter_init_double, iter_reg, cond_reg);

        const iter_val_reg = self.registerAlloc();
        defer self.registerFree(iter_val_reg);

        // loop condition
        loop_scope.cond_begin = @intCast(u32, self.code.items.len);

        // iter_next is fused with a jump_none
        try self.emitDouble(.iter_next_double, iter_val_reg, iter_reg);
        const exit_jump = self.code.items.len;
        try self.code.append(.{ .bare = 0 });

        if (node.capture != null) {
            const lval_res = if (self.tokens[node.let_const.?].id == .Keyword_let)
                Result{ .lval = .{ .let = &Value{ .rt = iter_val_reg } } }
            else
                Result{ .lval = .{ .Const = &Value{ .rt = iter_val_reg } } };

            assert((try self.genNode(node.capture.?, lval_res)) == .empty);
        }

        switch (sub_res) {
            .discard => {
                const body_val = try self.genNode(node.body, res);
                if (body_val.isRt()) {
                    try self.emitSingle(.discard_single, body_val.getRt());
                }
            },
            .rt => |list| {
                const body_val = try self.genNodeNonEmpty(node.body, .value);
                const reg = try body_val.toRt(self);
                defer self.registerFree(reg);
                try self.emitDouble(.append_double, list, reg);
            },
            else => unreachable,
        }

        // jump to the start of the loop
        const end = try self.emitJump(.jump, null);
        self.code.items[end] = .{
            .bare_signed = @truncate(i32, -@intCast(isize, end - loop_scope.cond_begin)),
        };

        // exit loop when IterNext results in None
        self.finishJump(exit_jump);
        for (loop_scope.breaks.items) |jump| {
            self.finishJump(jump);
        }

        return sub_res.toVal();
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        const r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (r_val.isRt()) {
            const op_id: bog.Op = switch (node.op) {
                .bool_not => .bool_not_double,
                .bit_not => .bit_not_double,
                .minus => .negate_double,
                // TODO should unary + be a no-op
                .plus => return r_val,
            };
            const reg = r_val.getRt();
            defer r_val.free(self, reg);

            const sub_res = res.toRt(self);
            try self.emitDouble(op_id, sub_res.rt, reg);
            return sub_res.toVal();
        }
        const ret_val: Value = switch (node.op) {
            .bool_not => .{ .Bool = !try r_val.getBool(self, node.rhs.firstToken()) },
            .bit_not => .{ .int = ~try r_val.getInt(self, node.rhs.firstToken()) },
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
            .func
        else if (mem.eql(u8, type_str, "tagged"))
            bog.Type.tagged
        else
            return self.reportErr("expected a type name", node.type_tok);

        if (l_val.isRt()) {
            const sub_res = res.toRt(self);
            const reg = l_val.getRt();
            defer l_val.free(self, reg);

            try self.code.append(.{
                .type_id = .{
                    .op = if (node.op == .as) .as_type_id else .is_type_id,
                    .res = sub_res.rt,
                    .arg = reg,
                    .type_id = type_id,
                },
            });
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
                .tuple, .map, .list, .tagged => return self.reportErr("invalid cast", node.type_tok),
                else => unreachable,
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
        if (l_val != .str and l_val != .func and !l_val.isRt()) {
            return self.reportErr("invalid left hand side to suffix op", node.lhs.firstToken());
        }
        const l_reg = try l_val.toRt(self);
        defer l_val.free(self, l_reg);

        const index_val = switch (node.op) {
            .call => |args| {
                if (args.len > max_params) {
                    return self.reportErr("too many arguments", node.l_tok);
                }

                const sub_res = res.toRt(self);
                const start = self.used_regs;
                self.used_regs += @truncate(RegRef, args.len);

                var i = start;
                for (args) |arg| {
                    _ = try self.genNode(arg, .{ .rt = i });
                    i += 1;
                }

                try self.code.append(.{
                    .call = .{
                        .res = sub_res.rt,
                        .func = l_reg,
                        .first = start,
                    },
                });
                try self.code.append(.{ .bare = @truncate(u8, args.len) });

                if (self.cur_scope.getTry()) |try_scope| {
                    try self.emitDouble(.move_double, try_scope.err_reg, sub_res.rt);
                    try try_scope.jumps.append(try self.emitJump(.jump_error, sub_res.rt));
                }

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
                    try self.emitTriple(.set_triple, l_reg, index_reg, r_reg);
                    return Value.empty;
                },
            },
            .discard, .value => self.registerAlloc(),
        };

        try self.emitTriple(.get_triple, res_reg, l_reg, index_reg);
        return Value{ .rt = res_reg };
    }

    fn genInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        try res.notLval(self, node.tok);
        switch (node.op) {
            .bool_or,
            .bool_and,
            => return self.genBoolInfix(node, res),

            .less_than,
            .less_than_equal,
            .greater_than,
            .greater_than_equal,
            .equal,
            .not_equal,
            .in,
            => return self.genComparisonInfix(node, res),

            .bit_and,
            .bit_or,
            .bit_xor,
            .l_shift,
            .r_shift,
            => return self.genIntInfix(node, res),

            .add,
            .sub,
            .mul,
            .div,
            .div_floor,
            .mod,
            .pow,
            => return self.genNumericInfix(node, res),

            .assign,
            .add_assign,
            .sub_assign,
            .mul_assign,
            .pow_assign,
            .div_assign,
            .div_floor_assign,
            .mod_assign,
            .l_shift_assign,
            .r_shift_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_x_or_assign,
            => return self.genAssignInfix(node, res),

            .append => {
                var l_val = try self.genNodeNonEmpty(node.lhs, .value);
                var r_val = try self.genNodeNonEmpty(node.rhs, .value);

                if (l_val.isRt() or r_val.isRt()) {
                    const l_reg = try l_val.toRt(self);
                    const r_reg = try r_val.toRt(self);
                    defer r_val.free(self, r_reg);

                    try self.emitDouble(.append_double, l_reg, r_reg);
                    if (res == .discard) {
                        return Value{ .none = {} };
                    } else if (res == .rt) {
                        try self.emitDouble(.move_double, res.rt, l_reg);
                        return Value{ .rt = res.rt };
                    } else {
                        return Value{ .rt = l_reg };
                    }
                }

                if (res == .discard) {
                    return Value{ .none = {} };
                }

                const l_str = try l_val.getStr(self, node.lhs.firstToken());
                const r_str = try r_val.getStr(self, node.rhs.firstToken());

                const concatted = Value{
                    .str = try mem.concat(self.arena, u8, &[_][]const u8{ l_str, r_str }),
                };
                return concatted.maybeRt(self, res);
            },
        }
    }

    fn genAssignInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        if (res != .discard) {
            return self.reportErr("assignment produces no value", node.tok);
        }
        const r_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (node.op == .assign) {
            const l_val = try self.genNode(node.lhs, .{ .lval = .{ .assign = &r_val } });
            std.debug.assert(l_val == .empty);
            return l_val;
        }

        const l_val = try self.genNode(node.lhs, .{ .lval = .aug_assign });
        if (!r_val.isRt()) switch (node.op) {
            .add_assign,
            .sub_assign,
            .mul_assign,
            .pow_assign,
            .div_assign,
            .div_floor_assign,
            .mod_assign,
            => try r_val.checkNum(self, node.rhs.firstToken()),

            .l_shift_assign,
            .r_shift_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_x_or_assign,
            => _ = try r_val.getInt(self, node.rhs.firstToken()),
            else => unreachable,
        };

        const reg = try r_val.toRt(self);
        defer r_val.free(self, reg);

        try self.emitTriple(switch (node.op) {
            .add_assign => .add_triple,
            .sub_assign => .sub_triple,
            .mul_assign => .mul_triple,
            .pow_assign => .pow_triple,
            .div_assign => .div_triple,
            .div_floor_assign => .div_floor_triple,
            .mod_assign => .mod_triple,
            .l_shift_assign => .l_shift_triple,
            .r_shift_assign => .r_shift_triple,
            .bit_and_assign => .bit_and_triple,
            .bit_or_assign => .bit_or_triple,
            .bit_x_or_assign => .bit_xor_triple,
            else => unreachable,
        }, l_val.getRt(), l_val.getRt(), reg);
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

            try self.emitTriple(switch (node.op) {
                .add => .add_triple,
                .sub => .sub_triple,
                .mul => .mul_triple,
                .div => .div_triple,
                .div_floor => .div_floor_triple,
                .mod => .mod_triple,
                .pow => .pow_triple,
                else => unreachable,
            }, sub_res.rt, l_reg, r_reg);
            return sub_res.toVal();
        }
        try l_val.checkNum(self, node.lhs.firstToken());
        try r_val.checkNum(self, node.rhs.firstToken());

        // TODO makeRuntime if overflow
        const ret_val = switch (node.op) {
            .add => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = l_val.getNum() + r_val.getNum() };
                }
                break :blk Value{ .int = l_val.int + r_val.int };
            },
            .sub => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = l_val.getNum() - r_val.getNum() };
                }
                break :blk Value{ .int = l_val.int - r_val.int };
            },
            .mul => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = l_val.getNum() * r_val.getNum() };
                }
                break :blk Value{ .int = l_val.int * r_val.int };
            },
            .div => Value{ .num = l_val.getNum() / r_val.getNum() },
            .div_floor => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .int = @floatToInt(i64, @divFloor(l_val.getNum(), r_val.getNum())) };
                }
                break :blk Value{ .int = @divFloor(l_val.int, r_val.int) };
            },
            .mod => blk: {
                if (needNum(l_val, r_val)) {
                    break :blk Value{ .num = @rem(l_val.getNum(), r_val.getNum()) };
                }
                break :blk Value{ .int = std.math.rem(i64, l_val.int, r_val.int) catch @panic("TODO") };
            },
            .pow => blk: {
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

            try self.emitTriple(switch (node.op) {
                .less_than => .less_than_triple,
                .less_than_equal => .less_than_equal_triple,
                .greater_than => .greater_than_triple,
                .greater_than_equal => .greater_than_equal_triple,
                .equal => .equal_triple,
                .not_equal => .not_equal_triple,
                .in => .in_triple,
                else => unreachable,
            }, sub_res.rt, l_reg, r_reg);
            return sub_res.toVal();
        }

        // order comparisons are only allowed on numbers
        switch (node.op) {
            .in, .equal, .not_equal => {},
            else => {
                try l_val.checkNum(self, node.lhs.firstToken());
                try r_val.checkNum(self, node.rhs.firstToken());
            },
        }

        const ret_val: Value = switch (node.op) {
            .less_than => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() < r_val.getNum()
                else
                    l_val.int < r_val.int,
            },
            .less_than_equal => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() <= r_val.getNum()
                else
                    l_val.int <= r_val.int,
            },
            .greater_than => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() > r_val.getNum()
                else
                    l_val.int > r_val.int,
            },
            .greater_than_equal => .{
                .Bool = if (needNum(l_val, r_val))
                    l_val.getNum() >= r_val.getNum()
                else
                    l_val.int >= r_val.int,
            },
            .equal, .not_equal => blk: {
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
                const copy = if (node.op == .equal) eql else !eql;
                break :blk Value{ .Bool = copy };
            },
            .in => .{
                .Bool = switch (l_val) {
                    .str => mem.indexOf(
                        u8,
                        try l_val.getStr(self, node.lhs.firstToken()),
                        try r_val.getStr(self, node.rhs.firstToken()),
                    ) != null,
                    else => return self.reportErr("TODO: range without strings", node.lhs.firstToken()),
                },
            },
            else => unreachable,
        };
        return ret_val.maybeRt(self, res);
    }

    fn genBoolInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var l_val = try self.genNodeNonEmpty(node.lhs, .value);

        if (!l_val.isRt()) {
            const l_bool = try l_val.getBool(self, node.lhs.firstToken());
            if (node.op == .bool_and) {
                if (!l_bool) return l_val;
            } else {
                if (l_bool) return l_val;
            }
            return self.genNodeNonEmpty(node.rhs, res);
        }

        const sub_res = res.toRt(self);
        const l_reg = l_val.getRt();
        try self.emitDouble(.move_double, sub_res.rt, l_reg);

        const rhs_skip = if (node.op == .bool_and)
            try self.emitJump(.jump_false, l_reg)
        else
            try self.emitJump(.jump_true, l_reg);

        _ = try self.genNodeNonEmpty(node.rhs, sub_res);
        self.finishJump(rhs_skip);

        return sub_res.toVal();
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

            try self.emitTriple(switch (node.op) {
                .bit_and => .bit_and_triple,
                .bit_or => .bit_or_triple,
                .bit_xor => .bit_xor_triple,
                .l_shift => .l_shift_triple,
                .r_shift => .r_shift_triple,
                else => unreachable,
            }, sub_res.rt, l_reg, r_reg);
            return sub_res.toVal();
        }
        const l_int = try l_val.getInt(self, node.lhs.firstToken());
        const r_int = try r_val.getInt(self, node.rhs.firstToken());

        const ret_val: Value = switch (node.op) {
            .bit_and => .{ .int = l_int & r_int },
            .bit_or => .{ .int = l_int | r_int },
            .bit_xor => .{ .int = l_int ^ r_int },
            .l_shift => blk: {
                if (r_int < 0)
                    return self.reportErr("shift by negative amount", node.rhs.firstToken());
                const val = if (r_int > std.math.maxInt(u6)) 0 else l_int << @intCast(u6, r_int);
                break :blk Value{ .int = val };
            },
            .r_shift => blk: {
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

        const lval_kind = if (self.tokens[node.let_const].id == .Keyword_let)
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
                        try self.emitDouble(.copy_double, copy_reg, reg);
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
                        try self.emitDouble(.copy_double, sym.reg, val.getRt());
                    } else if (val.isRt()) {
                        try self.emitDouble(.move_double, sym.reg, val.getRt());
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
            try self.emitDouble(if (sym.mutable) .move_double else .copy_double, res.rt, sym.reg);
            return res.toVal();
        }
        return Value{ .ref = sym.reg };
    }

    fn genThis(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        // `this` cannot be assigned to
        try res.notLval(self, node.tok);

        const sub_res = res.toRt(self);
        try self.emitSingle(.load_this_single, sub_res.rt);
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

        try self.emitOff(.import_off, sub_res.rt, str_loc);
        return sub_res.toVal();
    }

    fn genError(self: *Compiler, node: *Node.Error, res: Result) Error!Value {
        if (res == .lval) switch (res.lval) {
            .Const, .let, .assign => |val| {
                if (!val.isRt()) {
                    return self.reportErr("expected an error", node.base.firstToken());
                }
                if (node.capture == null) {
                    return self.reportErr("expected a capture", node.base.firstToken());
                }
                const unwrap_reg = self.registerAlloc();
                try self.emitDouble(.unwrap_error_double, unwrap_reg, val.getRt());
                const r_val = Value{ .rt = unwrap_reg };
                const l_val = try self.genNode(node.capture.?, switch (res.lval) {
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
        const val = if (node.capture) |some|
            try self.genNodeNonEmpty(some, .value)
        else
            Value{ .none = {} };

        const sub_res = res.toRt(self);
        const reg = try val.toRt(self);
        defer val.free(self, reg);

        try self.emitDouble(.build_error_double, sub_res.rt, reg);
        return sub_res.toVal();
    }

    fn genTagged(self: *Compiler, node: *Node.Tagged, res: Result) Error!Value {
        if (res == .lval) switch (res.lval) {
            .Const, .let, .assign => |val| {
                if (!val.isRt()) {
                    return self.reportErr("expected a tagged value", node.base.firstToken());
                }
                if (node.capture == null) {
                    return self.reportErr("expected a capture", node.base.firstToken());
                }
                const str_loc = try self.putString(self.tokenSlice(node.name));
                const unwrap_reg = self.registerAlloc();
                try self.emitDouble(.unwrap_tagged, unwrap_reg, val.getRt());
                try self.code.append(.{
                    .bare = str_loc,
                });
                const r_val = Value{ .rt = unwrap_reg };
                const l_val = try self.genNode(node.capture.?, switch (res.lval) {
                    .Const => Result{ .lval = .{ .Const = &r_val } },
                    .let => Result{ .lval = .{ .let = &r_val } },
                    .assign => Result{ .lval = .{ .assign = &r_val } },
                    else => unreachable,
                });
                std.debug.assert(l_val == .empty);
                return Value.empty;
            },
            .aug_assign => {
                return self.reportErr("invalid left hand side to augmented assignment", node.at);
            },
        };

        const sub_res = res.toRt(self);
        const str_loc = try self.putString(self.tokenSlice(node.name));

        try self.code.append(.{
            .tagged = .{
                .res = sub_res.rt,
                .kind = if (node.capture == null) .none else .some,
                .arg = if (node.capture) |some| blk: {
                    const val = try self.genNodeNonEmpty(some, .value);
                    const reg = try val.toRt(self);
                    defer val.free(self, reg);
                    break :blk reg;
                } else 0,
            },
        });
        try self.code.append(.{
            .bare = str_loc,
        });
        return sub_res.toVal();
    }

    fn genRange(self: *Compiler, node: *Node.Range, res: Result) Error!Value {
        if (res == .lval) switch (res.lval) {
            .Const, .let, .assign => |val| {
                if (!val.isRt()) {
                    return self.reportErr("expected a range", node.base.firstToken());
                }
                return self.reportErr("TODO: range destructure", node.base.firstToken());
            },
            .aug_assign => {
                return self.reportErr("invalid left hand side to augmented assignment", node.base.firstToken());
            },
        };

        const sub_res = res.toRt(self);
        const start = try self.genRangePart(node.start);
        const end = try self.genRangePart(node.end);
        const step = try self.genRangePart(node.step);
        try self.code.append(.{
            .range = .{
                .res = sub_res.rt,
                .start = start.val,
                .end = end.val,
            },
        });
        try self.code.append(.{
            .range_cont = .{
                .step = step.val,
                .start_kind = start.kind,
                .end_kind = end.kind,
                .step_kind = step.kind,
            },
        });
        return sub_res.toVal();
    }

    const RangePart = struct {
        kind: bog.Instruction.RangeKind = .default,
        val: RegRef = 0,
    };

    fn genRangePart(self: *Compiler, node: ?*Node) !RangePart {
        var res: RangePart = .{};
        const some = node orelse return res;

        const value = try self.genNodeNonEmpty(some, .value);
        if (value.isRt()) {
            res.kind = .reg;
            res.val = value.getRt();
            return res;
        }

        const int = try value.getInt(self, some.firstToken());
        switch (int) {
            0...std.math.maxInt(RegRef) => {
                res.kind = .value;
                res.val = @intCast(RegRef, int);
            },
            else => {
                res.kind = .reg;
                res.val = try value.toRt(self);
            },
        }
        return res;
    }

    fn genFormatString(self: *Compiler, node: *Node.FormatString, res: Result) Error!Value {
        // transform f"foo{255:X}bar" into "foo{X}bar".format((255,))
        var buf = std.ArrayList(u8).init(self.errors.list.allocator);
        defer buf.deinit();

        var i: usize = 0;
        for (node.format) |str| {
            var slice = self.tokenSlice(str);
            if (slice[0] == 'f') slice = slice[2..];
            if (slice[0] == ':') slice = slice[1..];
            if (self.tokens[str].id == .FormatEnd) slice = slice[0 .. slice.len - 1];

            try buf.ensureCapacity(buf.capacity + slice.len);
            buf.expandToCapacity();
            i += try self.parseStrExtra(str, slice, buf.items[i..]);
        }
        const format_string = try self.arena.dupe(u8, buf.items[0..i]);

        // format_reg = "formatstring".format
        const sub_res = res.toRt(self);
        const format_reg = try (Value{ .str = format_string }).toRt(self);
        const format_member_str = try (Value{ .str = "format" }).toRt(self);
        try self.emitTriple(.get_triple, format_reg, format_reg, format_member_str);
        defer self.registerFree(format_member_str);

        // arg_reg = (args...)
        const arg_reg = format_member_str;
        try self.emitOff(.build_tuple_off, arg_reg, @intCast(u32, node.args.len));

        // prepare registers
        const index_reg = self.registerAlloc();
        defer self.registerFree(index_reg);
        const result_reg = self.registerAlloc();
        defer self.registerFree(result_reg);

        var index = Value{ .int = 0 };
        for (node.args) |arg| {
            _ = try self.genNode(arg, .{ .rt = result_reg });

            try self.makeRuntime(index_reg, index);
            try self.emitTriple(.set_triple, arg_reg, index_reg, result_reg);
            index.int += 1;
        }

        // sub_res.rt = format_reg(arg_reg)
        try self.code.append(.{
            .call = .{
                .res = sub_res.rt,
                .func = format_reg,
                .first = arg_reg,
            },
        });
        try self.code.append(.{ .bare = 1 });
        return sub_res.toVal();
    }

    fn genTry(self: *Compiler, node: *Node.Try, res: Result) Error!Value {
        const sub_res = switch (res) {
            .rt, .discard => res,
            .value => Result{
                // value is only known at runtime
                .rt = self.registerAlloc(),
            },
            .lval => unreachable,
        };

        var try_scope = Scope.Try{
            .base = .{
                .id = .try_catch,
                .parent = self.cur_scope,
                .syms = Symbol.List.init(self.errors.list.allocator),
            },
            .jumps = Scope.Loop.BreakList.init(self.errors.list.allocator),
            .err_reg = self.registerAlloc(),
        };
        self.cur_scope = &try_scope.base;
        defer {
            try_scope.base.syms.deinit();
            try_scope.jumps.deinit();
        }

        const expr_val = try self.genNode(node.expr, sub_res);
        if (sub_res != .rt and expr_val.isRt()) {
            try self.emitSingle(.discard_single, expr_val.getRt());
        }
        // no longer in try scope
        self.cur_scope = try_scope.base.parent.?;

        if (try_scope.jumps.items.len == 0) {
            // no possible error to be handled
            return sub_res.toVal();
        }

        for (try_scope.jumps.items) |jump| {
            self.finishJump(jump);
        }
        try_scope.jumps.items.len = 0;

        // if no error jump over all catchers
        try try_scope.jumps.append(try self.emitJump(.jump_not_error, try_scope.err_reg));
        // otherwise unwrap the error
        try self.emitDouble(.unwrap_error_double, try_scope.err_reg, try_scope.err_reg);

        var capture_scope = Scope{
            .id = .capture,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.errors.list.allocator),
        };
        self.cur_scope = &capture_scope;
        defer {
            capture_scope.syms.deinit();
            self.cur_scope = capture_scope.parent.?;
        }

        const capture_reg = self.registerAlloc();
        var seen_catch_all = false;
        for (node.catches) |catcher, catcher_i| {
            if (seen_catch_all) {
                return self.reportErr("additional handlers after catch-all handler", catcher.firstToken());
            }
            const catch_node = @fieldParentPtr(Node.Catch, "base", catcher);

            var catch_skip: ?usize = null;
            if (catch_node.capture) |some| {
                if (catch_node.let_const) |tok| {
                    seen_catch_all = true;

                    const lval_res = if (self.tokens[tok].id == .Keyword_let)
                        Result{ .lval = .{ .let = &Value{ .rt = try_scope.err_reg } } }
                    else
                        Result{ .lval = .{ .Const = &Value{ .rt = try_scope.err_reg } } };

                    assert((try self.genNode(some, lval_res)) == .empty);
                } else {
                    _ = try self.genNodeNonEmpty(some, .{ .rt = capture_reg });
                    // if not equal to the error value jump over this handler
                    try self.emitTriple(.equal_triple, capture_reg, capture_reg, try_scope.err_reg);
                    catch_skip = try self.emitJump(.jump_false, capture_reg);
                }
            } else {
                seen_catch_all = true;
            }

            const catch_res = try self.genNode(catch_node.expr, sub_res);
            if (sub_res != .rt and catch_res.isRt()) {
                try self.emitSingle(.discard_single, catch_res.getRt());
            }

            // exit this hanler (unless it's the last one)
            if (catcher_i + 1 != node.catches.len) {
                try try_scope.jumps.append(try self.emitJump(.jump, null));
            }

            // jump over this handler if the value doesn't match
            if (catch_skip) |some| {
                self.finishJump(some);
            }
        }

        // return uncaught errors
        if (!seen_catch_all) {
            try self.emitSingle(.return_single, try_scope.err_reg);
        }

        // exit try-catch
        for (try_scope.jumps.items) |jump| {
            self.finishJump(jump);
        }
        return sub_res.toVal();
    }

    fn genMatch(self: *Compiler, node: *Node.Match, res: Result) Error!Value {
        const sub_res = switch (res) {
            .rt, .discard => res,
            .value => Result{
                // value is only known at runtime
                .rt = self.registerAlloc(),
            },
            .lval => unreachable,
        };

        const cond_val = try self.genNodeNonEmpty(node.expr, .value);
        const cond_reg = try cond_val.toRt(self);

        var capture_scope = Scope{
            .id = .capture,
            .parent = self.cur_scope,
            .syms = Symbol.List.init(self.errors.list.allocator),
        };
        self.cur_scope = &capture_scope;
        defer {
            capture_scope.syms.deinit();
            self.cur_scope = capture_scope.parent.?;
        }

        var jumps = Scope.Loop.BreakList.init(self.errors.list.allocator);
        defer jumps.deinit();

        var case_reg = self.registerAlloc();
        var seen_catch_all = false;
        for (node.cases) |uncasted_case, case_i| {
            if (seen_catch_all) {
                return self.reportErr("additional cases after catch-all case", uncasted_case.firstToken());
            }

            var expr: *Node = undefined;
            var case_skip: ?usize = null;

            if (uncasted_case.cast(.MatchCatchAll)) |case| {
                seen_catch_all = true;
                expr = case.expr;
            } else if (uncasted_case.cast(.MatchLet)) |case| {
                seen_catch_all = true;
                expr = case.expr;

                const lval_res = if (self.tokens[case.let_const].id == .Keyword_let)
                    Result{ .lval = .{ .let = &Value{ .rt = cond_reg } } }
                else
                    Result{ .lval = .{ .Const = &Value{ .rt = cond_reg } } };

                assert((try self.genNode(case.capture, lval_res)) == .empty);
            } else if (uncasted_case.cast(.MatchCase)) |case| {
                expr = case.expr;

                if (case.items.len != 1) {
                    return self.reportErr("TODO multi item cases", uncasted_case.firstToken());
                }

                _ = try self.genNodeNonEmpty(case.items[0], .{ .rt = case_reg });
                // if not equal to the error value jump over this handler
                try self.emitTriple(.equal_triple, case_reg, case_reg, cond_reg);
                case_skip = try self.emitJump(.jump_false, case_reg);
            } else unreachable;

            const case_res = try self.genNode(expr, sub_res);
            if (sub_res != .rt and case_res.isRt()) {
                try self.emitSingle(.discard_single, case_res.getRt());
            }

            // exit match (unless it's this is the last case)
            if (case_i + 1 != node.cases.len) {
                try jumps.append(try self.emitJump(.jump, null));
            }

            // jump over this case if the value doesn't match
            if (case_skip) |some| {
                self.finishJump(some);
            }
        }

        // result in none if no matches
        if (!seen_catch_all and sub_res == .rt) {
            try self.code.append(.{
                .primitive = .{ .res = sub_res.rt, .kind = .none },
            });
        }

        // exit match
        for (jumps.items) |jump| {
            self.finishJump(jump);
        }
        return sub_res.toVal();
    }

    fn addLineInfo(self: *Compiler, node: *Node) !void {
        const tok = self.tokens[node.firstToken()];

        try self.code.append(.{ .op = .{ .op = .line_info } });
        try self.code.append(.{ .bare = tok.start });
    }

    fn finishJump(self: *Compiler, jump_addr: usize) void {
        self.code.items[jump_addr] = .{
            .bare = @intCast(u32, self.code.items.len - jump_addr),
        };
    }

    fn getLastNode(self: *Compiler, first_node: *Node) *Node {
        var node = first_node;
        while (true) switch (node.id) {
            .Grouped => node = @fieldParentPtr(Node.Grouped, "base", node).expr,
            else => return node,
        };
    }

    fn tokenSlice(self: *Compiler, token: TokenIndex) []const u8 {
        const tok = self.tokens[token];
        return self.source[tok.start..tok.end];
    }

    fn parseStr(self: *Compiler, tok: TokenIndex) ![]u8 {
        var slice = self.tokenSlice(tok);
        slice = slice[1 .. slice.len - 1];
        var buf = try self.arena.alloc(u8, slice.len);
        return buf[0..try self.parseStrExtra(tok, slice, buf)];
    }

    fn parseStrExtra(self: *Compiler, tok: TokenIndex, slice: []const u8, buf: []u8) !usize {
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
        return i;
    }

    fn reportErr(self: *Compiler, msg: []const u8, tok: TokenIndex) Error {
        try self.errors.add(msg, self.tokens[tok].start, .err);
        return error.CompileError;
    }
};
