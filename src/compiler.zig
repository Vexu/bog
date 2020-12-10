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

pub fn compile(gpa: *Allocator, source: []const u8, errors: *Errors) (Compiler.Error || bog.Parser.Error || bog.Tokenizer.Error)!*bog.Module {
    var tree = try bog.parse(gpa, source, errors);
    defer tree.deinit();

    // TODO reduce usage of arenas
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const arena = &arena_state.allocator;

    var module = Compiler.Fn{
        .captures = undefined,
        .code = Compiler.Code.init(gpa),
    };
    defer module.code.deinit();

    var compiler = Compiler{
        .gpa = gpa,
        .errors = errors,
        .tokens = tree.tokens,
        .source = source,
        .arena = arena,
        .func = &module,
        .module_code = Compiler.Code.init(gpa),
        .scopes = std.ArrayList(Compiler.Scope).init(gpa),
        .strings = std.ArrayList(u8).init(gpa),
        .string_interner = std.StringHashMap(u32).init(gpa),
    };
    defer compiler.deinit();

    try compiler.scopes.append(.{ .module = &module });

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
    try compiler.module_code.appendSlice(module.code.items);
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
    repl.compiler.tokens = repl.tokenizer.tokens.items;
    repl.compiler.source = repl.buffer.items;
    const start_len = repl.compiler.module_code.items.len;

    try repl.compiler.autoForwardDecl(node);
    try repl.compiler.addLineInfo(node);
    const val = try repl.compiler.genNode(node, .discard);
    if (val != .empty) {
        const reg = try val.toRt(&repl.compiler);
        defer val.free(&repl.compiler, reg);

        try repl.compiler.emitSingle(.discard_single, reg);
    }
    try repl.compiler.module_code.appendSlice(repl.compiler.func.code.items);
    repl.compiler.func.code.items.len = 0;

    return bog.Module{
        .name = "<stdin>",
        .code = repl.compiler.module_code.items,
        .strings = repl.compiler.strings.items,
        .entry = @intCast(u32, start_len),
    };
}

pub const Compiler = struct {
    tokens: []const bog.Token,
    source: []const u8,
    errors: *Errors,
    gpa: *Allocator,
    arena: *Allocator,
    scopes: std.ArrayList(Scope),
    func: *Fn,
    module_code: Code,
    strings: std.ArrayList(u8),
    string_interner: std.StringHashMap(u32),

    pub fn deinit(self: *Compiler) void {
        self.scopes.deinit();
        self.strings.deinit();
        self.module_code.deinit();
        self.string_interner.deinit();
    }

    pub const Code = std.ArrayList(bog.Instruction);

    pub const Fn = struct {
        code: Code,
        captures: std.ArrayList(Symbol),
        regs: std.PackedIntArray(u1, 256) = .{
            .bytes = [_]u8{0} ** 32,
        },
        // index of first free register
        free_index: u8 = 0,

        pub fn regAlloc(self: *Fn) !RegRef {
            var i: u32 = self.free_index;
            while (i < 256) : (i += 1) {
                if (self.regs.get(i) == 0) {
                    self.regs.set(i, 1);
                    self.free_index = @truncate(u8, i + 1);
                    return @truncate(u8, i);
                }
            }
            // return error.RanOutOfRegisters;
            @panic("TODO RanOutOfRegisters");
        }

        pub fn regFree(self: *Fn, reg: RegRef) void {
            self.regs.set(reg, 0);
            self.free_index = std.math.min(self.free_index, reg);
        }

        fn regAllocN(self: *Fn, n: usize) !RegRef {
            var i = self.free_index;
            outer: while (i + n < 256) : (i += 1) {
                var j: usize = 0;
                while (j < n) : (j += 1) {
                    if (self.regs.get(i + j) == 1) continue :outer;
                }
                j = 0;
                while (j < n) : (j += 1) {
                    self.regs.set(i + j, 1);
                }
                return i;
            }
            @panic("TODO RanOutOfRegisters");
        }

        fn regFreeN(self: *Fn, start: usize, n: usize) void {
            var i: usize = 0;
            while (i < n) : (i += 1) {
                self.regs.set(start + i, 0);
            }
        }
    };

    const Symbol = struct {
        name: []const u8,
        // val: Value,
        reg: RegRef,
    };

    const Loop = struct {
        breaks: BreakList,
        cond_begin: u32,

        const BreakList = std.ArrayList(u32);
    };

    pub const Scope = union(enum) {
        module: *Fn,
        func: *Fn,
        loop: *Loop,
        constant: Symbol,
        mut: Symbol,
        forward_decl: Symbol,
        try_catch: *Try,
    };

    const Try = struct {
        jumps: JumpList,
        err_reg: RegRef,
    };

    const JumpList = std.ArrayList(u32);

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
            captures: []Symbol,
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
                self.func.regFree(reg);
            }
        }

        fn toRt(val: Value, self: *Compiler) !RegRef {
            switch (val) {
                .rt, .ref => |r| return r,
                .empty => unreachable,
                else => {
                    const reg = try self.func.regAlloc();
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

    pub const Error = error{CompileError} || Allocator.Error;

    fn getTry(self: *Compiler) ?*Try {
        var i = self.scopes.items.len;
        while (true) {
            i -= 1;
            const scope = self.scopes.items[i];
            switch (scope) {
                .try_catch => |t| return t,
                .func, .module => return null,
                else => {},
            }
        }
    }

    fn clearScopes(self: *Compiler, scope_count: usize) void {
        var i = self.scopes.items.len;
        while (i != scope_count) {
            i -= 1;
            const scope = self.scopes.items[i];
            switch (scope) {
                .constant => |sym| self.func.regFree(sym.reg),
                .mut => |sym| self.func.regFree(sym.reg),
                .try_catch, .loop => {},
                else => unreachable,
            }
        }
        self.scopes.items.len = scope_count;
    }

    fn emitSingle(self: *Compiler, op: bog.Op, arg: RegRef) !void {
        try self.func.code.append(.{
            .single = .{
                .op = op,
                .arg = arg,
            },
        });
    }

    fn emitDouble(self: *Compiler, op: bog.Op, res: RegRef, arg: RegRef) !void {
        try self.func.code.append(.{
            .double = .{
                .op = op,
                .res = res,
                .arg = arg,
            },
        });
    }

    fn emitTriple(self: *Compiler, op: bog.Op, res: RegRef, lhs: RegRef, rhs: RegRef) !void {
        try self.func.code.append(.{
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
        try self.func.code.append(.{
            .off = .{
                .op = op,
                .res = res,
                .off = if (long) 0xFFFF else @truncate(u16, off),
            },
        });
        if (long) try self.func.code.append(.{ .bare = off });
    }

    fn emitJump(self: *Compiler, op: bog.Op, arg: ?RegRef) !u32 {
        try self.func.code.append(.{ .jump = .{ .op = op, .arg = arg orelse 0 } });
        try self.func.code.append(undefined);
        return @intCast(u32, self.func.code.items.len - 1);
    }

    fn makeRuntime(self: *Compiler, res: RegRef, val: Value) Error!void {
        return switch (val) {
            .empty => unreachable,
            .ref, .rt => |v| assert(v == res),
            .none => try self.func.code.append(.{
                .primitive = .{ .res = res, .kind = .none },
            }),
            .int => |v| if (v >= std.math.minInt(i15) and v <= std.math.maxInt(i15)) {
                try self.func.code.append(.{ .int = .{ .res = res, .long = false, .arg = @truncate(i15, v) } });
            } else {
                try self.func.code.append(.{ .int = .{ .res = res, .long = true, .arg = 0 } });
                const arr = @bitCast([2]u32, v);
                try self.func.code.append(.{ .bare = arr[0] });
                try self.func.code.append(.{ .bare = arr[1] });
            },
            .num => |v| {
                try self.func.code.append(.{ .single = .{ .op = .const_num, .arg = res } });
                const arr = @bitCast([2]u32, v);
                try self.func.code.append(.{ .bare = arr[0] });
                try self.func.code.append(.{ .bare = arr[1] });
            },
            .Bool => |v| try self.func.code.append(.{ .primitive = .{ .res = res, .kind = if (v) .True else .False } }),
            .str => |v| try self.emitOff(.const_string_off, res, try self.putString(v)),
            .func => |v| {
                try self.func.code.append(.{
                    .func = .{
                        .res = res,
                        .arg_count = v.params,
                        .capture_count = @intCast(u8, v.captures.len),
                    },
                });
                try self.func.code.append(.{ .bare = v.offset });

                for (v.captures) |capture, i| {
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

    fn autoForwardDecl(self: *Compiler, node: *Node) !void {
        if (node.id != .Decl) return;
        const decl = @fieldParentPtr(Node.Decl, "base", node);

        // only forward declarations like
        // `const IDENTIFIER = fn ...`
        if (self.tokens[decl.let_const].id == .Keyword_const and
            decl.capture.id != .Identifier or decl.value.id != .Fn)
            return;

        const ident = @fieldParentPtr(Node.SingleToken, "base", decl.capture);
        const reg = try self.func.regAlloc();
        try self.scopes.append(.{
            .forward_decl = .{
                // .val = undefined,
                .name = self.tokenSlice(ident.tok),
                .reg = reg,
            },
        });
    }

    const RegAndMut = struct {
        reg: RegRef,
        mut: bool,
    };

    fn findSymbol(self: *Compiler, tok: TokenIndex) !RegAndMut {
        const name = self.tokenSlice(tok);
        var i = self.scopes.items.len;

        while (true) {
            i -= 1;
            const item = self.scopes.items[i];
            switch (item) {
                .module => break,
                .func => {
                    @panic("TODO captures");
                },
                .loop, .try_catch => {},
                .constant, .mut, .forward_decl => |sym| if (mem.eql(u8, sym.name, name)) {
                    return RegAndMut{
                        .reg = sym.reg,
                        .mut = item == .mut,
                    };
                },
            }
        }
        return self.reportErr("use of undeclared identifier", tok);
    }

    fn getForwardDecl(self: *Compiler, tok: TokenIndex) ?RegRef {
        const name = self.tokenSlice(tok);
        var i = self.scopes.items.len;
        var in_fn_scope = false;

        while (true) {
            i -= 1;
            const item = self.scopes.items[i];
            switch (item) {
                .module => break,
                .func => in_fn_scope = true,
                .loop, .try_catch => {},
                .constant, .mut => return null,
                .forward_decl => |sym| if (mem.eql(u8, sym.name, name)) {
                    assert(!in_fn_scope);
                    return sym.reg;
                },
            }
        }
        return null;
    }

    const Result = union(enum) {
        /// A runtime value is expected
        rt: RegRef,

        /// A value, runtime or constant, is expected
        value,

        /// No value is expected if some is given it will be discarded
        discard,

        fn toRt(res: Result, compiler: *Compiler) !Result {
            return if (res == .rt) res else .{ .rt = try compiler.func.regAlloc() };
        }

        /// returns .empty if res != .rt
        fn toVal(res: Result) Value {
            return if (res != .rt)
                .empty
            else
                .{ .rt = res.rt };
        }
    };

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

        const sub_res = try res.toRt(self);
        try self.emitOff(.build_map_off, sub_res.rt, @intCast(u32, node.values.len));

        // prepare registers
        const container_reg = sub_res.rt;
        const index_reg = try self.func.regAlloc();
        defer self.func.regFree(index_reg);
        const result_reg = try self.func.regAlloc();
        defer self.func.regFree(result_reg);

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

        const sub_res = try res.toRt(self);
        try self.emitOff(switch (node.base.id) {
            .Tuple => .build_tuple_off,
            .List => .build_list_off,
            else => unreachable,
        }, sub_res.rt, @intCast(u32, node.values.len));

        // prepare registers
        const container_reg = sub_res.rt;
        const index_reg = try self.func.regAlloc();
        defer self.func.regFree(index_reg);
        const result_reg = try self.func.regAlloc();
        defer self.func.regFree(result_reg);

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
        if (node.params.len > max_params) {
            return self.reportErr("too many parameters", node.fn_tok);
        }
        const param_count = @truncate(u8, node.params.len);

        const prev_func = self.func;
        defer self.func = prev_func;

        const scopes = self.scopes.items.len;
        defer self.scopes.items.len = scopes;

        var func = Fn{
            .captures = std.ArrayList(Symbol).init(self.gpa),
            .code = Code.init(self.gpa),
        };
        defer func.captures.deinit();
        defer func.code.deinit();

        try self.scopes.append(.{ .func = &func });
        const old_func = self.func;
        self.func = &func;

        // destructure parameters
        for (node.params) |param| {
            // we checked that there are less than max_params params
            const reg = try func.regAlloc();
            try self.genLval(param, .{
                .mut = &Value{ .rt = reg },
            });
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
            try self.func.code.append(.{ .op = .{ .op = .return_none } });
        } else {
            const reg = try body_val.toRt(self);
            defer body_val.free(self, reg);

            try self.emitSingle(.return_single, reg);
        }

        // reset regs after generating body
        self.func = old_func;

        const offset = @intCast(u32, self.module_code.items.len);
        try self.module_code.appendSlice(func.code.items);

        const ret_val = Value{
            .func = .{
                .params = param_count,
                .offset = offset,
                .captures = try self.arena.dupe(Symbol, func.captures.items),
            },
        };
        return ret_val.maybeRt(self, res);
    }

    fn genBlock(self: *Compiler, node: *Node.Block, res: Result) Error!Value {
        const scope_count = self.scopes.items.len;
        defer self.clearScopes(scope_count);

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
        const scope_count = self.scopes.items.len;
        var if_skip: usize = undefined;

        const cond_val = try self.genNodeNonEmpty(node.cond, .value);
        if (node.capture != null) {
            // TODO handle cond_val.isRt()
            const cond_reg = try cond_val.toRt(self);
            // jump past if_body if cond == .none
            if_skip = try self.emitJump(.jump_none, cond_reg);

            const lval_res = try self.genLval(node.capture.?, if (self.tokens[node.let_const.?].id == .Keyword_let)
                .{ .mut = &Value{ .rt = cond_reg } }
            else
                .{ .constant = &Value{ .rt = cond_reg } });
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
                .rt = try self.func.regAlloc(),
            },
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
        self.clearScopes(scope_count);

        if (node.else_body) |some| {
            const else_val = try self.genNode(some, sub_res);
            if (sub_res != .rt and else_val.isRt()) {
                try self.emitSingle(.discard_single, else_val.getRt());
            }
        } else if (sub_res == .rt) {
            try self.func.code.append(.{
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
                try self.func.code.append(.{ .op = .{ .op = .return_none } });
            }
            return Value{ .empty = {} };
        }

        // find inner most loop
        const loop_scope = blk: {
            var i = self.scopes.items.len;
            while (true) {
                i -= 1;
                const scope = self.scopes.items[i];
                switch (scope) {
                    .module, .func => return self.reportErr(if (node.op == .Continue)
                        "continue outside of loop"
                    else
                        "break outside of loop", node.tok),
                    .loop => |loop| break :blk loop,
                    else => {},
                }
            }
        };
        if (node.op == .Continue) {
            self.func.code.items[try self.emitJump(.jump, null)] = .{
                .bare_signed = @intCast(i32, -@intCast(isize, self.func.code.items.len - loop_scope.cond_begin)),
            };
        } else {
            try loop_scope.breaks.append(try self.emitJump(.jump, null));
        }

        return Value{ .empty = {} };
    }

    fn createListComprehension(self: *Compiler, reg: ?RegRef) !Result {
        const list = reg orelse try self.func.regAlloc();
        try self.emitOff(.build_list_off, list, 0);
        return Result{ .rt = list };
    }

    fn genWhile(self: *Compiler, node: *Node.While, res: Result) Error!Value {
        const sub_res = switch (res) {
            .discard => res,
            .value => try self.createListComprehension(null),
            .rt => |reg| try self.createListComprehension(reg),
        };

        const scope_count = self.scopes.items.len;
        defer self.clearScopes(scope_count);

        var loop = Loop{
            .breaks = Loop.BreakList.init(self.strings.allocator),
            .cond_begin = @intCast(u32, self.func.code.items.len),
        };
        defer loop.breaks.deinit();

        try self.scopes.append(.{ .loop = &loop });

        // beginning of condition
        var cond_jump: ?usize = null;

        const cond_val = try self.genNode(node.cond, .value);
        if (node.capture != null) {
            // TODO handle cond_val.isRt()
            const cond_reg = try cond_val.toRt(self);
            // jump past exit loop if cond == .none
            cond_jump = try self.emitJump(.jump_none, cond_reg);

            try self.genLval(node.capture.?, if (self.tokens[node.let_const.?].id == .Keyword_let)
                .{ .mut = &Value{ .rt = cond_reg } }
            else
                .{ .constant = &Value{ .rt = cond_reg } });
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
                defer self.func.regFree(reg);
                try self.emitDouble(.append_double, list, reg);
            },
            else => unreachable,
        }

        // jump back to condition
        const end = try self.emitJump(.jump, null);
        self.func.code.items[end] = .{
            .bare_signed = @truncate(i32, -@intCast(isize, end - loop.cond_begin)),
        };

        // exit loop if cond == false
        if (cond_jump) |some| {
            self.finishJump(some);
        }
        for (loop.breaks.items) |some| {
            self.finishJump(some);
        }

        return sub_res.toVal();
    }

    fn genFor(self: *Compiler, node: *Node.For, res: Result) Error!Value {
        const sub_res = switch (res) {
            .discard => res,
            .value => try self.createListComprehension(null),
            .rt => |reg| try self.createListComprehension(reg),
        };

        const scope_count = self.scopes.items.len;
        defer self.clearScopes(scope_count);

        var loop = Loop{
            .breaks = Loop.BreakList.init(self.strings.allocator),
            .cond_begin = @intCast(u32, self.func.code.items.len),
        };
        defer loop.breaks.deinit();

        try self.scopes.append(.{ .loop = &loop });

        const cond_val = try self.genNode(node.cond, .value);
        if (!cond_val.isRt() and cond_val != .str)
            return self.reportErr("expected iterable value", node.cond.firstToken());

        const cond_reg = try cond_val.toRt(self);
        defer cond_val.free(self, cond_reg);

        const iter_reg = try self.func.regAlloc();
        defer self.func.regFree(iter_reg);

        // initialize the iterator
        try self.emitDouble(.iter_init_double, iter_reg, cond_reg);

        const iter_val_reg = try self.func.regAlloc();
        defer self.func.regFree(iter_val_reg);

        // loop condition
        loop.cond_begin = @intCast(u32, self.func.code.items.len);

        // iter_next is fused with a jump_none
        try self.emitDouble(.iter_next_double, iter_val_reg, iter_reg);
        const exit_jump = self.func.code.items.len;
        try self.func.code.append(.{ .bare = 0 });

        if (node.capture != null) {
            try self.genLval(node.capture.?, if (self.tokens[node.let_const.?].id == .Keyword_let)
                .{ .mut = &Value{ .rt = iter_val_reg } }
            else
                .{ .constant = &Value{ .rt = iter_val_reg } });
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
                defer self.func.regFree(reg);
                try self.emitDouble(.append_double, list, reg);
            },
            else => unreachable,
        }

        // jump to the start of the loop
        const end = try self.emitJump(.jump, null);
        self.func.code.items[end] = .{
            .bare_signed = @truncate(i32, -@intCast(isize, end - loop.cond_begin)),
        };

        // exit loop when IterNext results in None
        self.finishJump(exit_jump);

        for (loop.breaks.items) |some| {
            self.finishJump(some);
        }

        return sub_res.toVal();
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) Error!Value {
        const rhs_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (rhs_val.isRt()) {
            const op_id: bog.Op = switch (node.op) {
                .bool_not => .bool_not_double,
                .bit_not => .bit_not_double,
                .minus => .negate_double,
                // TODO should unary + be a no-op
                .plus => return rhs_val,
            };
            const reg = rhs_val.getRt();
            defer rhs_val.free(self, reg);

            const sub_res = try res.toRt(self);
            try self.emitDouble(op_id, sub_res.rt, reg);
            return sub_res.toVal();
        }
        const ret_val: Value = switch (node.op) {
            .bool_not => .{ .Bool = !try rhs_val.getBool(self, node.rhs.firstToken()) },
            .bit_not => .{ .int = ~try rhs_val.getInt(self, node.rhs.firstToken()) },
            .minus => blk: {
                try rhs_val.checkNum(self, node.rhs.firstToken());
                if (rhs_val == .int) {
                    // TODO check for overflow
                    break :blk Value{ .int = -rhs_val.int };
                } else {
                    break :blk Value{ .num = -rhs_val.num };
                }
            },
            .plus => blk: {
                try rhs_val.checkNum(self, node.rhs.firstToken());
                break :blk rhs_val;
            },
        };
        return ret_val.maybeRt(self, res);
    }

    fn genTypeInfix(self: *Compiler, node: *Node.TypeInfix, res: Result) Error!Value {
        const lhs = try self.genNodeNonEmpty(node.lhs, .value);

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

        if (lhs.isRt()) {
            const sub_res = try res.toRt(self);
            const reg = lhs.getRt();
            defer lhs.free(self, reg);

            try self.func.code.append(.{
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
                    .int = switch (lhs) {
                        .int => |val| val,
                        .num => |val| @floatToInt(i64, val),
                        .Bool => |val| @boolToInt(val),
                        .str => |str| util.parseInt(str) catch
                            return self.reportErr("invalid cast to int", node.lhs.firstToken()),
                        else => return self.reportErr("invalid cast to int", node.lhs.firstToken()),
                    },
                },
                .num => Value{
                    .num = switch (lhs) {
                        .num => |val| val,
                        .int => |val| @intToFloat(f64, val),
                        .Bool => |val| @intToFloat(f64, @boolToInt(val)),
                        .str => |str| util.parseNum(str) catch
                            return self.reportErr("invalid cast to num", node.lhs.firstToken()),
                        else => return self.reportErr("invalid cast to num", node.lhs.firstToken()),
                    },
                },
                .bool => Value{
                    .Bool = switch (lhs) {
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
                    .str = switch (lhs) {
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
                    .none => lhs == .none,
                    .int => lhs == .int,
                    .num => lhs == .num,
                    .bool => lhs == .Bool,
                    .str => lhs == .str,
                    else => false,
                },
            },
        };

        return ret_val.maybeRt(self, res);
    }

    fn genSuffix(self: *Compiler, node: *Node.Suffix, res: Result) Error!Value {
        const lhs_val = try self.genNode(node.lhs, .value);
        if (lhs_val != .str and lhs_val != .func and !lhs_val.isRt()) {
            return self.reportErr("invalid left hand side to suffix op", node.lhs.firstToken());
        }
        const l_reg = try lhs_val.toRt(self);
        defer if (node.op == .call) {
            // member access and subscript will set `this` so we can't free l_reg
            lhs_val.free(self, l_reg);
        };

        const index_val = switch (node.op) {
            .call => |args| {
                if (args.len > max_params) {
                    return self.reportErr("too many arguments", node.l_tok);
                }

                var start = try self.func.regAllocN(args.len);

                var i = start;
                for (args) |arg| {
                    _ = try self.genNode(arg, .{ .rt = i });
                    i += 1;
                }
                self.func.regFreeN(start, args.len);

                const sub_res = try res.toRt(self);
                try self.func.code.append(.{
                    .call = .{
                        .res = sub_res.rt,
                        .func = l_reg,
                        .first = start,
                    },
                });
                try self.func.code.append(.{ .bare = @truncate(u8, args.len) });

                if (self.getTry()) |try_scope| {
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

        const sub_res = try res.toRt(self);
        try self.emitTriple(.get_triple, sub_res.rt, l_reg, index_reg);
        return sub_res.toVal();
    }

    fn genInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
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
                var lhs_val = try self.genNodeNonEmpty(node.lhs, .value);
                var rhs_val = try self.genNodeNonEmpty(node.rhs, .value);

                if (lhs_val.isRt() or rhs_val.isRt()) {
                    const l_reg = try lhs_val.toRt(self);
                    const r_reg = try rhs_val.toRt(self);
                    defer rhs_val.free(self, r_reg);

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

                const l_str = try lhs_val.getStr(self, node.lhs.firstToken());
                const r_str = try rhs_val.getStr(self, node.rhs.firstToken());

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
        const rhs_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (node.op == .assign) {
            try self.genLval(node.lhs, .{ .assign = &rhs_val });
            return .empty;
        }

        var lhs_val: Value = .none;
        try self.genLval(node.lhs, .{ .aug_assign = &lhs_val });
        if (!rhs_val.isRt()) switch (node.op) {
            .add_assign,
            .sub_assign,
            .mul_assign,
            .pow_assign,
            .div_assign,
            .div_floor_assign,
            .mod_assign,
            => try rhs_val.checkNum(self, node.rhs.firstToken()),

            .l_shift_assign,
            .r_shift_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_x_or_assign,
            => _ = try rhs_val.getInt(self, node.rhs.firstToken()),
            else => unreachable,
        };

        const reg = try rhs_val.toRt(self);
        defer rhs_val.free(self, reg);

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
        }, lhs_val.getRt(), lhs_val.getRt(), reg);
        return Value.empty;
    }

    fn needNum(a: Value, b: Value) bool {
        return a == .num or b == .num;
    }

    fn genNumericInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var lhs_val = try self.genNodeNonEmpty(node.lhs, .value);
        var rhs_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (rhs_val.isRt() or lhs_val.isRt()) {
            const sub_res = try res.toRt(self);

            const l_reg = try lhs_val.toRt(self);
            const r_reg = try rhs_val.toRt(self);
            defer {
                rhs_val.free(self, r_reg);
                lhs_val.free(self, l_reg);
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
        try lhs_val.checkNum(self, node.lhs.firstToken());
        try rhs_val.checkNum(self, node.rhs.firstToken());

        // TODO makeRuntime if overflow
        const ret_val = switch (node.op) {
            .add => blk: {
                if (needNum(lhs_val, rhs_val)) {
                    break :blk Value{ .num = lhs_val.getNum() + rhs_val.getNum() };
                }
                break :blk Value{ .int = lhs_val.int + rhs_val.int };
            },
            .sub => blk: {
                if (needNum(lhs_val, rhs_val)) {
                    break :blk Value{ .num = lhs_val.getNum() - rhs_val.getNum() };
                }
                break :blk Value{ .int = lhs_val.int - rhs_val.int };
            },
            .mul => blk: {
                if (needNum(lhs_val, rhs_val)) {
                    break :blk Value{ .num = lhs_val.getNum() * rhs_val.getNum() };
                }
                break :blk Value{ .int = lhs_val.int * rhs_val.int };
            },
            .div => Value{ .num = lhs_val.getNum() / rhs_val.getNum() },
            .div_floor => blk: {
                if (needNum(lhs_val, rhs_val)) {
                    break :blk Value{ .int = @floatToInt(i64, @divFloor(lhs_val.getNum(), rhs_val.getNum())) };
                }
                break :blk Value{ .int = @divFloor(lhs_val.int, rhs_val.int) };
            },
            .mod => blk: {
                if (needNum(lhs_val, rhs_val)) {
                    break :blk Value{ .num = @rem(lhs_val.getNum(), rhs_val.getNum()) };
                }
                break :blk Value{ .int = std.math.rem(i64, lhs_val.int, rhs_val.int) catch @panic("TODO") };
            },
            .pow => blk: {
                if (needNum(lhs_val, rhs_val)) {
                    break :blk Value{ .num = std.math.pow(f64, lhs_val.getNum(), rhs_val.getNum()) };
                }
                break :blk Value{
                    .int = std.math.powi(i64, lhs_val.int, rhs_val.int) catch
                        return self.reportErr("TODO integer overflow", node.tok),
                };
            },
            else => unreachable,
        };

        return ret_val.maybeRt(self, res);
    }

    fn genComparisonInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var lhs_val = try self.genNodeNonEmpty(node.lhs, .value);
        var rhs_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (rhs_val.isRt() or lhs_val.isRt()) {
            const sub_res = try res.toRt(self);

            const l_reg = try lhs_val.toRt(self);
            const r_reg = try rhs_val.toRt(self);
            defer {
                rhs_val.free(self, r_reg);
                lhs_val.free(self, l_reg);
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
                try lhs_val.checkNum(self, node.lhs.firstToken());
                try rhs_val.checkNum(self, node.rhs.firstToken());
            },
        }

        const ret_val: Value = switch (node.op) {
            .less_than => .{
                .Bool = if (needNum(lhs_val, rhs_val))
                    lhs_val.getNum() < rhs_val.getNum()
                else
                    lhs_val.int < rhs_val.int,
            },
            .less_than_equal => .{
                .Bool = if (needNum(lhs_val, rhs_val))
                    lhs_val.getNum() <= rhs_val.getNum()
                else
                    lhs_val.int <= rhs_val.int,
            },
            .greater_than => .{
                .Bool = if (needNum(lhs_val, rhs_val))
                    lhs_val.getNum() > rhs_val.getNum()
                else
                    lhs_val.int > rhs_val.int,
            },
            .greater_than_equal => .{
                .Bool = if (needNum(lhs_val, rhs_val))
                    lhs_val.getNum() >= rhs_val.getNum()
                else
                    lhs_val.int >= rhs_val.int,
            },
            .equal, .not_equal => blk: {
                const eql = switch (lhs_val) {
                    .none => |a_val| switch (rhs_val) {
                        .none => true,
                        else => false,
                    },
                    .int => |a_val| switch (rhs_val) {
                        .int => |b_val| a_val == b_val,
                        .num => |b_val| @intToFloat(f64, a_val) == b_val,
                        else => false,
                    },
                    .num => |a_val| switch (rhs_val) {
                        .int => |b_val| a_val == @intToFloat(f64, b_val),
                        .num => |b_val| a_val == b_val,
                        else => false,
                    },
                    .Bool => |a_val| switch (rhs_val) {
                        .Bool => |b_val| a_val == b_val,
                        else => false,
                    },
                    .str => |a_val| switch (rhs_val) {
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
                .Bool = switch (lhs_val) {
                    .str => mem.indexOf(
                        u8,
                        try lhs_val.getStr(self, node.lhs.firstToken()),
                        try rhs_val.getStr(self, node.rhs.firstToken()),
                    ) != null,
                    else => return self.reportErr("TODO: range without strings", node.lhs.firstToken()),
                },
            },
            else => unreachable,
        };
        return ret_val.maybeRt(self, res);
    }

    fn genBoolInfix(self: *Compiler, node: *Node.Infix, res: Result) Error!Value {
        var lhs_val = try self.genNodeNonEmpty(node.lhs, .value);

        if (!lhs_val.isRt()) {
            const l_bool = try lhs_val.getBool(self, node.lhs.firstToken());
            if (node.op == .bool_and) {
                if (!l_bool) return lhs_val;
            } else {
                if (l_bool) return lhs_val;
            }
            return self.genNodeNonEmpty(node.rhs, res);
        }

        const sub_res = try res.toRt(self);
        const l_reg = lhs_val.getRt();
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
        var lhs_val = try self.genNodeNonEmpty(node.lhs, .value);
        var rhs_val = try self.genNodeNonEmpty(node.rhs, .value);

        if (lhs_val.isRt() or rhs_val.isRt()) {
            const sub_res = try res.toRt(self);

            const l_reg = try lhs_val.toRt(self);
            const r_reg = try rhs_val.toRt(self);
            defer {
                rhs_val.free(self, r_reg);
                lhs_val.free(self, l_reg);
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
        const l_int = try lhs_val.getInt(self, node.lhs.firstToken());
        const r_int = try rhs_val.getInt(self, node.rhs.firstToken());

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
        const rhs_val = try self.genNodeNonEmpty(node.value, .value);

        try self.genLval(node.capture, if (self.tokens[node.let_const].id == .Keyword_let)
            .{ .mut = &rhs_val }
        else
            .{ .constant = &rhs_val });
        return Value.empty;
    }

    fn genIdentifier(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        const sym = try self.findSymbol(node.tok);
        if (res == .rt) {
            try self.emitDouble(if (sym.mut) .move_double else .copy_double, res.rt, sym.reg);
            return res.toVal();
        }
        return Value{ .ref = sym.reg };
    }

    fn genThis(self: *Compiler, node: *Node.SingleToken, res: Result) Error!Value {
        const sub_res = try res.toRt(self);
        try self.emitSingle(.load_this_single, sub_res.rt);
        return sub_res.toVal();
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) Error!Value {
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
        const sub_res = try res.toRt(self);
        const str = try self.parseStr(node.str_tok);
        const str_loc = try self.putString(str);

        try self.emitOff(.import_off, sub_res.rt, str_loc);
        return sub_res.toVal();
    }

    fn genError(self: *Compiler, node: *Node.Error, res: Result) Error!Value {
        const val = if (node.capture) |some|
            try self.genNodeNonEmpty(some, .value)
        else
            Value{ .none = {} };

        const sub_res = try res.toRt(self);
        const reg = try val.toRt(self);
        defer val.free(self, reg);

        try self.emitDouble(.build_error_double, sub_res.rt, reg);
        return sub_res.toVal();
    }

    fn genTagged(self: *Compiler, node: *Node.Tagged, res: Result) Error!Value {
        const sub_res = try res.toRt(self);
        const str_loc = try self.putString(self.tokenSlice(node.name));

        try self.func.code.append(.{
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
        try self.func.code.append(.{
            .bare = str_loc,
        });
        return sub_res.toVal();
    }

    fn genRange(self: *Compiler, node: *Node.Range, res: Result) Error!Value {
        const sub_res = try res.toRt(self);
        const start = try self.genRangePart(node.start);
        const end = try self.genRangePart(node.end);
        const step = try self.genRangePart(node.step);
        try self.func.code.append(.{
            .range = .{
                .res = sub_res.rt,
                .start = start.val,
                .end = end.val,
            },
        });
        try self.func.code.append(.{
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
        var buf = std.ArrayList(u8).init(self.gpa);
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
        const sub_res = try res.toRt(self);
        const format_reg = try (Value{ .str = format_string }).toRt(self);
        const format_member_str = try (Value{ .str = "format" }).toRt(self);
        try self.emitTriple(.get_triple, format_reg, format_reg, format_member_str);
        defer self.func.regFree(format_member_str);

        // arg_reg = (args...)
        const arg_reg = format_member_str;
        try self.emitOff(.build_tuple_off, arg_reg, @intCast(u32, node.args.len));

        // prepare registers
        const index_reg = try self.func.regAlloc();
        defer self.func.regFree(index_reg);
        const result_reg = try self.func.regAlloc();
        defer self.func.regFree(result_reg);

        var index = Value{ .int = 0 };
        for (node.args) |arg| {
            _ = try self.genNode(arg, .{ .rt = result_reg });

            try self.makeRuntime(index_reg, index);
            try self.emitTriple(.set_triple, arg_reg, index_reg, result_reg);
            index.int += 1;
        }

        // sub_res.rt = format_reg(arg_reg)
        try self.func.code.append(.{
            .call = .{
                .res = sub_res.rt,
                .func = format_reg,
                .first = arg_reg,
            },
        });
        try self.func.code.append(.{ .bare = 1 });
        return sub_res.toVal();
    }

    fn genTry(self: *Compiler, node: *Node.Try, res: Result) Error!Value {
        const sub_res = switch (res) {
            .rt, .discard => res,
            .value => Result{
                // value is only known at runtime
                .rt = try self.func.regAlloc(),
            },
        };

        var try_scope = Try{
            .jumps = JumpList.init(self.gpa),
            .err_reg = try self.func.regAlloc(),
        };
        defer {
            try_scope.jumps.deinit();
            self.func.regFree(try_scope.err_reg);
        }

        try self.scopes.append(.{ .try_catch = &try_scope });

        const expr_val = try self.genNode(node.expr, sub_res);
        if (sub_res != .rt and expr_val.isRt()) {
            try self.emitSingle(.discard_single, expr_val.getRt());
        }
        // no longer in try scope
        assert(self.scopes.pop() == .try_catch);

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

        const scope_count = self.scopes.items.len;
        defer self.clearScopes(scope_count);

        const capture_reg = try self.func.regAlloc();
        defer self.func.regFree(capture_reg);

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

                    try self.genLval(some, if (self.tokens[tok].id == .Keyword_let)
                        .{ .mut = &Value{ .rt = try_scope.err_reg } }
                    else
                        .{ .constant = &Value{ .rt = try_scope.err_reg } });
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
                .rt = try self.func.regAlloc(),
            },
        };

        const cond_val = try self.genNodeNonEmpty(node.expr, .value);
        const cond_reg = try cond_val.toRt(self);

        var jumps = JumpList.init(self.gpa);
        defer jumps.deinit();

        var case_reg = try self.func.regAlloc();
        defer self.func.regFree(case_reg);
        var seen_catch_all = false;
        for (node.cases) |uncasted_case, case_i| {
            if (seen_catch_all) {
                return self.reportErr("additional cases after catch-all case", uncasted_case.firstToken());
            }

            const scope_count = self.scopes.items.len;
            defer self.clearScopes(scope_count);

            var expr: *Node = undefined;
            var case_skip: ?usize = null;

            if (uncasted_case.cast(.MatchCatchAll)) |case| {
                seen_catch_all = true;
                expr = case.expr;
            } else if (uncasted_case.cast(.MatchLet)) |case| {
                seen_catch_all = true;
                expr = case.expr;

                try self.genLval(case.capture, if (self.tokens[case.let_const].id == .Keyword_let)
                    .{ .mut = &Value{ .rt = cond_reg } }
                else
                    .{ .constant = &Value{ .rt = cond_reg } });
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
            try self.func.code.append(.{
                .primitive = .{ .res = sub_res.rt, .kind = .none },
            });
        }

        // exit match
        for (jumps.items) |jump| {
            self.finishJump(jump);
        }
        return sub_res.toVal();
    }

    const Lval = union(enum) {
        constant: *const Value,
        mut: *const Value,
        assign: *const Value,
        aug_assign: *Value,
    };

    fn genLval(self: *Compiler, node: *Node, lval: Lval) Error!void {
        switch (node.id) {
            .Literal,
            .Block,
            .Prefix,
            .Decl,
            .Infix,
            .If,
            .TypeInfix,
            .Fn,
            .While,
            .Jump,
            .Catch,
            .Import,
            .For,
            .This,
            .Match,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            .FormatString,
            .Try,
            => return self.reportErr("invalid left hand side to assignment", node.firstToken()),

            .Discard => return self.reportErr("'_' can only be used to discard unwanted tuple/list items in destructuring assignment", node.firstToken()),
            .Grouped => return self.genLval(@fieldParentPtr(Node.Grouped, "base", node).expr, lval),
            .Tagged => return self.genLValTagged(@fieldParentPtr(Node.Tagged, "base", node), lval),
            .Range => return self.genLValRange(@fieldParentPtr(Node.Range, "base", node), lval),
            .Error => return self.genLValError(@fieldParentPtr(Node.Error, "base", node), lval),
            .MapItem => unreachable,
            .List => return self.genLValTupleList(@fieldParentPtr(Node.ListTupleMap, "base", node), lval),
            .Tuple => return self.genLValTupleList(@fieldParentPtr(Node.ListTupleMap, "base", node), lval),
            .Map => return self.genLValMap(@fieldParentPtr(Node.ListTupleMap, "base", node), lval),
            .Identifier => return self.genLValIdentifier(@fieldParentPtr(Node.SingleToken, "base", node), lval),
            .Suffix => return self.genLValSuffix(@fieldParentPtr(Node.Suffix, "base", node), lval),
        }
    }

    fn genLValRange(self: *Compiler, node: *Node.Range, lval: Lval) Error!void {
        const val = switch (lval) {
            .constant, .mut, .assign => |val| val,
            .aug_assign => return self.reportErr("invalid left hand side to augmented assignment", node.base.firstToken()),
        };
        if (!val.isRt()) {
            return self.reportErr("expected a range", node.base.firstToken());
        }
        return self.reportErr("TODO: range destructure", node.base.firstToken());
    }

    fn genLValTagged(self: *Compiler, node: *Node.Tagged, lval: Lval) Error!void {
        const val = switch (lval) {
            .constant, .mut, .assign => |val| val,
            .aug_assign => return self.reportErr("invalid left hand side to augmented assignment", node.at),
        };
        if (!val.isRt()) {
            return self.reportErr("expected a tagged value", node.base.firstToken());
        }
        if (node.capture == null) {
            return self.reportErr("expected a capture", node.base.firstToken());
        }
        const str_loc = try self.putString(self.tokenSlice(node.name));
        const unwrap_reg = try self.func.regAlloc();
        try self.emitDouble(.unwrap_tagged, unwrap_reg, val.getRt());
        try self.func.code.append(.{ .bare = str_loc });

        const rhs_val = Value{ .rt = unwrap_reg };
        try self.genLval(node.capture.?, switch (lval) {
            .constant => .{ .constant = &rhs_val },
            .mut => .{ .mut = &rhs_val },
            .assign => .{ .assign = &rhs_val },
            else => unreachable,
        });
    }

    fn genLValError(self: *Compiler, node: *Node.Error, lval: Lval) Error!void {
        const val = switch (lval) {
            .constant, .mut, .assign => |val| val,
            .aug_assign => return self.reportErr("invalid left hand side to augmented assignment", node.tok),
        };
        if (!val.isRt()) {
            return self.reportErr("expected an error", node.tok);
        }
        if (node.capture == null) {
            return self.reportErr("expected a capture", node.tok);
        }
        const unwrap_reg = try self.func.regAlloc();
        try self.emitDouble(.unwrap_error_double, unwrap_reg, val.getRt());

        const rhs_val = Value{ .rt = unwrap_reg };
        try self.genLval(node.capture.?, switch (lval) {
            .constant => .{ .constant = &rhs_val },
            .mut => .{ .mut = &rhs_val },
            .assign => .{ .assign = &rhs_val },
            else => unreachable,
        });
    }

    fn genLValTupleList(self: *Compiler, node: *Node.ListTupleMap, lval: Lval) Error!void {
        if (node.values.len > std.math.maxInt(u32)) {
            return self.reportErr("too many items", node.l_tok);
        }
        const res = switch (lval) {
            .constant, .mut, .assign => |val| val,
            .aug_assign => return self.reportErr("invalid left hand side to augmented assignment", node.l_tok),
        };
        if (!res.isRt()) {
            return self.reportErr("expected a tuple/list", node.l_tok);
        }

        // prepare registers
        const container_reg = res.getRt();
        const index_reg = try self.func.regAlloc();
        defer self.func.regFree(index_reg);
        var result_reg = try self.func.regAlloc();

        var index = Value{ .int = 0 };
        for (node.values) |val, i| {
            if (val.id == .Discard) {
                index.int += 1;
                continue;
            }

            try self.makeRuntime(index_reg, index);
            try self.emitTriple(.get_triple, result_reg, container_reg, index_reg);
            const rt_val = Value{ .rt = result_reg };
            try self.genLval(val, switch (lval) {
                .constant => .{ .constant = &rt_val },
                .mut => .{ .mut = &rt_val },
                .assign => .{ .assign = &rt_val },
                else => unreachable,
            });
            index.int += 1;

            if (i + 1 != node.values.len and lval != .assign)
                result_reg = try self.func.regAlloc();
        }
    }

    fn genLValMap(self: *Compiler, node: *Node.ListTupleMap, lval: Lval) Error!void {
        if (node.values.len > std.math.maxInt(u32)) {
            return self.reportErr("too many items", node.base.firstToken());
        }
        const res = switch (lval) {
            .constant, .mut, .assign => |val| val,
            .aug_assign => return self.reportErr("invalid left hand side to augmented assignment", node.l_tok),
        };
        if (!res.isRt()) {
            return self.reportErr("expected a map", node.base.firstToken());
        }
        const container_reg = res.getRt();
        const index_reg = try self.func.regAlloc();
        defer self.func.regFree(index_reg);
        var result_reg = try self.func.regAlloc();

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
            try self.genLval(item.value, switch (lval) {
                .constant => .{ .constant = &rt_val },
                .mut => .{ .mut = &rt_val },
                .assign => .{ .assign = &rt_val },
                else => unreachable,
            });

            if (i + 1 != node.values.len and lval != .assign) result_reg = try self.func.regAlloc();
        }
    }

    fn genLValIdentifier(self: *Compiler, node: *Node.SingleToken, lval: Lval) Error!void {
        switch (lval) {
            .mut, .constant => |val| {
                if (self.getForwardDecl(node.tok)) |some| {
                    // only functions can be forward declared
                    assert(val.* == .func);
                    return self.makeRuntime(some, val.*);
                }
                var reg = try val.toRt(self);

                if (val.* == .ref and lval == .mut) {
                    // copy on assign
                    const copy_reg = try self.func.regAlloc();
                    try self.emitDouble(.copy_double, copy_reg, reg);
                    reg = copy_reg;
                }
                const sym = Symbol{
                    .name = self.tokenSlice(node.tok),
                    .reg = reg,
                };
                try self.scopes.append(if (lval == .mut)
                    .{ .mut = sym }
                else
                    .{ .constant = sym });
            },
            .assign => |val| {
                const sym = try self.findSymbol(node.tok);
                if (!sym.mut) {
                    return self.reportErr("assignment to constant", node.tok);
                }
                if (val.* == .ref) {
                    try self.emitDouble(.copy_double, sym.reg, val.getRt());
                } else if (val.isRt()) {
                    try self.emitDouble(.move_double, sym.reg, val.getRt());
                } else {
                    try self.makeRuntime(sym.reg, val.*);
                }
            },
            .aug_assign => |val| {
                const sym = try self.findSymbol(node.tok);
                if (!sym.mut) {
                    return self.reportErr("assignment to constant", node.tok);
                }
                val.* = Value{ .ref = sym.reg };
            },
        }
    }

    fn genLValSuffix(self: *Compiler, node: *Node.Suffix, lval: Lval) Error!void {
        if (node.op == .call) {
            return self.reportErr("invalid left hand side to assignment", node.lhs.firstToken());
        }
        const lhs_val = try self.genNode(node.lhs, .value);
        if (lhs_val != .str and !lhs_val.isRt()) {
            return self.reportErr("invalid left hand side to suffix op", node.lhs.firstToken());
        }
        const l_reg = try lhs_val.toRt(self);
        defer lhs_val.free(self, l_reg);

        const index_val = switch (node.op) {
            .call => unreachable,
            .member => Value{ .str = self.tokenSlice(node.r_tok) },
            .subscript => |val| try self.genNodeNonEmpty(val, .value),
        };
        const index_reg = try index_val.toRt(self);
        defer index_val.free(self, index_reg);

        switch (lval) {
            .mut, .constant => return self.reportErr("cannot declare to subscript", node.l_tok),
            .aug_assign => |val| {
                const res_reg = try self.func.regAlloc();
                try self.emitTriple(.get_triple, res_reg, l_reg, index_reg);
                val.* = Value{ .rt = res_reg };
            },
            .assign => |rhs_val| {
                const r_reg = try rhs_val.toRt(self);
                defer rhs_val.free(self, r_reg);
                try self.emitTriple(.set_triple, l_reg, index_reg, r_reg);
            },
        }
    }

    fn addLineInfo(self: *Compiler, node: *Node) !void {
        const tok = self.tokens[node.firstToken()];

        try self.func.code.append(.{ .op = .{ .op = .line_info } });
        try self.func.code.append(.{ .bare = tok.start });
    }

    fn finishJump(self: *Compiler, jump_addr: usize) void {
        self.func.code.items[jump_addr] = .{
            .bare = @intCast(u32, self.func.code.items.len - jump_addr),
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
