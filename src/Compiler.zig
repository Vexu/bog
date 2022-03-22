const std = @import("std");
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const mem = std.mem;
const bog = @import("bog.zig");
const Bytecode = bog.Bytecode;
const Errors = bog.Errors;
const Node = bog.Node;
const Ref = Bytecode.Ref;
const Tree = bog.Tree;
const TokenIndex = bog.Token.Index;

const Compiler = @This();

// inputs
tree: *const Tree,
errors: *Errors,
gpa: Allocator,

// outputs
instructions: Bytecode.Inst.List = .{},
extra: std.ArrayListUnmanaged(u32) = .{},
strings: std.ArrayListUnmanaged(u8) = .{},
string_interner: std.StringHashMapUnmanaged(u32) = .{},

// intermediate
arena: Allocator,
scopes: std.ArrayListUnmanaged(Scope) = .{},
unresolved_globals: std.ArrayListUnmanaged(UnresolvedGlobal) = .{},
cur_loop: ?*Loop = null,
cur_try: ?*Try = null,

code: *Code,

pub fn compile(gpa: Allocator, source: []const u8, errors: *Errors) (Compiler.Error || bog.Parser.Error || bog.Tokenizer.Error)!Bytecode {
    var tree = try bog.parse(gpa, source, errors);
    defer tree.deinit(gpa);

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    var code: Code = .{};
    defer code.deinit(gpa);

    var compiler = Compiler{
        .tree = &tree,
        .errors = errors,
        .gpa = gpa,
        .arena = arena_state.allocator(),
        .code = &code,
    };
    defer compiler.deinit();

    for (tree.root_nodes) |node| {
        // try compiler.addLineInfo(node);

        const val = try compiler.genNode(node, .discard);
        if (val.isRt()) {
            // discard unused runtime value
            _ = try compiler.addUn(.discard, val.getRt());
        }
    }
    _ = try compiler.addUn(.ret_null, undefined);

    return Bytecode{
        .name = "",
        .code = compiler.instructions.toOwnedSlice(),
        .extra = compiler.extra.toOwnedSlice(gpa),
        .strings = compiler.strings.toOwnedSlice(gpa),
        .main = code.toOwnedSlice(gpa),
        .debug_info = undefined, // TODO
    };
}

pub fn deinit(c: *Compiler) void {
    c.scopes.deinit(c.gpa);
    c.instructions.deinit(c.gpa);
    c.extra.deinit(c.gpa);
    c.strings.deinit(c.gpa);
    c.string_interner.deinit(c.gpa);
    c.* = undefined;
}

pub const max_params = 32;

const Code = std.ArrayListUnmanaged(Bytecode.Ref);

const Fn = struct {
    code: Code,
    captures: std.ArrayList(Capture),

    const Capture = struct {
        name: []const u8,
        local_ref: Ref,
        parent_ref: Ref,
        mut: bool,
    };
};

const UnresolvedGlobal = struct {
    identifier: TokenIndex,
    ref: Ref,
};

const Symbol = struct {
    name: []const u8,
    val: Value,
    ref: Ref,
    mut: bool,
};

const Scope = union(enum) {
    func: *Fn,
    symbol: Symbol,
};

const Loop = struct {
    breaks: BreakList = .{},
    first_inst: u32,

    const BreakList = std.ArrayListUnmanaged(u32);
};

const Try = struct {
    jumps: JumpList = .{},
    err_ref: Ref,
};

const JumpList = std.ArrayListUnmanaged(u32);

const Value = union(enum) {
    /// result of continue, break, return and assignment; cannot exist at runtime
    empty,
    ref: Ref,

    /// reference to a mutable variable
    mut: Ref,

    @"null",
    int: i64,
    num: f64,
    Bool: bool,
    str: []const u8,

    fn isRt(val: Value) bool {
        return switch (val) {
            .ref, .mut => true,
            else => false,
        };
    }

    fn getRt(val: Value) Ref {
        switch (val) {
            .ref, .mut => |r| return r,
            else => unreachable,
        }
    }

    fn getBool(val: Value, c: *Compiler, node: Node.Index) !bool {
        if (val != .Bool) {
            return c.reportErr("expected a boolean", node);
        }
        return val.Bool;
    }

    fn getInt(val: Value, c: *Compiler, node: Node.Index) !i64 {
        if (val != .int) {
            return c.reportErr("expected an integer", node);
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

    fn getStr(val: Value, c: *Compiler, node: Node.Index) ![]const u8 {
        if (val != .str) {
            return c.reportErr("expected a string", node);
        }
        return val.str;
    }

    fn checkNum(val: Value, c: *Compiler, node: Node.Index) !void {
        if (val != .int and val != .num) {
            return c.reportErr("expected a number", node);
        }
    }
};

pub const Error = error{CompileError} || Allocator.Error;

fn addInst(c: *Compiler, op: Bytecode.Inst.Op, data: Bytecode.Inst.Data) !Ref {
    const new_index = @intCast(Ref, c.instructions.len);
    try c.instructions.append(c.gpa, .{ .op = op, .data = data });
    try c.code.append(c.gpa, new_index);
    return new_index;
}

fn addUn(c: *Compiler, op: Bytecode.Inst.Op, arg: Ref) !Ref {
    const new_index = @intCast(Ref, c.instructions.len);
    try c.instructions.append(c.gpa, .{
        .op = op,
        .data = .{ .un = arg },
    });
    try c.code.append(c.gpa, new_index);
    return new_index;
}

fn addBin(c: *Compiler, op: Bytecode.Inst.Op, lhs: Ref, rhs: Ref) !Ref {
    const new_index = @intCast(Ref, c.instructions.len);
    try c.instructions.append(c.gpa, .{
        .op = op,
        .data = .{ .bin = .{ .lhs = lhs, .rhs = rhs } },
    });
    try c.code.append(c.gpa, new_index);
    return new_index;
}

fn addJump(c: *Compiler, op: Bytecode.Inst.Op, operand: Ref) !Ref {
    return c.addInst(op, .{
        .jump_condition = .{
            .operand = operand,
            .offset = undefined, // set later
        },
    });
}

fn addAggregate(c: *Compiler, op: Bytecode.Inst.Op, items: []const Ref) !Ref {
    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.appendSlice(c.gpa, items);
    return c.addInst(op, .{
        .aggregate = .{
            .extra = extra,
            .len = @intCast(u32, items.len),
        },
    });
}

fn finishJump(c: *Compiler, jump_inst: Ref) void {
    const offset = @intCast(u32, c.code.items.len);
    const data = c.instructions.items(.data);
    const ops = c.instructions.items(.op);
    if (ops[jump_inst] == .jump) {
        data[jump_inst] = .{ .jump = offset };
    } else {
        data[jump_inst].jump_condition.offset = offset;
    }
}

fn makeRuntime(c: *Compiler, val: Value) Error!Ref {
    return switch (val) {
        .empty => unreachable,
        .mut, .ref => |ref| ref,
        .@"null" => try c.addInst(.primitive, .{ .primitive = .@"null" }),
        .int => |int| try c.addInst(.int, .{ .int = int }),
        .num => |num| try c.addInst(.num, .{ .num = num }),
        .Bool => |b| try c.addInst(.primitive, .{ .primitive = if (b) .@"true" else .@"false" }),
        .str => |str| try c.addInst(.str, .{ .str = .{
            .len = @intCast(u32, str.len),
            .offset = try c.putString(str),
        } }),
    };
}

fn putString(c: *Compiler, str: []const u8) !u32 {
    if (c.string_interner.get(str)) |some| return some;
    const offset = @intCast(u32, c.strings.items.len);
    try c.strings.appendSlice(c.gpa, str);

    _ = try c.string_interner.put(c.gpa, str, offset);
    return offset;
}

const FoundSymbol = struct {
    ref: Ref,
    mut: bool,
    global: bool = false,
};

fn findSymbol(c: *Compiler, tok: TokenIndex) !FoundSymbol {
    return c.findSymbolExtra(tok, c.scopes.items.len);
}

fn findSymbolExtra(c: *Compiler, tok: TokenIndex, start_index: usize) Error!FoundSymbol {
    const name = c.tree.tokenSlice(tok);
    var i = start_index;

    while (i > 0) {
        i -= 1;
        const item = c.scopes.items[i];
        switch (item) {
            .func => |f| {
                for (f.captures.items) |capture| {
                    if (mem.eql(u8, capture.name, name)) {
                        return FoundSymbol{
                            .ref = capture.local_ref,
                            .mut = capture.mut,
                        };
                    }
                }

                const sym = try c.findSymbolExtra(tok, i);
                const loaded_capture = @intCast(Ref, c.instructions.len);
                try c.instructions.append(c.gpa, .{
                    .op = .load_capture,
                    .data = .{ .un = @intCast(u32, f.captures.items.len - 1) },
                });
                try f.code.append(c.gpa, loaded_capture);
                try f.captures.append(.{
                    .name = name,
                    .parent_ref = sym.ref,
                    .local_ref = loaded_capture,
                    .mut = sym.mut,
                });
                return FoundSymbol{ .ref = loaded_capture, .mut = sym.mut };
            },
            .symbol => |sym| if (mem.eql(u8, sym.name, name)) {
                return FoundSymbol{ .ref = sym.ref, .mut = sym.mut };
            },
        }
    }

    const ref = try c.addInst(.load_global, undefined);
    try c.unresolved_globals.append(c.gpa, .{ .identifier = tok, .ref = ref });
    return FoundSymbol{ .ref = ref, .mut = false, .global = true };
}

fn checkRedeclaration(c: *Compiler, tok: TokenIndex) !void {
    const name = c.tree.tokenSlice(tok);
    var i = c.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = c.scopes.items[i];
        switch (scope) {
            .symbol => |sym| if (std.mem.eql(u8, sym.name, name)) {
                const msg = try bog.Value.String.init(c.gpa, "redeclaration of '{s}'", .{name});
                const starts = c.tree.tokens.items(.start);
                try c.errors.add(msg, starts[tok], .err);
                return error.CompileError;
            },
            else => {},
        }
    }
}

const Result = union(enum) {
    /// A runtime value is expected
    ref: Ref,

    /// A value, runtime or constant, is expected
    value,

    /// No value is expected if some is given it will be discarded
    discard,

    /// returns .empty if res != .rt
    fn toVal(res: Result) Value {
        return if (res == .ref) .{ .ref = res.ref } else .empty;
    }
};

fn wrapResult(c: *Compiler, node: Node.Index, val: Value, res: Result) Error!Value {
    if (val == .empty and res != .discard) {
        return c.reportErr("expected a value", node);
    }
    if (res == .discard and val.isRt()) {
        // discard unused runtime value
        _ = try c.addUn(.discard, val.getRt());
    }
    if (res == .ref) {
        const val_ref = try c.makeRuntime(val);
        if (val_ref == res.ref) return val;
        if (val == .mut) {
            _ = try c.addBin(.copy, res.ref, val_ref);
        } else {
            _ = try c.addBin(.move, res.ref, val_ref);
        }
        return Value{ .ref = res.ref };
    }
    return val;
}

fn genNode(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const ids = c.tree.nodes.items(.id);
    const tokens = c.tree.nodes.items(.token);
    switch (ids[node]) {
        .string_expr => {
            const val = Value{ .str = try c.parseStr(tokens[node]) };
            return c.wrapResult(node, val, res);
        },
        .int_expr => {
            const slice = c.tree.tokenSlice(tokens[node]);
            const val = Value{
                .int = std.fmt.parseInt(i64, slice, 0) catch
                    return c.reportErr("TODO big int", node),
            };
            return c.wrapResult(node, val, res);
        },
        .num_expr => {
            const slice = c.tree.tokenSlice(tokens[node]);
            const val = Value{
                .num = std.fmt.parseFloat(f64, slice) catch unreachable,
            };
            return c.wrapResult(node, val, res);
        },
        .true_expr => {
            const val = Value{ .Bool = true };
            return c.wrapResult(node, val, res);
        },
        .false_expr => {
            const val = Value{ .Bool = false };
            return c.wrapResult(node, val, res);
        },
        .null_expr => {
            const val = Value{ .@"null" = {} };
            return c.wrapResult(node, val, res);
        },
        .decl_ref_expr => {
            const val = try c.genDeclRef(node);
            return c.wrapResult(node, val, res);
        },

        .decl => try c.genDecl(node),

        .return_expr => try c.genReturn(node),
        .break_expr => try c.genBreak(node),
        .continue_expr => try c.genContinue(node),
        .for_expr, .for_let_expr => return c.genFor(node, res),
        .while_expr, .while_let_expr => return c.genWhile(node, res),
        .if_expr,
        .if_else_expr,
        .if_let_expr,
        .if_let_else_expr,
        => return c.genIf(node, res),
        .match_expr,
        .match_expr_one,
        => return c.genMatch(node, res),
        .match_case_catch_all,
        .match_case_let,
        .match_case,
        .match_case_one,
        => unreachable, // handled in genMatch
        .block_stmt_two,
        .block_stmt,
        => {
            var buf: [2]Node.Index = undefined;
            const stmts = c.tree.nodeItems(node, &buf);
            return c.genBlock(stmts, res);
        },
        .paren_expr => {
            const data = c.tree.nodes.items(.data);
            return c.genNode(data[node].un, res);
        },
        .as_expr => {
            const val = try c.genAs(node);
            return c.wrapResult(node, val, res);
        },
        .is_expr => {
            const val = try c.genIs(node);
            return c.wrapResult(node, val, res);
        },
        .bool_not_expr => {
            const val = try c.genBoolNot(node);
            return c.wrapResult(node, val, res);
        },
        .bit_not_expr => {
            const val = try c.genBitNot(node);
            return c.wrapResult(node, val, res);
        },
        .negate_expr => {
            const val = try c.genNegate(node);
            return c.wrapResult(node, val, res);
        },

        .this_expr,
        .throw_expr,
        .member_access_expr,
        .bool_or_expr,
        .bool_and_expr,
        .less_than_expr,
        .less_than_equal_expr,
        .greater_than_expr,
        .greater_than_equal_expr,
        .equal_expr,
        .not_equal_expr,
        .in_expr,
        .bit_and_expr,
        .bit_or_expr,
        .bit_xor_expr,
        .l_shift_expr,
        .r_shift_expr,
        .add_expr,
        .sub_expr,
        .mul_expr,
        .div_expr,
        .div_floor_expr,
        .mod_expr,
        .pow_expr,
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
        .map_item_expr,
        .array_access_expr,
        .import_expr,
        .error_expr,
        .enum_expr,
        .range_expr,
        .range_expr_start,
        .range_expr_end,
        .range_expr_step,
        .fn_expr,
        .fn_expr_one,
        .call_expr,
        .call_expr_one,
        .tuple_expr,
        .tuple_expr_two,
        .list_expr,
        .list_expr_two,
        .map_expr,
        .map_expr_two,
        .try_expr,
        .try_expr_one,
        .catch_let_expr,
        .catch_expr,
        .format_expr,
        => @panic("TODO"),
        // zig fmt: off
        .ident_dest, .discard_dest, .mut_ident_dest, .error_dest,
        .range_dest, .range_dest_start, .range_dest_end, .range_dest_step,
        .tuple_dest, .tuple_dest_two, .list_dest, .list_dest_two,
        .map_dest, .map_dest_two => unreachable,
        // zig fmt: on
    }
    return c.wrapResult(node, .empty, res);
}

fn genDeclRef(c: *Compiler, node: Node.Index) Error!Value {
    const tokens = c.tree.nodes.items(.token);
    const sym = try c.findSymbol(tokens[node]);
    if (sym.mut) {
        return Value{ .mut = sym.ref };
    } else {
        return Value{ .ref = sym.ref };
    }
}

fn genDecl(c: *Compiler, node: Node.Index) !void {
    const data = c.tree.nodes.items(.data);
    const init_val = try c.genNode(data[node].bin.rhs, .value);
    try c.genLval(data[node].bin.lhs, .{ .let = &init_val });
}

fn genReturn(c: *Compiler, node: Node.Index) !void {
    const data = c.tree.nodes.items(.data);
    if (data[node].un != 0) {
        const operand = try c.genNode(data[node].un, .value);
        _ = try c.addUn(.ret, try c.makeRuntime(operand));
    } else {
        _ = try c.addUn(.ret_null, undefined);
    }
}

fn genBreak(c: *Compiler, node: Node.Index) !void {
    const loop = c.cur_loop orelse
        return c.reportErr("break outside of loop", node);

    const jump = try c.addInst(.jump, undefined);
    try loop.breaks.append(c.gpa, jump);
}

fn genContinue(c: *Compiler, node: Node.Index) !void {
    const loop = c.cur_loop orelse
        return c.reportErr("continue outside of loop", node);

    _ = try c.addInst(.jump, .{ .jump = loop.first_inst });
}

fn createListComprehension(c: *Compiler, ref: ?Ref) !Result {
    const list = try c.addAggregate(.build_list, &.{});
    if (ref) |some| {
        _ = try c.addBin(.move, some, list);
        return Result{ .ref = some };
    } else {
        return Result{ .ref = list };
    }
}

fn genFor(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const sub_res = switch (res) {
        .discard => res,
        .value => try c.createListComprehension(null),
        .ref => |ref| try c.createListComprehension(ref),
    };
    const for_expr = Tree.For.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    const cond_val = try c.genNode(for_expr.cond, .value);
    if (!cond_val.isRt() and cond_val != .str)
        return c.reportErr("expected iterable value", for_expr.cond);

    const cond_ref = try c.makeRuntime(cond_val);

    // create the iterator
    const iter_ref = try c.addUn(.iter_init, cond_ref);
    if (c.cur_try) |try_scope| {
        _ = try c.addBin(.move, try_scope.err_ref, iter_ref);
        try try_scope.jumps.append(c.gpa, try c.addJump(.jump_if_error, iter_ref));
    }

    var loop = Loop{
        .first_inst = @intCast(u32, c.code.items.len),
    };
    defer loop.breaks.deinit(c.gpa);

    const old_loop = c.cur_loop;
    defer c.cur_loop = old_loop;
    c.cur_loop = &loop;

    // iter next is fused with a jump_null, offset is set after body is generated
    const elem_ref = try c.addJump(.iter_next, iter_ref);

    if (for_expr.capture) |some| {
        try c.genLval(some, .{ .let = &.{ .ref = elem_ref } });
    }

    switch (sub_res) {
        .discard => _ = try c.genNode(for_expr.body, .discard),
        .ref => |list| {
            const body_val = try c.genNode(for_expr.body, .value);
            const body_ref = try c.makeRuntime(body_val);
            _ = try c.addBin(.append, list, body_ref);
        },
        else => unreachable,
    }

    // jump to the start of the loop
    _ = try c.addInst(.jump, .{ .jump = loop.first_inst });

    // exit loop when IterNext results in None
    c.finishJump(elem_ref);

    for (loop.breaks.items) |@"break"| {
        c.finishJump(@"break");
    }
    return sub_res.toVal();
}

fn genWhile(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const sub_res = switch (res) {
        .discard => res,
        .value => try c.createListComprehension(null),
        .ref => |ref| try c.createListComprehension(ref),
    };
    const while_expr = Tree.While.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    var loop = Loop{
        .first_inst = @intCast(u32, c.code.items.len),
    };
    defer loop.breaks.deinit(c.gpa);

    const old_loop = c.cur_loop;
    defer c.cur_loop = old_loop;
    c.cur_loop = &loop;

    // beginning of condition
    var cond_jump: ?Ref = null;

    const cond_val = try c.genNode(while_expr.cond, .value);
    if (while_expr.capture) |capture| {
        if (cond_val.isRt()) {
            // exit loop if cond == null
            cond_jump = try c.addJump(.jump_if_null, cond_val.getRt());
        } else if (cond_val == .@"null") {
            // never executed
            return sub_res.toVal();
        }
        const cond_ref = try c.makeRuntime(cond_val);

        try c.genLval(capture, .{ .let = &.{ .ref = cond_ref } });
    } else if (cond_val.isRt()) {
        cond_jump = try c.addJump(.jump_if_false, cond_val.getRt());
    } else {
        const bool_val = try cond_val.getBool(c, while_expr.cond);
        if (bool_val == false) {
            // never executed
            return sub_res.toVal();
        }
    }

    switch (sub_res) {
        .discard => _ = try c.genNode(while_expr.body, .discard),
        .ref => |list| {
            const body_val = try c.genNode(while_expr.body, .value);
            const body_ref = try c.makeRuntime(body_val);
            _ = try c.addBin(.append, list, body_ref);
        },
        else => unreachable,
    }

    // jump to the start of the loop
    _ = try c.addInst(.jump, .{ .jump = loop.first_inst });

    // exit loop if cond == false
    if (cond_jump) |some| {
        c.finishJump(some);
    }

    for (loop.breaks.items) |@"break"| {
        c.finishJump(@"break");
    }

    return sub_res.toVal();
}

fn genIf(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const if_expr = Tree.If.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    var if_skip: u32 = undefined;

    const cond_val = try c.genNode(if_expr.cond, .value);
    if (if_expr.capture) |capture| {
        if (cond_val.isRt()) {
            // jump past if_body if cond == .none
            if_skip = try c.addJump(.jump_if_null, cond_val.getRt());
        } else if (cond_val == .@"null") {
            if (if_expr.else_body) |some| {
                return c.genNode(some, res);
            }

            const res_val = Value{ .@"null" = {} };
            return c.wrapResult(node, res_val, res);
        }
        const cond_ref = try c.makeRuntime(cond_val);

        try c.genLval(capture, .{ .let = &.{ .ref = cond_ref } });
    } else if (!cond_val.isRt()) {
        const bool_val = try cond_val.getBool(c, if_expr.cond);

        if (bool_val) {
            return c.genNode(if_expr.then_body, res);
        } else if (if_expr.else_body) |some| {
            return c.genNode(some, res);
        }

        const res_val = Value{ .@"null" = {} };
        return c.wrapResult(node, res_val, res);
    } else {
        // jump past if_body if cond == false
        if_skip = try c.addJump(.jump_if_false, cond_val.getRt());
    }
    const sub_res = switch (res) {
        .ref, .discard => res,
        .value => val: {
            // add a dummy instruction we can store the value into
            const res_ref = @intCast(Ref, c.instructions.len);
            try c.instructions.append(c.gpa, undefined);
            break :val Result{ .ref = res_ref };
        },
    };

    // sub_res is either ref or discard, either way wrapResult handles it
    _ = try c.genNode(if_expr.then_body, sub_res);

    // jump past else_body since if_body was executed
    const else_skip = if (if_expr.else_body != null or sub_res == .ref)
        try c.addUn(.jump, undefined)
    else
        null;

    c.finishJump(if_skip);
    // end capture scope
    c.scopes.items.len = scope_count;

    if (if_expr.else_body) |some| {
        // sub_res is either ref or discard, either way wrapResult handles it
        _ = try c.genNode(some, sub_res);
    } else {
        const res_val = Value{ .@"null" = {} };
        _ = try c.wrapResult(node, res_val, sub_res);
    }

    if (else_skip) |some| {
        c.finishJump(some);
    }
    return sub_res.toVal();
}

fn genMatch(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const sub_res = switch (res) {
        .ref, .discard => res,
        .value => val: {
            // add a dummy instruction we can store the value into
            const res_ref = @intCast(Ref, c.instructions.len);
            try c.instructions.append(c.gpa, undefined);
            break :val Result{ .ref = res_ref };
        },
    };

    const ids = c.tree.nodes.items(.id);
    const data = c.tree.nodes.items(.data);
    var buf: [2]Node.Index = undefined;
    const cases = c.tree.nodeItems(node, &buf);

    const cond_val = try c.genNode(cases[0], .value);
    const cond_ref = try c.makeRuntime(cond_val);

    var jumps: JumpList = .{};
    defer jumps.deinit(c.gpa);

    var seen_catch_all = false;
    for (cases[1..]) |case, case_i| {
        if (seen_catch_all) {
            return c.reportErr("additional cases after catch-all case", case);
        }

        const scope_count = c.scopes.items.len;
        defer c.scopes.items.len = scope_count;

        var expr: Node.Index = undefined;
        var case_skip: ?u32 = null;

        switch (ids[case]) {
            .match_case_catch_all => {
                seen_catch_all = true;
                expr = data[case].un;
            },
            .match_case_let => {
                seen_catch_all = true;
                expr = data[case].bin.rhs;

                try c.genLval(case, .{ .let = &.{ .ref = cond_ref } });
            },
            .match_case,
            .match_case_one,
            => {
                var buf_2: [2]Node.Index = undefined;
                const items = c.tree.nodeItems(case, &buf_2);
                expr = items[items.len - 1];

                if (items.len == 2) {
                    const item_val = try c.genNode(items[0], .value);
                    const item_ref = try c.makeRuntime(item_val);
                    // if not equal to the error value jump over this handler
                    const eq_ref = try c.addBin(.equal, item_ref, cond_ref);
                    case_skip = try c.addJump(.jump_if_false, eq_ref);
                } else {
                    var success_jumps: JumpList = .{};
                    defer success_jumps.deinit(c.gpa);

                    for (items[0 .. items.len - 1]) |item| {
                        const item_val = try c.genNode(item, .value);
                        const item_ref = try c.makeRuntime(item_val);

                        const eq_ref = try c.addBin(.equal, item_ref, cond_ref);
                        try success_jumps.append(c.gpa, try c.addJump(.jump_if_true, eq_ref));
                    }
                    case_skip = try c.addUn(.jump, undefined);

                    for (success_jumps.items) |some| {
                        c.finishJump(some);
                    }
                }
            },
            else => unreachable,
        }

        // sub_res is either ref or discard, either way wrapResult handles it
        _ = try c.genNode(expr, sub_res);

        // exit match (unless it's this is the last case)
        if (case_i + 2 != cases.len) {
            try jumps.append(c.gpa, try c.addUn(.jump, undefined));
        }

        // jump over this case if the value doesn't match
        if (case_skip) |some| {
            c.finishJump(some);
        }
    }

    if (!seen_catch_all) {
        const res_val = Value{ .@"null" = {} };
        _ = try c.wrapResult(node, res_val, sub_res);
    }

    // exit match
    for (jumps.items) |jump| {
        c.finishJump(jump);
    }
    return sub_res.toVal();
}

fn genBlock(c: *Compiler, stmts: []const Node.Index, res: Result) Error!Value {
    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    for (stmts) |stmt, i| {
        // return value of last instruction if it is not discarded
        if (i + 1 == stmts.len) {
            return c.genNode(stmt, res);
        }

        _ = try c.genNode(stmt, .discard);
    }
    return Value{ .@"null" = {} };
}

const type_id_map = std.ComptimeStringMap(bog.Type, .{
    .{ "null", .@"null" },
    .{ "int", .int },
    .{ "num", .num },
    .{ "bool", .bool },
    .{ "str", .str },
    .{ "tuple", .tuple },
    .{ "map", .map },
    .{ "list", .list },
    .{ "err", .err },
    .{ "range", .range },
    .{ "func", .func },
    .{ "tagged", .tagged },
});

fn genAs(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = try c.genNode(data[node].ty_bin.lhs, .value);

    const ty_tok = data[node].ty_bin.rhs;

    const type_str = c.tree.tokenSlice(ty_tok);
    const type_id = type_id_map.get(type_str) orelse
        return c.reportErr("expected a type name", ty_tok);

    if (lhs.isRt()) {
        const cast_ref = try c.addInst(.as, .{ .bin_ty = .{
            .operand = lhs.getRt(),
            .ty = type_id,
        } });

        // `as` can result in a type error
        if (c.cur_try) |try_scope| {
            _ = try c.addBin(.move, try_scope.err_ref, cast_ref);
            try try_scope.jumps.append(c.gpa, try c.addJump(.jump_if_error, cast_ref));
        }
        return Value{ .ref = cast_ref };
    }

    return switch (type_id) {
        .@"null" => Value{ .@"null" = {} },
        .int => Value{
            .int = switch (lhs) {
                .int => |val| val,
                .num => |val| std.math.lossyCast(i64, val),
                .Bool => |val| @boolToInt(val),
                .str => |str| std.fmt.parseInt(i64, str, 0) catch
                    return c.reportErr("invalid cast to int", ty_tok),
                else => return c.reportErr("invalid cast to int", ty_tok),
            },
        },
        .num => Value{
            .num = switch (lhs) {
                .num => |val| val,
                .int => |val| std.math.lossyCast(f64, val),
                .Bool => |val| @intToFloat(f64, @boolToInt(val)),
                .str => |str| std.fmt.parseFloat(f64, str) catch
                    return c.reportErr("invalid cast to num", ty_tok),
                else => return c.reportErr("invalid cast to num", ty_tok),
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
                    return c.reportErr("cannot cast string to bool", ty_tok),
                else => return c.reportErr("invalid cast to bool", ty_tok),
            },
        },
        .str => Value{
            .str = switch (lhs) {
                .int => |val| try std.fmt.allocPrint(c.arena, "{}", .{val}),
                .num => |val| try std.fmt.allocPrint(c.arena, "{d}", .{val}),
                .Bool => |val| if (val) "true" else "false",
                .str => |val| val,
                else => return c.reportErr("invalid cast to string", ty_tok),
            },
        },
        .func => return c.reportErr("cannot cast to function", ty_tok),
        .err => return c.reportErr("cannot cast to error", ty_tok),
        .range => return c.reportErr("cannot cast to range", ty_tok),
        .tuple, .map, .list, .tagged => return c.reportErr("invalid cast", ty_tok),
        else => unreachable,
    };
}

fn genIs(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = try c.genNode(data[node].ty_bin.lhs, .value);

    const ty_tok = data[node].ty_bin.rhs;

    const type_str = c.tree.tokenSlice(ty_tok);
    const type_id = type_id_map.get(type_str) orelse
        return c.reportErr("expected a type name", ty_tok);

    if (lhs.isRt()) {
        const ref = try c.addInst(.is, .{ .bin_ty = .{
            .operand = lhs.getRt(),
            .ty = type_id,
        } });
        return Value{ .ref = ref };
    }

    return Value{
        .Bool = switch (type_id) {
            .@"null" => lhs == .@"null",
            .int => lhs == .int,
            .num => lhs == .num,
            .bool => lhs == .Bool,
            .str => lhs == .str,
            else => false,
        },
    };
}

fn genBoolNot(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const operand = try c.genNode(data[node].un, .value);

    if (operand.isRt()) {
        const ref = try c.addUn(.bool_not, operand.getRt());
        return Value{ .ref = ref };
    }
    return Value{ .Bool = !try operand.getBool(c, data[node].un) };
}

fn genBitNot(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const operand = try c.genNode(data[node].un, .value);

    if (operand.isRt()) {
        const ref = try c.addUn(.bit_not, operand.getRt());
        return Value{ .ref = ref };
    }
    return Value{ .int = ~try operand.getInt(c, data[node].un) };
}

fn genNegate(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const operand = try c.genNode(data[node].un, .value);

    if (operand.isRt()) {
        const ref = try c.addUn(.negate, operand.getRt());
        return Value{ .ref = ref };
    }

    try operand.checkNum(c, data[node].un);
    if (operand == .int) {
        return Value{
            .int = std.math.sub(i64, 0, operand.int) catch
                return c.reportErr("TODO integer overflow", node),
        };
    } else {
        return Value{ .num = -operand.num };
    }
}

const Lval = union(enum) {
    let: *const Value,
    assign: *const Value,
    aug_assign: *Value,
};

fn genLval(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const ids = c.tree.nodes.items(.id);
    switch (ids[node]) {
        .ident_dest => try c.genLValIdentifier(node, lval, false),
        .mut_ident_dest => try c.genLValIdentifier(node, lval, true),
        .discard_dest => return c.reportErr(
            "'_' can only be used to discard unwanted tuple/list items in destructuring assignment",
            node,
        ),
        .error_dest => try c.genLValError(node, lval),
        .range_dest,
        .range_dest_start,
        .range_dest_end,
        .range_dest_step,
        .tuple_dest,
        .tuple_dest_two,
        .list_dest,
        .list_dest_two,
        .map_dest,
        .map_dest_two,
        => @panic("TODO"),
        else => unreachable,
    }
}

fn genLValIdentifier(c: *Compiler, node: Node.Index, lval: Lval, mutable: bool) Error!void {
    const tokens = c.tree.nodes.items(.token);
    switch (lval) {
        .let => |val| {
            try c.checkRedeclaration(tokens[node]);

            var ref = try c.makeRuntime(val.*);
            if (val.* == .mut or mutable) {
                // copy on assign
                ref = try c.addUn(.copy_un, ref);
            }
            const sym = Symbol{
                .name = c.tree.tokenSlice(tokens[node]),
                .ref = ref,
                .mut = mutable,
                .val = val.*,
            };
            try c.scopes.append(c.gpa, .{ .symbol = sym });
        },
        .assign => |val| {
            const sym = try c.findSymbol(tokens[node]);
            if (!sym.mut) {
                return c.reportErr("assignment to constant", node);
            }
            if (val.* == .mut) {
                _ = try c.addBin(.copy, sym.ref, val.mut);
            } else {
                _ = try c.addBin(.move, sym.ref, try c.makeRuntime(val.*));
            }
        },
        .aug_assign => |val| {
            const sym = try c.findSymbol(tokens[node]);
            if (!sym.mut) {
                return c.reportErr("assignment to constant", node);
            }
            val.* = Value{ .mut = sym.ref };
        },
    }
}

fn genLValError(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const val = switch (lval) {
        .let, .assign => |val| val,
        .aug_assign => return c.reportErr("invalid left hand side to augmented assignment", node),
    };
    if (!val.isRt()) {
        return c.reportErr("expected an error", node);
    }
    const unwrapped = try c.addUn(.unwrap_error, val.getRt());

    const rhs_val = Value{ .ref = unwrapped };
    const data = c.tree.nodes.items(.data);
    try c.genLval(data[node].un, switch (lval) {
        .let => .{ .let = &rhs_val },
        .assign => .{ .assign = &rhs_val },
        else => unreachable,
    });
}

fn parseStr(c: *Compiler, tok: TokenIndex) ![]u8 {
    var slice = c.tree.tokenSlice(tok);
    slice = slice[1 .. slice.len - 1];
    var buf = try c.arena.alloc(u8, slice.len);
    return buf[0..try c.parseStrExtra(tok, slice, buf)];
}

fn parseStrExtra(c: *Compiler, tok: TokenIndex, slice: []const u8, buf: []u8) !usize {
    var slice_i: u32 = 0;
    var i: u32 = 0;
    while (slice_i < slice.len) : (slice_i += 1) {
        const char = slice[slice_i];
        switch (char) {
            '\\' => {
                slice_i += 1;
                buf[i] = switch (slice[slice_i]) {
                    '\\' => '\\',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\'' => '\'',
                    '"' => '"',
                    'x', 'u' => return c.reportErr("TODO: more escape sequences", tok),
                    else => unreachable,
                };
            },
            else => buf[i] = char,
        }
        i += 1;
    }
    return i;
}

fn reportErr(c: *Compiler, msg: []const u8, node: Node.Index) Error {
    @setCold(true);
    const starts = c.tree.tokens.items(.start);
    try c.errors.add(.{ .data = msg }, starts[c.tree.firstToken(node)], .err);
    return error.CompileError;
}
