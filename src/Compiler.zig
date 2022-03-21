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

        const val = try compiler.genNode(node);
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

    fn getBool(val: Value, c: *Compiler, tok: TokenIndex) !bool {
        if (val != .Bool) {
            return c.reportErr("expected a boolean", tok);
        }
        return val.Bool;
    }

    fn getInt(val: Value, c: *Compiler, tok: TokenIndex) !i64 {
        if (val != .int) {
            return c.reportErr("expected an integer", tok);
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

    fn getStr(val: Value, c: *Compiler, tok: TokenIndex) ![]const u8 {
        if (val != .str) {
            return c.reportErr("expected a string", tok);
        }
        return val.str;
    }

    fn checkNum(val: Value, c: *Compiler, tok: TokenIndex) !void {
        if (val != .int and val != .num) {
            return c.reportErr("expected a number", tok);
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

fn genNode(c: *Compiler, node: Node.Index) Error!Value {
    const ids = c.tree.nodes.items(.id);
    const tokens = c.tree.nodes.items(.token);
    switch (ids[node]) {
        .string_expr => {
            return Value{ .str = try c.parseStr(tokens[node]) };
        },
        .int_expr => {
            const slice = c.tree.tokenSlice(tokens[node]);
            return Value{
                .int = std.fmt.parseInt(i64, slice, 0) catch
                    return c.reportErr("TODO big int", node),
            };
        },
        .num_expr => {
            const slice = c.tree.tokenSlice(tokens[node]);
            return Value{
                .num = std.fmt.parseFloat(f64, slice) catch unreachable,
            };
        },
        .true_expr => return Value{ .Bool = true },
        .false_expr => return Value{ .Bool = false },
        .null_expr => return Value{ .@"null" = {} },
        .decl_ref_expr => return c.genDeclRef(node),

        .decl => try c.genDecl(node),

        .return_expr => try c.genReturn(node),
        .break_expr => try c.genBreak(node),
        .continue_expr => try c.genContinue(node),
        .for_expr, .for_let_expr => try c.genFor(node),
        .while_expr, .while_let_expr => try c.genWhile(node),
        .if_expr,
        .if_else_expr,
        .if_let_expr,
        .if_let_else_expr,
        => try c.genIf(node),
        .match_expr,
        .match_expr_one,
        => try c.genMatch(node),
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
            return c.genBlock(stmts);
        },

        .this_expr,
        .bool_not_expr,
        .throw_expr,
        .bit_not_expr,
        .negate_expr,
        .plus_expr,
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
        .as_expr,
        .is_expr,
        .paren_expr,
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
    return Value{ .empty = {} };
}

fn genNodeNonEmpty(c: *Compiler, node: Node.Index) Error!Value {
    const val = try c.genNode(node);

    if (val == .empty) {
        return c.reportErr("expected a value", node);
    }
    return val;
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
    const init_val = try c.genNodeNonEmpty(data[node].bin.rhs);
    try c.genLval(data[node].bin.lhs, .{ .let = &init_val });
}

fn genReturn(c: *Compiler, node: Node.Index) !void {
    const data = c.tree.nodes.items(.data);
    if (data[node].un != 0) {
        const operand = try c.genNode(data[node].un);
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

fn genFor(c: *Compiler, node: Node.Index) Error!void {
    const for_expr = Tree.For.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    const cond_val = try c.genNode(for_expr.cond);
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

    const body_val = try c.genNode(for_expr.body);
    if (body_val.isRt()) {
        _ = try c.addUn(.discard, body_val.getRt());
    }

    // jump to the start of the loop
    _ = try c.addInst(.jump, .{ .jump = loop.first_inst });

    // exit loop when IterNext results in None
    c.finishJump(elem_ref);

    for (loop.breaks.items) |@"break"| {
        c.finishJump(@"break");
    }
}

fn genWhile(c: *Compiler, node: Node.Index) Error!void {
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

    const cond_val = try c.genNode(while_expr.cond);
    if (while_expr.capture) |capture| {
        // TODO handle cond_val.isRt()
        const cond_ref = try c.makeRuntime(cond_val);
        // jump past exit loop if cond == null
        cond_jump = try c.addJump(.jump_if_null, cond_ref);

        try c.genLval(capture, .{ .let = &.{ .ref = cond_ref } });
    } else if (cond_val.isRt()) {
        cond_jump = try c.addJump(.jump_if_false, cond_val.getRt());
    } else {
        const bool_val = try cond_val.getBool(c, while_expr.cond);
        if (bool_val == false) {
            // never executed
            return;
        }
    }

    const body_val = try c.genNode(while_expr.body);
    if (body_val.isRt()) {
        _ = try c.addUn(.discard, body_val.getRt());
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
}

fn genIf(c: *Compiler, node: Node.Index) Error!void {
    const if_expr = Tree.If.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    var if_skip: u32 = undefined;

    const cond_val = try c.genNodeNonEmpty(if_expr.cond);
    if (if_expr.capture) |capture| {
        const cond_ref = try c.makeRuntime(cond_val);
        // jump past if_body if cond == .none
        if_skip = try c.addJump(.jump_if_null, cond_ref);

        try c.genLval(capture, .{ .let = &.{ .ref = cond_ref } });
    } else if (!cond_val.isRt()) {
        const bool_val = try cond_val.getBool(c, if_expr.cond);

        if (bool_val) {
            const then_val = try c.genNode(if_expr.then_body);
            if (then_val.isRt()) {
                _ = try c.addUn(.discard, then_val.getRt());
            }
        } else if (if_expr.else_body) |some| {
            const else_val = try c.genNode(some);
            if (else_val.isRt()) {
                _ = try c.addUn(.discard, else_val.getRt());
            }
        }

        return;
    } else {
        // jump past if_body if cond == false
        if_skip = try c.addJump(.jump_if_false, cond_val.getRt());
    }

    const then_val = try c.genNode(if_expr.then_body);
    if (then_val.isRt()) {
        _ = try c.addUn(.discard, then_val.getRt());
    }

    // jump past else_body since if_body was executed
    const else_skip = if (if_expr.else_body != null)
        try c.addUn(.jump, undefined)
    else
        null;

    c.finishJump(if_skip);
    // end capture scope
    c.scopes.items.len = scope_count;

    if (if_expr.else_body) |some| {
        const else_val = try c.genNode(some);
        if (else_val.isRt()) {
            _ = try c.addUn(.discard, else_val.getRt());
        }
    }

    if (else_skip) |some| {
        c.finishJump(some);
    }
}

fn genMatch(c: *Compiler, node: Node.Index) Error!void {
    const ids = c.tree.nodes.items(.id);
    const data = c.tree.nodes.items(.data);
    var buf: [2]Node.Index = undefined;
    const cases = c.tree.nodeItems(node, &buf);

    const cond_val = try c.genNodeNonEmpty(cases[0]);
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
                    const item_val = try c.genNodeNonEmpty(items[0]);
                    const item_ref = try c.makeRuntime(item_val);
                    // if not equal to the error value jump over this handler
                    const eq_ref = try c.addBin(.equal, item_ref, cond_ref);
                    case_skip = try c.addJump(.jump_if_false, eq_ref);
                } else {
                    var success_jumps: JumpList = .{};
                    defer success_jumps.deinit(c.gpa);

                    for (items[0 .. items.len - 1]) |item| {
                        const item_val = try c.genNodeNonEmpty(item);
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

        const case_res = try c.genNode(expr);
        if (case_res.isRt()) {
            _ = try c.addUn(.discard, case_res.getRt());
        }

        // exit match (unless it's this is the last case)
        if (case_i + 2 != cases.len) {
            try jumps.append(c.gpa, try c.addUn(.jump, undefined));
        }

        // jump over this case if the value doesn't match
        if (case_skip) |some| {
            c.finishJump(some);
        }
    }

    // exit match
    for (jumps.items) |jump| {
        c.finishJump(jump);
    }
}

fn genBlock(c: *Compiler, stmts: []const Node.Index) Error!Value {
    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    for (stmts) |stmt, i| {
        // return value of last instruction if it is not discarded
        if (i + 1 == stmts.len) {
            return c.genNode(stmt);
        }

        const val = try c.genNode(stmt);
        if (val.isRt()) {
            // discard unused runtime value
            _ = try c.addUn(.discard, val.getRt());
        }
    }
    return Value{ .@"null" = {} };
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
