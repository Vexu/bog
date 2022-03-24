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
extra: std.ArrayListUnmanaged(Ref) = .{},
strings: std.ArrayListUnmanaged(u8) = .{},
string_interner: std.StringHashMapUnmanaged(u32) = .{},
lines: std.ArrayListUnmanaged(Bytecode.DebugInfo.Line) = .{},

// intermediate
arena: Allocator,
scopes: std.ArrayListUnmanaged(Scope) = .{},
globals: std.ArrayListUnmanaged(Symbol) = .{},
unresolved_globals: std.ArrayListUnmanaged(UnresolvedGlobal) = .{},
list_buf: std.ArrayListUnmanaged(Ref) = .{},
unwrap_jump_buf: JumpList = .{},
cur_loop: ?*Loop = null,
cur_fn: ?*Fn = null,
cur_line: u32 = 1,
prev_line_offset: u32 = 0,

code: *Code,

pub fn compile(gpa: Allocator, source: []const u8, path: []const u8, errors: *Errors) (Compiler.Error || bog.Parser.Error || bog.Tokenizer.Error)!Bytecode {
    const duped_path = try gpa.dupe(u8, path);
    errdefer gpa.free(duped_path);

    var tree = try bog.parse(gpa, source, duped_path, errors);
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
        _ = try compiler.genNode(node, .discard);
    }
    {
        // ensure module ends in a return
        const new_index = compiler.instructions.len;
        const ref = Bytecode.indexToRef(new_index);
        try compiler.instructions.append(gpa, .{ .op = .ret_null, .data = undefined });
        try compiler.code.append(gpa, ref);
    }
    try compiler.resolveGlobals();

    return Bytecode{
        .code = compiler.instructions.toOwnedSlice(),
        .extra = compiler.extra.toOwnedSlice(gpa),
        .strings = compiler.strings.toOwnedSlice(gpa),
        .main = code.toOwnedSlice(gpa),
        .debug_info = .{
            .lines = compiler.lines.toOwnedSlice(gpa),
            .source = source,
            .path = duped_path,
        },
    };
}

pub fn deinit(c: *Compiler) void {
    c.scopes.deinit(c.gpa);
    c.globals.deinit(c.gpa);
    c.unresolved_globals.deinit(c.gpa);
    c.list_buf.deinit(c.gpa);
    c.unwrap_jump_buf.deinit(c.gpa);
    c.instructions.deinit(c.gpa);
    c.extra.deinit(c.gpa);
    c.strings.deinit(c.gpa);
    c.string_interner.deinit(c.gpa);
    c.lines.deinit(c.gpa);
    c.* = undefined;
}

const Code = std.ArrayListUnmanaged(Bytecode.Ref);

const Fn = struct {
    code: Code = .{},
    captures: std.ArrayListUnmanaged(Capture) = .{},

    const Capture = struct {
        name: []const u8,
        local_ref: Ref,
        parent_ref: Ref,
        mut: bool,
    };
};

const UnresolvedGlobal = struct {
    tok: TokenIndex,
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
    breaks: JumpList = .{},
    first_inst: u32,
};

const Try = struct {
    jumps: JumpList = .{},
    err_ref: Ref,
};

const JumpList = std.ArrayListUnmanaged(Ref);

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

    fn checkZero(val: Value, c: *Compiler, node: Node.Index) !void {
        switch (val) {
            .int => |v| if (v != 0) return,
            .num => |v| if (v != 0) return,
            else => unreachable,
        }
        return c.reportErr("division by zero", node);
    }

    fn checkNegative(val: Value, c: *Compiler, node: Node.Index) !void {
        switch (val) {
            .int => |v| if (v < 0) return,
            .num => |v| if (v < 0) return,
            else => unreachable,
        }
        return c.reportErr("remainder division by negative denominator", node);
    }
};

pub const Error = error{CompileError} || Allocator.Error;

fn addInst(c: *Compiler, op: Bytecode.Inst.Op, data: Bytecode.Inst.Data, tok: TokenIndex) !Ref {
    const new_index = c.instructions.len;
    const ref = Bytecode.indexToRef(new_index);
    try c.instructions.append(c.gpa, .{ .op = op, .data = data });
    try c.code.append(c.gpa, ref);

    const starts = c.tree.tokens.items(.start);
    const byte_offset = starts[tok];
    const prev_line = c.cur_line;
    while (c.prev_line_offset < byte_offset) : (c.prev_line_offset += 1) {
        if (c.tree.source[c.prev_line_offset] == '\n') {
            c.cur_line += 1;
        }
    }
    if (prev_line < c.cur_line) {
        try c.lines.append(c.gpa, .{ .line = c.cur_line, .index = @intCast(u32, new_index) });
    }

    return ref;
}

fn addUn(c: *Compiler, op: Bytecode.Inst.Op, arg: Ref, node: Node.Index) !Ref {
    const tok = c.tree.nodes.items(.token)[node];
    return c.addInst(op, .{ .un = arg }, tok);
}

fn addBin(c: *Compiler, op: Bytecode.Inst.Op, lhs: Ref, rhs: Ref, node: Node.Index) !Ref {
    const tok = c.tree.nodes.items(.token)[node];
    return c.addInst(op, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, tok);
}

fn addJump(c: *Compiler, op: Bytecode.Inst.Op, operand: Ref, node: Node.Index) !Ref {
    const tok = c.tree.nodes.items(.token)[node];
    return c.addInst(op, .{
        .jump_condition = .{
            .operand = operand,
            .offset = undefined, // set later
        },
    }, tok);
}

fn addExtra(c: *Compiler, op: Bytecode.Inst.Op, items: []const Ref, node: Node.Index) !Ref {
    const tok = c.tree.nodes.items(.token)[node];
    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.appendSlice(c.gpa, items);
    return c.addInst(op, .{
        .extra = .{
            .extra = extra,
            .len = @intCast(u32, items.len),
        },
    }, tok);
}

fn finishJump(c: *Compiler, jump_ref: Ref) void {
    const offset = @intCast(u32, c.code.items.len);
    const data = c.instructions.items(.data);
    const ops = c.instructions.items(.op);
    const jump_index = Bytecode.refToIndex(jump_ref);
    if (ops[jump_index] == .jump or ops[jump_index] == .pop_err_handler) {
        data[jump_index] = .{ .jump = offset };
    } else {
        data[jump_index].jump_condition.offset = offset;
    }
}

fn makeRuntime(c: *Compiler, val: Value, node: Node.Index) Error!Ref {
    return switch (val) {
        .empty => unreachable,
        .mut, .ref => |ref| ref,
        .@"null" => try c.addInst(.primitive, .{ .primitive = .@"null" }, node),
        .int => |int| try c.addInst(.int, .{ .int = int }, node),
        .num => |num| try c.addInst(.num, .{ .num = num }, node),
        .Bool => |b| try c.addInst(.primitive, .{ .primitive = if (b) .@"true" else .@"false" }, node),
        .str => |str| try c.addInst(.str, .{ .str = .{
            .len = @intCast(u32, str.len),
            .offset = try c.putString(str),
        } }, node),
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
    if (c.cur_fn != null) {
        const name = c.tree.tokenSlice(tok);
        for (c.globals.items) |global| {
            if (mem.eql(u8, global.name, name)) {
                const ref = try c.addInst(.load_global, .{ .un = global.ref }, tok);
                return FoundSymbol{
                    .ref = ref,
                    .mut = global.mut,
                };
            }
        }
    }
    return c.findSymbolExtra(tok, c.scopes.items.len) catch |err| switch (err) {
        error.SymbolNotFound => {
            const ref = try c.addInst(.load_global, undefined, tok);
            try c.unresolved_globals.append(c.gpa, .{ .tok = tok, .ref = ref });
            return FoundSymbol{ .ref = ref, .mut = false, .global = true };
        },
        else => |e| return e,
    };
}

const FindSymbolError = Error || error{SymbolNotFound};
fn findSymbolExtra(c: *Compiler, tok: TokenIndex, start_index: usize) FindSymbolError!FoundSymbol {
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
                const loaded_capture = Bytecode.indexToRef(c.instructions.len);
                try c.instructions.append(c.gpa, .{
                    .op = .load_capture,
                    .data = .{ .un = @intToEnum(Ref, f.captures.items.len) },
                });
                try f.code.append(c.gpa, loaded_capture);
                try f.captures.append(c.gpa, .{
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
    return error.SymbolNotFound;
}

fn checkRedeclaration(c: *Compiler, tok: TokenIndex) !void {
    const name = c.tree.tokenSlice(tok);
    var i = c.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const scope = c.scopes.items[i];
        switch (scope) {
            .symbol => |sym| if (std.mem.eql(u8, sym.name, name)) {
                try c.errors.add(
                    try bog.Value.String.init(c.gpa, "redeclaration of '{s}'", .{name}),
                    c.tree.source,
                    c.tree.path,
                    tok,
                    .err,
                );
                return error.CompileError;
            },
            else => {},
        }
    }
}

fn resolveGlobals(c: *Compiler) !void {
    const data = c.instructions.items(.data);
    for (c.unresolved_globals.items) |unresolved| {
        const name = c.tree.tokenSlice(unresolved.tok);
        for (c.globals.items) |global| {
            if (mem.eql(u8, global.name, name)) {
                const index = Bytecode.refToIndex(unresolved.ref);
                data[index] = .{ .un = global.ref };
                break;
            }
        } else {
            try c.errors.add(
                .{ .data = "use of undeclared identifier" },
                c.tree.source,
                c.tree.path,
                unresolved.tok,
                .err,
            );
            return error.CompileError;
        }
    }
}

fn getLastNode(c: *Compiler, node: Node.Index) Node.Index {
    const data = c.tree.nodes.items(.data);
    const ids = c.tree.nodes.items(.id);
    var cur = node;
    while (true)
        switch (ids[cur]) {
            .paren_expr => cur = data[cur].un,
            else => return cur,
        };
}

const Result = union(enum) {
    /// A runtime value is expected
    ref: Ref,

    /// A value, runtime or constant, is expected
    value,

    /// No value is expected if some is given it will be discarded
    discard,

    /// A returnable value is expected
    ret,

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
        _ = try c.addUn(.discard, val.getRt(), node);
    } else if (res == .ref) {
        const val_ref = try c.makeRuntime(val, node);
        if (val_ref == res.ref) return val;
        if (val == .mut) {
            _ = try c.addBin(.copy, res.ref, val_ref, node);
        } else {
            _ = try c.addBin(.move, res.ref, val_ref, node);
        }
        return Value{ .ref = res.ref };
    } else if (res == .ret) {
        _ = try c.addUn(.ret, try c.makeRuntime(val, node), node);
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
        .ident_expr => {
            const val = try c.genIdent(node);
            return c.wrapResult(node, val, res);
        },
        .discard_expr => {
            return c.reportErr("'_' cannot be used as a value", node);
        },
        .mut_ident_expr => {
            return c.reportErr("'mut' cannot be used as a value", node);
        },
        .this_expr => {
            const res_ref = try c.addUn(.load_this, undefined, node);
            const res_val = Value{ .ref = res_ref };
            return c.wrapResult(node, res_val, res);
        },

        .decl => try c.genDecl(node),

        .throw_expr => try c.genThrow(node),
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
        .try_expr,
        .try_expr_one,
        => return c.genTry(node, res),
        .catch_let_expr,
        .catch_expr,
        => unreachable, // handled in genTry
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
        .bool_and_expr => return c.genBoolAnd(node, res),
        .bool_or_expr => return c.genBoolOr(node, res),
        .less_than_expr,
        .less_than_equal_expr,
        .greater_than_expr,
        .greater_than_equal_expr,
        .equal_expr,
        .not_equal_expr,
        .in_expr,
        => {
            const val = try c.genComparison(node);
            return c.wrapResult(node, val, res);
        },
        .bit_and_expr,
        .bit_or_expr,
        .bit_xor_expr,
        .l_shift_expr,
        .r_shift_expr,
        => {
            const val = try c.genIntArithmetic(node);
            return c.wrapResult(node, val, res);
        },
        .add_expr,
        .sub_expr,
        .mul_expr,
        .div_expr,
        .div_floor_expr,
        .rem_expr,
        .pow_expr,
        => {
            const val = try c.genArithmetic(node);
            return c.wrapResult(node, val, res);
        },
        .assign => return c.genAssign(node, res),
        .add_assign,
        .sub_assign,
        .mul_assign,
        .pow_assign,
        .div_assign,
        .div_floor_assign,
        .rem_assign,
        .l_shift_assign,
        .r_shift_assign,
        .bit_and_assign,
        .bit_or_assign,
        .bit_xor_assign,
        => return c.genAugAssign(node, res),
        .tuple_expr,
        .tuple_expr_two,
        => return c.genTupleList(node, res, .build_tuple),
        .list_expr,
        .list_expr_two,
        => return c.genTupleList(node, res, .build_list),
        .map_expr,
        .map_expr_two,
        => return c.genMap(node, res),
        .map_item_expr => unreachable, // handled in genMap
        .enum_expr => {
            const val = try c.genEnum(node);
            return c.wrapResult(node, val, res);
        },
        .error_expr => {
            const val = try c.genError(node);
            return c.wrapResult(node, val, res);
        },
        .range_expr,
        .range_expr_end,
        .range_expr_step,
        => {
            const val = try c.genRange(node);
            return c.wrapResult(node, val, res);
        },
        .import_expr => {
            const val = try c.genImport(node);
            return c.wrapResult(node, val, res);
        },
        .fn_expr, .fn_expr_one => {
            const val = try c.genFn(node);
            return c.wrapResult(node, val, res);
        },
        .call_expr,
        .call_expr_one,
        => {
            const val = try c.genCall(node);
            return c.wrapResult(node, val, res);
        },
        .member_access_expr => {
            const val = try c.genMemberAccess(node);
            return c.wrapResult(node, val, res);
        },
        .array_access_expr => {
            const val = try c.genArrayAccess(node);
            return c.wrapResult(node, val, res);
        },
        .format_expr => {
            const val = try c.genFormatString(node);
            return c.wrapResult(node, val, res);
        },
    }
    return c.wrapResult(node, .empty, res);
}

fn genIdent(c: *Compiler, node: Node.Index) Error!Value {
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
    const destructuring = data[node].bin.lhs;
    const ids = c.tree.nodes.items(.id);

    const last_node = c.getLastNode(destructuring);
    if (ids[last_node] == .discard_expr) {
        return c.reportErr(
            "'_' cannot be used directly in variable initialization",
            last_node,
        );
    }
    try c.genLval(destructuring, .{ .let = &init_val });
}

fn genThrow(c: *Compiler, node: Node.Index) !void {
    const data = c.tree.nodes.items(.data);
    const operand = data[node].un;
    const operand_val = try c.genNode(operand, .value);
    const operand_ref = try c.makeRuntime(operand_val, operand);
    _ = try c.addUn(.throw, operand_ref, node);
}

fn genReturn(c: *Compiler, node: Node.Index) !void {
    const data = c.tree.nodes.items(.data);
    if (data[node].un != 0) {
        // handled by result location
        _ = try c.genNode(data[node].un, .ret);
    } else {
        _ = try c.addUn(.ret_null, undefined, node);
    }
}

fn genBreak(c: *Compiler, node: Node.Index) !void {
    const loop = c.cur_loop orelse
        return c.reportErr("break outside of loop", node);

    const jump = try c.addInst(.jump, undefined, node);
    try loop.breaks.append(c.gpa, jump);
}

fn genContinue(c: *Compiler, node: Node.Index) !void {
    const loop = c.cur_loop orelse
        return c.reportErr("continue outside of loop", node);

    const tok = c.tree.nodes.items(.token)[node];
    _ = try c.addInst(.jump, .{ .jump = loop.first_inst }, tok);
}

fn createListComprehension(c: *Compiler, ref: ?Ref, node: Node.Index) !Result {
    const list = try c.addExtra(.build_list, &.{}, node);
    if (ref) |some| {
        _ = try c.addBin(.move, some, list, node);
        return Result{ .ref = some };
    } else {
        return Result{ .ref = list };
    }
}

fn genFor(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const sub_res = switch (res) {
        .discard => res,
        .value, .ret => try c.createListComprehension(null, node),
        .ref => |ref| try c.createListComprehension(ref, node),
    };
    const for_expr = Tree.For.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    const cond_val = try c.genNode(for_expr.cond, .value);
    if (!cond_val.isRt() and cond_val != .str)
        return c.reportErr("expected iterable value", for_expr.cond);

    const cond_ref = try c.makeRuntime(cond_val, for_expr.cond);

    // create the iterator
    const iter_ref = try c.addUn(.iter_init, cond_ref, for_expr.cond);
    var loop = Loop{
        .first_inst = @intCast(u32, c.code.items.len),
    };
    defer loop.breaks.deinit(c.gpa);

    const old_loop = c.cur_loop;
    defer c.cur_loop = old_loop;
    c.cur_loop = &loop;

    // iter next is fused with a jump_null, offset is set after body is generated
    const elem_ref = try c.addJump(.iter_next, iter_ref, for_expr.cond);

    if (for_expr.capture) |some| {
        try c.genLval(some, .{ .let = &.{ .ref = elem_ref } });
    }

    switch (sub_res) {
        .discard => _ = try c.genNode(for_expr.body, .discard),
        .ref => |list| {
            const body_val = try c.genNode(for_expr.body, .value);
            const body_ref = try c.makeRuntime(body_val, for_expr.body);
            _ = try c.addBin(.append, list, body_ref, for_expr.body);
        },
        else => unreachable,
    }

    // jump to the start of the loop
    _ = try c.addInst(.jump, .{ .jump = loop.first_inst }, for_expr.body);

    // exit loop when IterNext results in None
    c.finishJump(elem_ref);

    for (loop.breaks.items) |@"break"| {
        c.finishJump(@"break");
    }
    if (res == .ret) {
        _ = try c.addUn(.ret, sub_res.ref, for_expr.body);
        return Value.empty;
    } else {
        return sub_res.toVal();
    }
}

fn genWhile(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const sub_res = switch (res) {
        .discard => res,
        .value, .ret => try c.createListComprehension(null, node),
        .ref => |ref| try c.createListComprehension(ref, node),
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
            cond_jump = try c.addJump(.jump_if_null, cond_val.getRt(), while_expr.cond);
        } else if (cond_val == .@"null") {
            // never executed
            return sub_res.toVal();
        }
        const cond_ref = try c.makeRuntime(cond_val, while_expr.cond);

        try c.genLval(capture, .{ .let = &.{ .ref = cond_ref } });
    } else if (cond_val.isRt()) {
        cond_jump = try c.addJump(.jump_if_false, cond_val.getRt(), while_expr.cond);
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
            const body_ref = try c.makeRuntime(body_val, while_expr.body);
            _ = try c.addBin(.append, list, body_ref, while_expr.body);
        },
        else => unreachable,
    }

    // jump to the start of the loop
    _ = try c.addInst(.jump, .{ .jump = loop.first_inst }, while_expr.body);

    // exit loop if cond == false
    if (cond_jump) |some| {
        c.finishJump(some);
    }

    for (loop.breaks.items) |@"break"| {
        c.finishJump(@"break");
    }

    if (res == .ret) {
        _ = try c.addUn(.ret, sub_res.ref, while_expr.body);
        return Value.empty;
    } else {
        return sub_res.toVal();
    }
}

fn genIf(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const if_expr = Tree.If.get(c.tree.*, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    const jump_buf_top = c.unwrap_jump_buf.items.len;
    defer c.unwrap_jump_buf.items.len = jump_buf_top;

    const cond_val = try c.genNode(if_expr.cond, .value);
    if (if_expr.capture) |capture| {
        const cond_ref = try c.makeRuntime(cond_val, if_expr.cond);
        try c.genTryUnwrap(capture, &.{ .ref = cond_ref });
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
        const skip_jump = try c.addJump(.jump_if_false, cond_val.getRt(), if_expr.cond);
        try c.unwrap_jump_buf.append(c.gpa, skip_jump);
    }
    const sub_res = switch (res) {
        .ref, .discard, .ret => res,
        .value => val: {
            // add a dummy instruction we can store the value into
            const res_ref = Bytecode.indexToRef(c.instructions.len);
            try c.instructions.append(c.gpa, undefined);
            break :val Result{ .ref = res_ref };
        },
    };

    // sub_res is either ref or discard, either way wrapResult handles it
    _ = try c.genNode(if_expr.then_body, sub_res);

    // jump past else_body since if_body was executed
    const else_skip = if (if_expr.else_body != null or sub_res == .ref)
        try c.addUn(.jump, undefined, if_expr.then_body)
    else
        null;

    for (c.unwrap_jump_buf.items[jump_buf_top..]) |skip| {
        c.finishJump(skip);
    }

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
        .ref, .discard, .ret => res,
        .value => val: {
            // add a dummy instruction we can store the value into
            const res_ref = Bytecode.indexToRef(c.instructions.len);
            try c.instructions.append(c.gpa, undefined);
            break :val Result{ .ref = res_ref };
        },
    };

    const ids = c.tree.nodes.items(.id);
    const data = c.tree.nodes.items(.data);
    var buf: [2]Node.Index = undefined;
    const cases = c.tree.nodeItems(node, &buf);

    const cond_val = try c.genNode(cases[0], .value);
    const cond_ref = try c.makeRuntime(cond_val, cases[0]);

    const jump_buf_start = c.unwrap_jump_buf.items.len;
    var jump_buf_top = jump_buf_start;
    defer c.unwrap_jump_buf.items.len = jump_buf_start;

    var seen_catch_all = false;
    for (cases[1..]) |case, case_i| {
        if (seen_catch_all) {
            return c.reportErr("additional cases after catch-all case", case);
        }

        const scope_count = c.scopes.items.len;
        defer c.scopes.items.len = scope_count;

        var expr: Node.Index = undefined;
        c.unwrap_jump_buf.items.len = jump_buf_top;
        switch (ids[case]) {
            .match_case_catch_all => {
                seen_catch_all = true;
                expr = data[case].un;
            },
            .match_case_let => {
                const capture = data[case].bin.lhs;
                expr = data[case].bin.rhs;
                switch (ids[c.getLastNode(capture)]) {
                    .ident_expr, .mut_ident_expr => seen_catch_all = true,
                    .discard_expr => return c.reportErr("use plain '_' instead of 'let _'", capture),
                    else => {},
                }
                try c.genTryUnwrap(capture, &.{ .ref = cond_ref });
            },
            .match_case,
            .match_case_one,
            => {
                var buf_2: [2]Node.Index = undefined;
                const items = c.tree.nodeItems(case, &buf_2);
                expr = items[items.len - 1];

                if (items.len == 2) {
                    const item_val = try c.genNode(items[0], .value);
                    const item_ref = try c.makeRuntime(item_val, items[0]);
                    // if not equal to the error value jump over this handler
                    const eq_ref = try c.addBin(.equal, item_ref, cond_ref, items[0]);
                    try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_false, eq_ref, items[0]));
                } else {
                    for (items[0 .. items.len - 1]) |item| {
                        const item_val = try c.genNode(item, .value);
                        const item_ref = try c.makeRuntime(item_val, item);

                        const eq_ref = try c.addBin(.equal, item_ref, cond_ref, item);
                        try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_true, eq_ref, item));
                    }
                    const exit_jump = try c.addUn(.jump, undefined, node);

                    for (c.unwrap_jump_buf.items[jump_buf_top..]) |some| {
                        c.finishJump(some);
                    }

                    c.unwrap_jump_buf.items.len = jump_buf_top;
                    try c.unwrap_jump_buf.append(c.gpa, exit_jump);
                }
            },
            else => unreachable,
        }

        // sub_res is either ref or discard, either way wrapResult handles it
        _ = try c.genNode(expr, sub_res);

        // exit match (unless it's this is the last case)
        const exit_jump = if (case_i + 2 != cases.len)
            try c.addUn(.jump, undefined, expr)
        else
            null;

        // jump over this case if the value doesn't match
        for (c.unwrap_jump_buf.items[jump_buf_top..]) |some| {
            c.finishJump(some);
        }

        c.unwrap_jump_buf.items.len = jump_buf_top;
        if (exit_jump) |some| {
            try c.unwrap_jump_buf.append(c.gpa, some);
            jump_buf_top += 1;
        }
    }

    if (!seen_catch_all) {
        const res_val = Value{ .@"null" = {} };
        _ = try c.wrapResult(node, res_val, sub_res);
    }

    // exit match
    for (c.unwrap_jump_buf.items[jump_buf_start..]) |jump| {
        c.finishJump(jump);
    }
    return sub_res.toVal();
}

fn genTry(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const data = c.tree.nodes.items(.data);
    const ids = c.tree.nodes.items(.id);
    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);
    const cond = items[0];
    const catches = items[1..];

    const sub_res = switch (res) {
        .ref, .discard, .ret => res,
        .value => val: {
            // add a dummy instruction we can store the value into
            const res_ref = Bytecode.indexToRef(c.instructions.len);
            try c.instructions.append(c.gpa, undefined);
            break :val Result{ .ref = res_ref };
        },
    };

    const err_ref = err_ref: {
        // add a dummy instruction for the thrown value
        const res_ref = Bytecode.indexToRef(c.instructions.len);
        try c.instructions.append(c.gpa, undefined);
        break :err_ref res_ref;
    };
    const err_handler_inst = try c.addJump(.push_err_handler, err_ref, node);

    _ = try c.genNode(cond, sub_res);

    // no longer in try scope
    c.finishJump(err_handler_inst);

    // if no error jump over all catchers
    const skip_all = try c.addUn(.pop_err_handler, undefined, node);

    const scope_count = c.scopes.items.len;
    defer c.scopes.items.len = scope_count;

    const jump_buf_start = c.unwrap_jump_buf.items.len;
    var jump_buf_top = jump_buf_start;
    defer c.unwrap_jump_buf.items.len = jump_buf_start;

    try c.unwrap_jump_buf.append(c.gpa, skip_all);
    jump_buf_top += 1;

    var seen_catch_all = false;
    for (catches) |catcher, catcher_i| {
        if (seen_catch_all) {
            return c.reportErr("additional handlers after a catch-all handler", catcher);
        }

        c.unwrap_jump_buf.items.len = jump_buf_top;
        const capture = data[catcher].bin.lhs;
        if (capture != 0) {
            if (ids[catcher] == .catch_let_expr) {
                switch (ids[c.getLastNode(capture)]) {
                    .ident_expr, .mut_ident_expr => seen_catch_all = true,
                    .discard_expr => return c.reportErr("use plain 'catch' instead of 'catch let _'", capture),
                    else => {},
                }
                try c.genTryUnwrap(capture, &.{ .ref = err_ref });
            } else {
                const capture_val = try c.genNode(capture, .value);
                const capture_ref = try c.makeRuntime(capture_val, capture);
                // if not equal to the error value jump over this handler
                const eq_ref = try c.addBin(.equal, capture_ref, err_ref, capture);
                try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_false, eq_ref, capture));
            }
        } else {
            seen_catch_all = true;
        }

        const expr = data[catcher].bin.rhs;
        _ = try c.genNode(expr, sub_res);

        var exit_handler: ?Ref = null;

        // exit this handler (unless it's the last one)
        if (catcher_i + 1 != catches.len) {
            exit_handler = try c.addUn(.jump, undefined, expr);
        }

        // jump over this handler if the value doesn't match
        for (c.unwrap_jump_buf.items[jump_buf_top..]) |some| {
            c.finishJump(some);
        }

        if (exit_handler) |some| {
            try c.unwrap_jump_buf.append(c.gpa, some);
            jump_buf_top += 1;
        }
    }

    // return uncaught errors
    if (!seen_catch_all) {
        _ = try c.addUn(.ret, err_ref, node);
    }

    // exit try-catch
    for (c.unwrap_jump_buf.items[jump_buf_start..]) |jump| {
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

    switch (type_id) {
        .@"null", .int, .num, .bool, .str, .tuple, .map, .list => {},
        else => return c.reportErr("invalid cast type", ty_tok),
    }

    if (lhs.isRt()) {
        const cast_ref = try c.addInst(.as, .{ .bin_ty = .{
            .operand = lhs.getRt(),
            .ty = type_id,
        } }, ty_tok);
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
                .int => |val| @intToFloat(f64, val),
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
        } }, ty_tok);
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
        const ref = try c.addUn(.bool_not, operand.getRt(), node);
        return Value{ .ref = ref };
    }
    return Value{ .Bool = !try operand.getBool(c, data[node].un) };
}

fn genBitNot(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const operand = try c.genNode(data[node].un, .value);

    if (operand.isRt()) {
        const ref = try c.addUn(.bit_not, operand.getRt(), node);
        return Value{ .ref = ref };
    }
    return Value{ .int = ~try operand.getInt(c, data[node].un) };
}

fn genNegate(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const operand = try c.genNode(data[node].un, .value);

    if (operand.isRt()) {
        const ref = try c.addUn(.negate, operand.getRt(), node);
        return Value{ .ref = ref };
    }

    try operand.checkNum(c, data[node].un);
    if (operand == .int) {
        return Value{ .int = -operand.int };
    } else {
        return Value{ .num = -operand.num };
    }
}

fn needNum(a: Value, b: Value) bool {
    return a == .num or b == .num;
}

fn genBoolAnd(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    var lhs_val = try c.genNode(lhs, .value);

    if (!lhs_val.isRt()) {
        const l_bool = try lhs_val.getBool(c, lhs);
        if (!l_bool) return lhs_val;
        return c.genNode(rhs, res);
    }

    if (res == .ret) {
        const lhs_ref = try c.makeRuntime(lhs_val, lhs);

        const ret_skip = try c.addJump(.jump_if_true, lhs_ref, lhs);
        _ = try c.addUn(.ret, lhs_ref, lhs);
        c.finishJump(ret_skip);

        _ = try c.genNode(rhs, res);
        return Value{ .empty = {} };
    }

    const lhs_ref = if (lhs_val == .mut)
        try c.addUn(.copy_un, lhs_val.mut, lhs)
    else
        try c.makeRuntime(lhs_val, lhs);

    const rhs_skip = try c.addJump(.jump_if_false, lhs_ref, lhs);

    _ = try c.genNode(rhs, .{ .ref = lhs_ref });
    c.finishJump(rhs_skip);

    return Value{ .ref = lhs_ref };
}

fn genBoolOr(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    var lhs_val = try c.genNode(lhs, .value);

    if (!lhs_val.isRt()) {
        const l_bool = try lhs_val.getBool(c, lhs);
        if (l_bool) return lhs_val;
        return c.genNode(rhs, res);
    }

    if (res == .ret) {
        const lhs_ref = try c.makeRuntime(lhs_val, lhs);

        const ret_skip = try c.addJump(.jump_if_false, lhs_ref, lhs);
        _ = try c.addUn(.ret, lhs_ref, lhs);
        c.finishJump(ret_skip);

        _ = try c.genNode(rhs, res);
        return Value{ .empty = {} };
    }

    const lhs_ref = if (lhs_val == .mut)
        try c.addUn(.copy_un, lhs_val.mut, lhs)
    else
        try c.makeRuntime(lhs_val, lhs);

    const rhs_skip = try c.addJump(.jump_if_true, lhs_ref, lhs);

    _ = try c.genNode(rhs, .{ .ref = lhs_ref });
    c.finishJump(rhs_skip);

    return Value{ .ref = lhs_ref };
}

fn genComparison(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    var lhs_val = try c.genNode(lhs, .value);
    var rhs_val = try c.genNode(rhs, .value);

    const op: Bytecode.Inst.Op = switch (c.tree.nodes.items(.id)[node]) {
        .less_than_expr => .less_than,
        .less_than_equal_expr => .less_than_equal,
        .greater_than_expr => .greater_than,
        .greater_than_equal_expr => .greater_than_equal,
        .equal_expr => .equal,
        .not_equal_expr => .not_equal,
        .in_expr => .in,
        else => unreachable,
    };

    if (rhs_val.isRt() or lhs_val.isRt()) {
        const lhs_ref = try c.makeRuntime(lhs_val, lhs);
        const rhs_ref = try c.makeRuntime(rhs_val, rhs);

        const ref = try c.addBin(op, lhs_ref, rhs_ref, node);
        return Value{ .ref = ref };
    }

    // order comparisons are only allowed on numbers
    switch (op) {
        .in, .equal, .not_equal => {},
        else => {
            try lhs_val.checkNum(c, lhs);
            try rhs_val.checkNum(c, rhs);
        },
    }

    switch (op) {
        .less_than => return Value{
            .Bool = if (needNum(lhs_val, rhs_val))
                lhs_val.getNum() < rhs_val.getNum()
            else
                lhs_val.int < rhs_val.int,
        },
        .less_than_equal => return Value{
            .Bool = if (needNum(lhs_val, rhs_val))
                lhs_val.getNum() <= rhs_val.getNum()
            else
                lhs_val.int <= rhs_val.int,
        },
        .greater_than => return Value{
            .Bool = if (needNum(lhs_val, rhs_val))
                lhs_val.getNum() > rhs_val.getNum()
            else
                lhs_val.int > rhs_val.int,
        },
        .greater_than_equal => return Value{
            .Bool = if (needNum(lhs_val, rhs_val))
                lhs_val.getNum() >= rhs_val.getNum()
            else
                lhs_val.int >= rhs_val.int,
        },
        .equal, .not_equal => {
            const eql = switch (lhs_val) {
                .@"null" => rhs_val == .@"null",
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
                .empty, .mut, .ref => unreachable,
            };
            return Value{ .Bool = if (op == .equal) eql else !eql };
        },
        .in => return Value{
            .Bool = switch (lhs_val) {
                .str => mem.indexOf(
                    u8,
                    try lhs_val.getStr(c, lhs),
                    try rhs_val.getStr(c, rhs),
                ) != null,
                else => unreachable,
            },
        },
        else => unreachable,
    }
}

fn genIntArithmetic(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    var lhs_val = try c.genNode(lhs, .value);
    var rhs_val = try c.genNode(rhs, .value);

    const op: Bytecode.Inst.Op = switch (c.tree.nodes.items(.id)[node]) {
        .bit_and_expr => .bit_and,
        .bit_or_expr => .bit_or,
        .bit_xor_expr => .bit_xor,
        .l_shift_expr => .l_shift,
        .r_shift_expr => .r_shift,
        else => unreachable,
    };

    if (lhs_val.isRt() or rhs_val.isRt()) {
        const lhs_ref = try c.makeRuntime(lhs_val, lhs);
        const rhs_ref = try c.makeRuntime(rhs_val, rhs);

        const ref = try c.addBin(op, lhs_ref, rhs_ref, node);
        return Value{ .ref = ref };
    }
    const l_int = try lhs_val.getInt(c, lhs);
    const r_int = try rhs_val.getInt(c, rhs);

    switch (op) {
        .bit_and => return Value{ .int = l_int & r_int },
        .bit_or => return Value{ .int = l_int | r_int },
        .bit_xor => return Value{ .int = l_int ^ r_int },
        .l_shift => {
            if (r_int < 0)
                return c.reportErr("shift by negative amount", rhs);
            const val = if (r_int > std.math.maxInt(u6))
                0
            else
                l_int << @truncate(u6, @bitCast(u64, r_int));
            return Value{ .int = val };
        },
        .r_shift => {
            if (r_int < 0)
                return c.reportErr("shift by negative amount", rhs);
            const val = if (r_int > std.math.maxInt(u6))
                if (l_int < 0) std.math.maxInt(i64) else @as(i64, 0)
            else
                l_int >> @truncate(u6, @bitCast(u64, r_int));
            return Value{ .int = val };
        },
        else => unreachable,
    }
}

fn genArithmetic(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    var lhs_val = try c.genNode(lhs, .value);
    var rhs_val = try c.genNode(rhs, .value);

    const op: Bytecode.Inst.Op = switch (c.tree.nodes.items(.id)[node]) {
        .add_expr => .add,
        .sub_expr => .sub,
        .mul_expr => .mul,
        .div_expr => .div,
        .div_floor_expr => .div_floor,
        .rem_expr => .rem,
        .pow_expr => .pow,
        else => unreachable,
    };

    if (!rhs_val.isRt() and !lhs_val.isRt()) rt: {
        try lhs_val.checkNum(c, lhs);
        try rhs_val.checkNum(c, rhs);

        switch (op) {
            .add => {
                if (needNum(lhs_val, rhs_val)) {
                    return Value{ .num = lhs_val.getNum() + rhs_val.getNum() };
                }
                return Value{
                    .int = std.math.add(i64, lhs_val.int, rhs_val.int) catch break :rt,
                };
            },
            .sub => {
                if (needNum(lhs_val, rhs_val)) {
                    return Value{ .num = lhs_val.getNum() - rhs_val.getNum() };
                }
                return Value{
                    .int = std.math.sub(i64, lhs_val.int, rhs_val.int) catch break :rt,
                };
            },
            .mul => {
                if (needNum(lhs_val, rhs_val)) {
                    return Value{ .num = lhs_val.getNum() * rhs_val.getNum() };
                }
                return Value{
                    .int = std.math.mul(i64, lhs_val.int, rhs_val.int) catch break :rt,
                };
            },
            .div => {
                try rhs_val.checkZero(c, rhs);
                return Value{ .num = lhs_val.getNum() / rhs_val.getNum() };
            },
            .div_floor => {
                try rhs_val.checkZero(c, rhs);
                if (needNum(lhs_val, rhs_val)) {
                    return Value{ .int = std.math.lossyCast(i64, @divFloor(lhs_val.getNum(), rhs_val.getNum())) };
                }
                return Value{
                    .int = std.math.divFloor(i64, lhs_val.int, rhs_val.int) catch break :rt,
                };
            },
            .rem => {
                try rhs_val.checkZero(c, rhs);
                try rhs_val.checkNegative(c, rhs);
                if (needNum(lhs_val, rhs_val)) {
                    return Value{ .num = @rem(lhs_val.getNum(), rhs_val.getNum()) };
                }
                return Value{
                    .int = @rem(lhs_val.int, rhs_val.int),
                };
            },
            .pow => {
                if (needNum(lhs_val, rhs_val)) {
                    return Value{ .num = std.math.pow(f64, lhs_val.getNum(), rhs_val.getNum()) };
                }
                return Value{
                    .int = std.math.powi(i64, lhs_val.int, rhs_val.int) catch break :rt,
                };
            },
            else => unreachable,
        }
    }

    const lhs_ref = try c.makeRuntime(lhs_val, lhs);
    const rhs_ref = try c.makeRuntime(rhs_val, rhs);

    const ref = try c.addBin(op, lhs_ref, rhs_ref, node);
    return Value{ .ref = ref };
}

fn genAssign(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    if (res != .discard) {
        return c.reportErr("assignment produces no value", node);
    }
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    const rhs_val = try c.genNode(rhs, .value);

    try c.genLval(lhs, .{ .assign = &rhs_val });
    return .empty;
}

fn genAugAssign(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    if (res != .discard) {
        return c.reportErr("assignment produces no value", node);
    }
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;
    const rhs_val = try c.genNode(rhs, .value);

    const op: Bytecode.Inst.Op = switch (c.tree.nodes.items(.id)[node]) {
        .add_assign => .add,
        .sub_assign => .sub,
        .mul_assign => .mul,
        .pow_assign => .pow,
        .div_assign => .div,
        .div_floor_assign => .div_floor,
        .rem_assign => .rem,
        .l_shift_assign => .l_shift,
        .r_shift_assign => .r_shift,
        .bit_and_assign => .bit_and,
        .bit_or_assign => .bit_or,
        .bit_xor_assign => .bit_xor,
        else => unreachable,
    };

    var lhs_ref: Ref = undefined;
    try c.genLval(lhs, .{ .aug_assign = &lhs_ref });
    if (!rhs_val.isRt()) switch (op) {
        // zig fmt: off
        .add, .sub, .mul, .pow, .div, .div_floor, .rem,
        => try rhs_val.checkNum(c, rhs),
        .l_shift, .r_shift, .bit_and, .bit_or, .bit_xor,
        => _ = try rhs_val.getInt(c, rhs),
        // zig fmt: on
        else => unreachable,
    };

    const rhs_ref = try c.makeRuntime(rhs_val, rhs);
    const res_ref = try c.addBin(op, lhs_ref, rhs_ref, node);
    _ = try c.addBin(.move, lhs_ref, res_ref, node);
    return Value.empty;
}

fn genTupleList(
    c: *Compiler,
    node: Node.Index,
    res: Result,
    op: Bytecode.Inst.Op,
) Error!Value {
    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);

    const list_buf_top = c.list_buf.items.len;
    defer c.list_buf.items.len = list_buf_top;

    if (res == .discard) {
        for (items) |val| {
            _ = try c.genNode(val, .discard);
        }
        return Value{ .empty = {} };
    }

    for (items) |val| {
        const item_val = try c.genNode(val, .value);
        const item_ref = try c.makeRuntime(item_val, val);

        try c.list_buf.append(c.gpa, item_ref);
    }

    const ref = try c.addExtra(op, c.list_buf.items[list_buf_top..], node);
    return c.wrapResult(node, Value{ .ref = ref }, res);
}

fn genMap(c: *Compiler, node: Node.Index, res: Result) Error!Value {
    const data = c.tree.nodes.items(.data);
    const tokens = c.tree.nodes.items(.token);
    const tok_ids = c.tree.tokens.items(.id);
    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);

    const list_buf_top = c.list_buf.items.len;
    defer c.list_buf.items.len = list_buf_top;

    if (res == .discard) {
        for (items) |item| {
            if (data[item].bin.lhs != 0) {
                const last_node = c.getLastNode(data[item].bin.lhs);
                if (tok_ids[tokens[last_node]] != .identifier) {
                    _ = try c.genNode(data[item].bin.lhs, .discard);
                }
            }

            _ = try c.genNode(data[item].bin.lhs, .discard);
        }
        return Value{ .empty = {} };
    }

    for (items) |item| {
        var key: Ref = undefined;
        if (data[item].bin.lhs != 0) {
            const last_node = c.getLastNode(data[item].bin.lhs);
            const maybe_ident = c.tree.firstToken(last_node);
            if (tok_ids[maybe_ident] == .identifier) {
                // `ident = value` is equal to `"ident" = value`
                const str = c.tree.tokenSlice(maybe_ident);
                key = try c.addInst(.str, .{ .str = .{
                    .len = @intCast(u32, str.len),
                    .offset = try c.putString(str),
                } }, maybe_ident);
            } else {
                var key_val = try c.genNode(data[item].bin.lhs, .value);
                key = try c.makeRuntime(key_val, data[item].bin.lhs);
            }
        } else {
            const last_node = c.getLastNode(data[item].bin.rhs);
            const maybe_ident = c.tree.firstToken(last_node);
            if (tok_ids[maybe_ident] != .identifier) {
                return c.reportErr("expected a key", item);
            }
            // `ident` is equal to `"ident" = ident`
            const str = c.tree.tokenSlice(maybe_ident);
            key = try c.addInst(.str, .{ .str = .{
                .len = @intCast(u32, str.len),
                .offset = try c.putString(str),
            } }, maybe_ident);
        }

        var value_val = try c.genNode(data[item].bin.lhs, .value);
        const value_ref = try c.makeRuntime(value_val, data[item].bin.rhs);
        try c.list_buf.appendSlice(c.gpa, &.{ key, value_ref });
    }

    const ref = try c.addExtra(.build_map, c.list_buf.items[list_buf_top..], node);
    return c.wrapResult(node, Value{ .ref = ref }, res);
}

fn genEnum(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const tokens = c.tree.nodes.items(.token);
    const str = c.tree.tokenSlice(tokens[node]);
    const operand = data[node].un;
    if (operand == 0) {
        const res_ref = try c.addInst(.build_tagged_null, .{ .str = .{
            .len = @intCast(u32, str.len),
            .offset = try c.putString(str),
        } }, tokens[node]);
        return Value{ .ref = res_ref };
    }
    const operand_val = try c.genNode(operand, .value);
    const operand_ref = try c.makeRuntime(operand_val, operand);

    const str_offset = try c.putString(str);

    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.append(c.gpa, operand_ref);
    try c.extra.append(c.gpa, @intToEnum(Ref, str_offset));
    const res_ref = try c.addInst(.build_tagged, .{
        .extra = .{
            .extra = extra,
            .len = @intCast(u32, str.len),
        },
    }, tokens[node]);
    return Value{ .ref = res_ref };
}

fn genError(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const operand = data[node].un;
    if (operand == 0) {
        const ref = try c.addUn(.build_error_null, undefined, node);
        return Value{ .ref = ref };
    }
    const operand_val = try c.genNode(operand, .value);
    if (operand_val == .@"null") {
        const ref = try c.addUn(.build_error_null, undefined, node);
        return Value{ .ref = ref };
    }
    const operand_ref = try c.makeRuntime(operand_val, operand);

    const ref = try c.addUn(.build_error, operand_ref, node);
    return Value{ .ref = ref };
}

fn genRange(c: *Compiler, node: Node.Index) Error!Value {
    const range = Tree.Range.get(c.tree.*, node);

    const start_val = try c.genNode(range.start, .value);
    if (!start_val.isRt()) _ = try start_val.getInt(c, range.start);
    const start_ref = try c.makeRuntime(start_val, range.start);

    var end_val = Value{ .int = std.math.maxInt(i64) };
    if (range.end) |some| {
        end_val = try c.genNode(some, .value);
        if (!end_val.isRt()) _ = try end_val.getInt(c, some);
    }
    const end_ref = try c.makeRuntime(end_val, range.end orelse node);

    const step = range.step orelse {
        const res_ref = try c.addBin(.build_range, start_ref, end_ref, node);
        return Value{ .ref = res_ref };
    };

    const step_val = try c.genNode(step, .value);
    if (!step_val.isRt()) _ = try step_val.getInt(c, step);
    const step_ref = try c.makeRuntime(step_val, step);

    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.append(c.gpa, end_ref);
    try c.extra.append(c.gpa, step_ref);
    const res_ref = try c.addInst(.build_range_step, .{
        .range = .{
            .start = start_ref,
            .extra = extra,
        },
    }, c.tree.nodes.items(.token)[node]);
    return Value{ .ref = res_ref };
}

fn genImport(c: *Compiler, node: Node.Index) Error!Value {
    const tokens = c.tree.nodes.items(.token);
    const str = try c.parseStr(tokens[node]);

    const res_ref = try c.addInst(.import, .{ .str = .{
        .len = @intCast(u32, str.len),
        .offset = try c.putString(str),
    } }, tokens[node]);
    return Value{ .ref = res_ref };
}

fn genFn(c: *Compiler, node: Node.Index) Error!Value {
    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);
    const params = items[@boolToInt(items[0] == 0) .. items.len - 1];
    const body = items[items.len - 1];

    if (params.len > Bytecode.max_params) {
        return c.reportErr("too many parameters", node);
    }

    var func = Fn{};
    defer func.code.deinit(c.gpa);
    defer func.captures.deinit(c.gpa);

    const old_code = c.code;
    const scope_count = c.scopes.items.len;
    const old_loop = c.cur_loop;
    const old_fn = c.cur_fn;
    defer {
        c.code = old_code;
        c.scopes.items.len = scope_count;
        c.cur_loop = old_loop;
        c.cur_fn = old_fn;
    }
    c.code = &func.code;
    c.cur_loop = null;
    c.cur_fn = &func;

    try c.scopes.append(c.gpa, .{ .func = &func });

    // destructure parameters
    for (params) |param, i| {
        try c.genLval(param, .{ .let = &.{ .ref = @intToEnum(Ref, i) } });
    }

    // for one liner functions return the value of the expression,
    // otherwise require an explicit return statement
    const last = c.getLastNode(body);
    const ids = c.tree.nodes.items(.id);
    const sub_res: Result = switch (ids[last]) {
        // zig fmt: off
        .block_stmt_two, .block_stmt, .assign, .add_assign, .sub_assign, .mul_assign,
        .pow_assign, .div_assign, .div_floor_assign, .rem_assign, .l_shift_assign,
        .r_shift_assign, .bit_and_assign, .bit_or_assign, .bit_xor_assign => .discard,
        // zig fmt: on
        else => .value,
    };

    const body_val = try c.genNode(body, sub_res);
    if (body_val == .empty or body_val == .@"null") {
        _ = try c.addUn(.ret_null, undefined, body);
    } else {
        const body_ref = try c.makeRuntime(body_val, body);
        _ = try c.addUn(.ret, body_ref, body);
    }

    // done generating the new function
    c.code = old_code;

    const fn_info = Bytecode.Inst.Data.FnInfo{
        .args = @intCast(u8, params.len),
        .captures = @intCast(u24, func.captures.items.len),
    };

    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.append(c.gpa, @intToEnum(Ref, @bitCast(u32, fn_info)));
    try c.extra.appendSlice(c.gpa, func.code.items);
    const func_ref = try c.addInst(.build_func, .{
        .extra = .{
            .extra = extra,
            .len = @intCast(u32, func.code.items.len + 1),
        },
    }, c.tree.nodes.items(.token)[node]);

    for (func.captures.items) |capture| {
        _ = try c.addBin(.store_capture, func_ref, capture.parent_ref, node);
    }
    return Value{ .ref = func_ref };
}

fn genCall(c: *Compiler, node: Node.Index) Error!Value {
    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);

    const callee = items[0];
    const args = items[1..];

    const callee_val = try c.genNode(callee, .value);
    if (!callee_val.isRt()) {
        return c.reportErr("attempt to call non function value", callee);
    }
    const callee_ref = callee_val.getRt();

    const last_node = c.getLastNode(callee);
    const this = switch (c.tree.nodes.items(.id)[last_node]) {
        .member_access_expr,
        .array_access_expr,
        => c.instructions.items(.data)[Bytecode.refToIndex(callee_ref)].bin.lhs,
        else => null,
    };

    if (args.len > Bytecode.max_params) {
        return c.reportErr("too many arguments", node);
    }

    const list_buf_top = c.list_buf.items.len;
    defer c.list_buf.items.len = list_buf_top;

    try c.list_buf.append(c.gpa, callee_ref);
    if (this) |some| try c.list_buf.append(c.gpa, some);

    for (args) |arg| {
        const arg_val = try c.genNode(arg, .value);
        const arg_ref = if (arg_val == .mut)
            try c.addUn(.copy_un, arg_val.mut, arg)
        else
            try c.makeRuntime(arg_val, arg);

        try c.list_buf.append(c.gpa, arg_ref);
    }

    const arg_refs = c.list_buf.items[list_buf_top..];
    if (this) |some| {
        const res_ref = switch (arg_refs.len) {
            0, 1 => unreachable, // callee and this is always added
            2 => try c.addBin(.this_call_zero, callee_ref, some, node),
            else => try c.addExtra(.this_call, arg_refs, node),
        };
        return Value{ .ref = res_ref };
    }

    const res_ref = switch (arg_refs.len) {
        0 => unreachable, // callee is always added
        1 => try c.addUn(.call_zero, arg_refs[0], node),
        2 => try c.addBin(.call_one, arg_refs[0], arg_refs[1], node),
        else => try c.addExtra(.call, arg_refs, node),
    };
    return Value{ .ref = res_ref };
}

fn genMemberAccess(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const tokens = c.tree.nodes.items(.token);
    const operand = data[node].un;

    var operand_val = try c.genNode(operand, .value);
    if (operand_val != .str and !operand_val.isRt()) {
        return c.reportErr("invalid operand to member access", operand);
    }
    const operand_ref = try c.makeRuntime(operand_val, operand);

    var name_val = Value{ .str = c.tree.tokenSlice(tokens[node]) };
    var name_ref = try c.makeRuntime(name_val, node);

    const res_ref = try c.addBin(.get, operand_ref, name_ref, node);
    return Value{ .ref = res_ref };
}

fn genArrayAccess(c: *Compiler, node: Node.Index) Error!Value {
    const data = c.tree.nodes.items(.data);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;

    var lhs_val = try c.genNode(lhs, .value);
    if (lhs_val != .str and !lhs_val.isRt()) {
        return c.reportErr("invalid operand to subscript", lhs);
    }
    const lhs_ref = try c.makeRuntime(lhs_val, lhs);

    var rhs_val = try c.genNode(rhs, .value);
    var rhs_ref = try c.makeRuntime(rhs_val, rhs);

    const res_ref = try c.addBin(.get, lhs_ref, rhs_ref, node);
    return Value{ .ref = res_ref };
}

fn genFormatString(c: *Compiler, node: Node.Index) Error!Value {
    // transform f"foo {x=:X}bar" into "foo x={X}bar".format((255,))
    var buf = std.ArrayList(u8).init(c.gpa);
    defer buf.deinit();

    const data = c.tree.nodes.items(.data);
    const strings = data[node].format.str(c.tree.extra);
    const args = data[node].format.exprs(c.tree.extra);
    const token_ids = c.tree.tokens.items(.id);
    const starts = c.tree.tokens.items(.start);
    const ends = c.tree.tokens.items(.end);

    for (strings) |str, i| {
        if (i != 0 and token_ids[c.tree.prevToken(str)] == .equal) {
            assert(buf.pop() == '{');
            const first_token = c.tree.firstToken(args[i - 1]);
            const last_token = c.tree.lastToken(args[i - 1]);

            const slice = c.tree.source[starts[first_token]..ends[last_token]];
            try buf.appendSlice(slice);
            try buf.appendSlice("={");
        }
        var slice = c.tree.tokenSlice(str);
        if (token_ids[str] == .format_start) {
            slice = slice[2..]; // strip f"
        } else if (slice[0] == ':') {
            slice = slice[1..]; // strip : from :X}
        }
        if (token_ids[str] == .format_end) {
            slice = slice[0 .. slice.len - 1]; // strip final "
        }

        try buf.ensureUnusedCapacity(slice.len);
        const unused_slice = buf.items.ptr[buf.items.len..buf.capacity];
        buf.items.len += try c.parseStrExtra(str, slice, unused_slice);
    }
    const string_val = Value{ .str = try c.arena.dupe(u8, buf.items) };
    const string_ref = try c.makeRuntime(string_val, node);

    var format_val = Value{ .str = "format" };
    var format_ref = try c.makeRuntime(format_val, node);

    const format_fn_ref = try c.addBin(.get, string_ref, format_ref, node);

    const list_buf_top = c.list_buf.items.len;
    defer c.list_buf.items.len = list_buf_top;

    for (args) |arg| {
        const arg_val = try c.genNode(arg, .value);
        const arg_ref = if (arg_val == .mut)
            try c.addUn(.copy_un, arg_val.mut, arg)
        else
            try c.makeRuntime(arg_val, arg);

        try c.list_buf.append(c.gpa, arg_ref);
    }

    const arg_refs = c.list_buf.items[list_buf_top..];
    const args_tuple_ref = try c.addExtra(.build_tuple, arg_refs, node);

    const res_ref = try c.addExtra(.this_call, &.{
        format_fn_ref,
        string_ref,
        args_tuple_ref,
    }, node);
    return Value{ .ref = res_ref };
}

const Lval = union(enum) {
    let: *const Value,
    assign: *const Value,
    aug_assign: *Ref,
};

fn genLval(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const ids = c.tree.nodes.items(.id);
    switch (ids[node]) {
        .paren_expr => {
            const data = c.tree.nodes.items(.data);
            try c.genLval(data[node].un, lval);
        },
        .ident_expr => try c.genLvalIdent(node, lval, false),
        .mut_ident_expr => try c.genLvalIdent(node, lval, true),
        .discard_expr => {
            // no op
        },
        .enum_expr => try c.genLvalEnum(node, lval),
        .error_expr => try c.genLvalError(node, lval),
        .range_expr,
        .range_expr_end,
        .range_expr_step,
        => try c.genLvalRange(node, lval),
        .tuple_expr,
        .tuple_expr_two,
        .list_expr,
        .list_expr_two,
        => return c.genLvalTupleList(node, lval),
        .map_expr,
        .map_expr_two,
        => return c.genLvalMap(node, lval),
        .member_access_expr => return c.genLvalMemberAccess(node, lval),
        .array_access_expr => return c.genLvalArrayAccess(node, lval),
        else => switch (lval) {
            .let => return c.reportErr("invalid left-hand side to declaration", node),
            .assign, .aug_assign => return c.reportErr("invalid left-hand side to assignment", node),
        },
    }
}

fn genLvalIdent(c: *Compiler, node: Node.Index, lval: Lval, mutable: bool) Error!void {
    const tokens = c.tree.nodes.items(.token);
    switch (lval) {
        .let => |val| {
            try c.checkRedeclaration(tokens[node]);

            var ref = try c.makeRuntime(val.*, node);
            if (val.* == .mut or (mutable and val.isRt())) {
                // copy on assign
                ref = try c.addUn(.copy_un, ref, node);
            }
            const sym = Symbol{
                .name = c.tree.tokenSlice(tokens[node]),
                .ref = ref,
                .mut = mutable,
                .val = val.*,
            };
            try c.scopes.append(c.gpa, .{ .symbol = sym });
            if (c.cur_fn == null) {
                try c.globals.append(c.gpa, sym);
            }
        },
        .assign => |val| {
            const sym = try c.findSymbol(tokens[node]);
            if (!sym.mut) {
                return c.reportErr("assignment to constant", node);
            }
            if (val.* == .mut) {
                _ = try c.addBin(.copy, sym.ref, val.mut, node);
            } else {
                const val_ref = try c.makeRuntime(val.*, node);
                _ = try c.addBin(.move, sym.ref, val_ref, node);
            }
        },
        .aug_assign => |val| {
            const sym = try c.findSymbol(tokens[node]);
            if (!sym.mut) {
                return c.reportErr("assignment to constant", node);
            }
            val.* = sym.ref;
        },
    }
}

fn genLvalEnum(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const val = switch (lval) {
        .let, .assign => |val| val,
        .aug_assign => return c.reportErr("invalid left hand side to augmented assignment", node),
    };
    if (!val.isRt()) {
        return c.reportErr("expected a tagged value", node);
    }
    const data = c.tree.nodes.items(.data);
    if (data[node].un == 0) {
        return c.reportErr("expected a destructuring", node);
    }

    const tokens = c.tree.nodes.items(.token);
    const slice = c.tree.tokenSlice(tokens[node]);
    const str_offset = try c.putString(slice);

    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.append(c.gpa, val.getRt());
    try c.extra.append(c.gpa, @intToEnum(Ref, str_offset));
    const unwrapped_ref = try c.addInst(.unwrap_tagged, .{
        .extra = .{
            .extra = extra,
            .len = @intCast(u32, slice.len),
        },
    }, tokens[node]);

    const rhs_val = Value{ .ref = unwrapped_ref };
    try c.genLval(data[node].un, switch (lval) {
        .let => .{ .let = &rhs_val },
        .assign => .{ .assign = &rhs_val },
        else => unreachable,
    });
}

fn genLvalError(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const val = switch (lval) {
        .let, .assign => |val| val,
        .aug_assign => return c.reportErr("invalid left hand side to augmented assignment", node),
    };
    if (!val.isRt()) {
        return c.reportErr("expected an error", node);
    }
    const data = c.tree.nodes.items(.data);
    if (data[node].un == 0) {
        return c.reportErr("expected a destructuring", node);
    }
    const unwrapped = try c.addUn(.unwrap_error, val.getRt(), node);

    const rhs_val = Value{ .ref = unwrapped };
    try c.genLval(data[node].un, switch (lval) {
        .let => .{ .let = &rhs_val },
        .assign => .{ .assign = &rhs_val },
        else => unreachable,
    });
}

fn genLvalRange(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const val = switch (lval) {
        .let, .assign => |val| val,
        .aug_assign => return c.reportErr("invalid left hand side to augmented assignment", node),
    };
    if (!val.isRt()) {
        return c.reportErr("expected a range", node);
    }
    const range = Tree.Range.get(c.tree.*, node);

    try c.genLValRangePart(range.start, val.getRt(), lval, "start");
    if (range.end) |some| {
        try c.genLValRangePart(some, val.getRt(), lval, "end");
    }
    if (range.step) |some| {
        try c.genLValRangePart(some, val.getRt(), lval, "step");
    }
}

fn genLValRangePart(c: *Compiler, node: Node.Index, range_ref: Ref, lval: Lval, part: []const u8) Error!void {
    var name_val = Value{ .str = part };
    var name_ref = try c.makeRuntime(name_val, node);

    const res_ref = try c.addBin(.get, range_ref, name_ref, node);
    const res_val = Value{ .ref = res_ref };
    try c.genLval(node, switch (lval) {
        .let => .{ .let = &res_val },
        .assign => .{ .assign = &res_val },
        else => unreachable,
    });
}

fn genLvalTupleList(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const res = switch (lval) {
        .let, .assign => |val| val,
        .aug_assign => return c.reportErr("invalid left hand side to augmented assignment", node),
    };
    if (!res.isRt()) {
        return c.reportErr("expected a tuple/list", node);
    }
    const container_ref = res.getRt();

    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);
    const ids = c.tree.nodes.items(.id);

    _ = try c.addBin(.assert_len, container_ref, @intToEnum(Ref, items.len), node);

    for (items) |item, i| {
        const last_node = c.getLastNode(item);
        if (ids[last_node] == .discard_expr) {
            continue;
        }

        const index_ref = try c.makeRuntime(Value{ .int = @intCast(u32, i) }, item);
        const res_ref = try c.addBin(.get, container_ref, index_ref, item);
        const res_val = Value{ .ref = res_ref };
        try c.genLval(item, switch (lval) {
            .let => .{ .let = &res_val },
            .assign => .{ .assign = &res_val },
            else => unreachable,
        });
    }
}

fn genLvalMap(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    const res = switch (lval) {
        .let, .assign => |val| val,
        .aug_assign => return c.reportErr("invalid left hand side to augmented assignment", node),
    };
    if (!res.isRt()) {
        return c.reportErr("expected a tuple/list", node);
    }
    const container_ref = res.getRt();

    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);
    const tok_ids = c.tree.tokens.items(.id);
    const data = c.tree.nodes.items(.data);

    for (items) |item| {
        var key: Ref = undefined;
        if (data[item].bin.lhs != 0) {
            const last_node = c.getLastNode(data[item].bin.lhs);
            const maybe_ident = c.tree.firstToken(last_node);
            if (tok_ids[maybe_ident] == .identifier) {
                // `ident = value` is equal to `"ident" = value`
                const str = c.tree.tokenSlice(maybe_ident);
                key = try c.addInst(.str, .{ .str = .{
                    .len = @intCast(u32, str.len),
                    .offset = try c.putString(str),
                } }, maybe_ident);
            } else {
                var key_val = try c.genNode(data[item].bin.lhs, .value);
                key = try c.makeRuntime(key_val, data[item].bin.lhs);
            }
        } else {
            const last_node = c.getLastNode(data[item].bin.rhs);
            const maybe_ident = c.tree.firstToken(last_node);
            if (tok_ids[maybe_ident] != .identifier) {
                return c.reportErr("expected a key", item);
            }
            // `ident` is equal to `"ident" = ident´
            const str = c.tree.tokenSlice(maybe_ident);
            key = try c.addInst(.str, .{ .str = .{
                .len = @intCast(u32, str.len),
                .offset = try c.putString(str),
            } }, maybe_ident);
        }

        const res_ref = try c.addBin(.get, container_ref, key, item);
        const res_val = Value{ .ref = res_ref };
        try c.genLval(data[item].bin.rhs, switch (lval) {
            .let => .{ .let = &res_val },
            .assign => .{ .assign = &res_val },
            else => unreachable,
        });
    }
}

fn genLvalMemberAccess(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    if (lval == .let) {
        return c.reportErr("invalid left hand side to augmented assignment", node);
    }
    const data = c.tree.nodes.items(.data);
    const tokens = c.tree.nodes.items(.token);
    const operand = data[node].un;

    var operand_val = try c.genNode(operand, .value);
    if (operand_val != .str and !operand_val.isRt()) {
        return c.reportErr("invalid operand to member access", operand);
    }
    const operand_ref = try c.makeRuntime(operand_val, operand);

    var name_val = Value{ .str = c.tree.tokenSlice(tokens[node]) };
    var name_ref = try c.makeRuntime(name_val, operand);

    switch (lval) {
        .aug_assign => {
            return c.reportErr("TODO augmented assign member access", node);
        },
        .assign => |val| {
            const val_ref = if (val.* == .mut)
                try c.addUn(.copy_un, val.mut, node)
            else
                try c.makeRuntime(val.*, node);

            const extra = @intCast(u32, c.extra.items.len);
            try c.extra.append(c.gpa, name_ref);
            try c.extra.append(c.gpa, val_ref);
            _ = try c.addInst(.set, .{
                .range = .{
                    .start = operand_ref,
                    .extra = extra,
                },
            }, tokens[node]);
        },
        else => unreachable,
    }
}

fn genLvalArrayAccess(c: *Compiler, node: Node.Index, lval: Lval) Error!void {
    if (lval == .let) {
        return c.reportErr("cannot declare a subscript", node);
    }
    const data = c.tree.nodes.items(.data);
    const tokens = c.tree.nodes.items(.token);
    const lhs = data[node].bin.lhs;
    const rhs = data[node].bin.rhs;

    var lhs_val = try c.genNode(lhs, .value);
    if (lhs_val != .str and !lhs_val.isRt()) {
        return c.reportErr("invalid operand to subscript", lhs);
    }
    const lhs_ref = try c.makeRuntime(lhs_val, lhs);

    var rhs_val = try c.genNode(rhs, .value);
    var rhs_ref = try c.makeRuntime(rhs_val, rhs);

    switch (lval) {
        .aug_assign => {
            return c.reportErr("TODO augmented assign member access", node);
        },
        .assign => |val| {
            const val_ref = if (val.* == .mut)
                try c.addUn(.copy_un, val.mut, node)
            else
                try c.makeRuntime(val.*, node);

            const extra = @intCast(u32, c.extra.items.len);
            try c.extra.append(c.gpa, rhs_ref);
            try c.extra.append(c.gpa, val_ref);
            _ = try c.addInst(.set, .{
                .range = .{
                    .start = lhs_ref,
                    .extra = extra,
                },
            }, tokens[node]);
        },
        else => unreachable,
    }
}

fn genTryUnwrap(c: *Compiler, node: Node.Index, val: *const Value) Error!void {
    const ids = c.tree.nodes.items(.id);
    switch (ids[node]) {
        .paren_expr => {
            const data = c.tree.nodes.items(.data);
            try c.genTryUnwrap(data[node].un, val);
        },
        .ident_expr => try c.genLvalIdent(node, .{ .let = val }, false),
        .mut_ident_expr => try c.genLvalIdent(node, .{ .let = val }, true),
        .discard_expr => {
            // no op
        },
        .enum_expr => try c.genTryUnwrapEnum(node, val),
        .error_expr => try c.genTryUnwrapError(node, val),
        .range_expr,
        .range_expr_end,
        .range_expr_step,
        => try c.genTryUnwrapRange(node, val),
        .tuple_expr,
        .tuple_expr_two,
        .list_expr,
        .list_expr_two,
        => return c.genTryUnwrapTupleList(node, val),
        .map_expr,
        .map_expr_two,
        => return c.genTryUnwrapMap(node, val),
        else => return c.reportErr("invalid left-hand side to declaration", node),
    }
}

fn genTryUnwrapEnum(c: *Compiler, node: Node.Index, val: *const Value) Error!void {
    if (!val.isRt()) {
        return c.reportErr("expected a tagged value", node);
    }
    const data = c.tree.nodes.items(.data);
    if (data[node].un == 0) {
        return c.reportErr("expected a destructuring", node);
    }

    const tokens = c.tree.nodes.items(.token);
    const slice = c.tree.tokenSlice(tokens[node]);
    const str_offset = try c.putString(slice);

    const extra = @intCast(u32, c.extra.items.len);
    try c.extra.append(c.gpa, val.getRt());
    try c.extra.append(c.gpa, @intToEnum(Ref, str_offset));
    const unwrapped_ref = try c.addInst(.unwrap_tagged_or_null, .{
        .extra = .{
            .extra = extra,
            .len = @intCast(u32, slice.len),
        },
    }, tokens[node]);
    try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_null, unwrapped_ref, node));

    try c.genTryUnwrap(data[node].un, &.{ .ref = unwrapped_ref });
}

fn genTryUnwrapError(c: *Compiler, node: Node.Index, val: *const Value) Error!void {
    if (!val.isRt()) {
        return c.reportErr("expected an error", node);
    }
    const data = c.tree.nodes.items(.data);
    if (data[node].un == 0) {
        return c.reportErr("expected a destructuring", node);
    }
    const unwrapped_ref = try c.addJump(.unwrap_error_or_jump, val.getRt(), node);
    try c.unwrap_jump_buf.append(c.gpa, unwrapped_ref);

    try c.genTryUnwrap(data[node].un, &.{ .ref = unwrapped_ref });
}

fn genTryUnwrapRange(c: *Compiler, node: Node.Index, val: *const Value) Error!void {
    const range = Tree.Range.get(c.tree.*, node);
    if (!val.isRt()) {
        return c.reportErr("expected a range", node);
    }
    const range_ref = val.getRt();
    const tokens = c.tree.nodes.items(.token);

    const is_range_ref = try c.addInst(.is, .{
        .bin_ty = .{ .operand = range_ref, .ty = .range },
    }, tokens[node]);
    try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_false, is_range_ref, node));

    try c.genUnwrapRangePart(range.start, range_ref, "start");
    if (range.end) |some| {
        try c.genUnwrapRangePart(some, range_ref, "end");
    }
    if (range.step) |some| {
        try c.genUnwrapRangePart(some, range_ref, "step");
    }
}

fn genUnwrapRangePart(c: *Compiler, node: Node.Index, range_ref: Ref, part: []const u8) Error!void {
    var name_val = Value{ .str = part };
    var name_ref = try c.makeRuntime(name_val, node);

    const res_ref = try c.addBin(.get, range_ref, name_ref, node);
    try c.genTryUnwrap(node, &.{ .ref = res_ref });
}

fn genTryUnwrapTupleList(c: *Compiler, node: Node.Index, val: *const Value) Error!void {
    if (!val.isRt()) {
        return c.reportErr("expected a tuple/list", node);
    }
    const container_ref = val.getRt();

    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);
    const ids = c.tree.nodes.items(.id);

    const len_ref = try c.addBin(.check_len, val.getRt(), @intToEnum(Ref, items.len), node);
    try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_false, len_ref, node));

    for (items) |item, i| {
        const last_node = c.getLastNode(item);
        if (ids[last_node] == .discard_expr) {
            continue;
        }

        const index_ref = try c.makeRuntime(Value{ .int = @intCast(u32, i) }, item);
        const res_ref = try c.addBin(.get, container_ref, index_ref, item);

        try c.genTryUnwrap(item, &.{ .ref = res_ref });
    }
}

fn genTryUnwrapMap(c: *Compiler, node: Node.Index, val: *const Value) Error!void {
    if (!val.isRt()) {
        return c.reportErr("expected a map", node);
    }
    const container_ref = val.getRt();

    var buf: [2]Node.Index = undefined;
    const items = c.tree.nodeItems(node, &buf);
    const tok_ids = c.tree.tokens.items(.id);
    const data = c.tree.nodes.items(.data);

    for (items) |item| {
        var key: Ref = undefined;
        if (data[item].bin.lhs != 0) {
            const last_node = c.getLastNode(data[item].bin.lhs);
            const maybe_ident = c.tree.firstToken(last_node);
            if (tok_ids[maybe_ident] == .identifier) {
                // `ident = value` is equal to `"ident" = value`
                const str = c.tree.tokenSlice(maybe_ident);
                key = try c.addInst(.str, .{ .str = .{
                    .len = @intCast(u32, str.len),
                    .offset = try c.putString(str),
                } }, maybe_ident);
            } else {
                var key_val = try c.genNode(data[item].bin.lhs, .value);
                key = try c.makeRuntime(key_val, data[item].bin.lhs);
            }
        } else {
            const last_node = c.getLastNode(data[item].bin.rhs);
            const maybe_ident = c.tree.firstToken(last_node);
            if (tok_ids[maybe_ident] != .identifier) {
                return c.reportErr("expected a key", item);
            }
            // `ident` is equal to `"ident" = ident`
            const str = c.tree.tokenSlice(maybe_ident);
            key = try c.addInst(.str, .{ .str = .{
                .len = @intCast(u32, str.len),
                .offset = try c.putString(str),
            } }, maybe_ident);
        }

        const res_ref = try c.addBin(.get_or_null, container_ref, key, item);
        try c.unwrap_jump_buf.append(c.gpa, try c.addJump(.jump_if_null, res_ref, item));

        try c.genTryUnwrap(data[item].bin.rhs, &.{ .ref = res_ref });
    }
}

fn parseStr(c: *Compiler, tok: TokenIndex) ![]u8 {
    var slice = c.tree.tokenSlice(tok);
    const start = @as(u32, 1) + @boolToInt(slice[0] == 'f');
    slice = slice[start .. slice.len - 1];
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
    try c.errors.add(
        .{ .data = msg },
        c.tree.source,
        c.tree.path,
        starts[c.tree.firstToken(node)],
        .err,
    );
    return error.CompileError;
}
