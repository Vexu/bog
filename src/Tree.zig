const std = @import("std");
const Allocator = std.mem.Allocator;

const bog = @import("bog.zig");
const Token = bog.Token;

pub const Tree = @This();

tokens: Token.List,
nodes: Node.List,
extra: []Node.Index,
root_nodes: []Node.Index,

/// not owned by the tree
source: []const u8,
path: []const u8,

pub fn deinit(tree: *Tree, gpa: Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra);
    gpa.free(tree.root_nodes);
    tree.* = undefined;
}

pub const render = @import("render.zig").render;

pub fn tokenSlice(tree: Tree, tok: Token.Index) []const u8 {
    const starts = tree.tokens.items(.start);
    const ends = tree.tokens.items(.end);
    return tree.source[starts[tok]..ends[tok]];
}

pub fn firstToken(tree: Tree, node: Node.Index) Token.Index {
    const toks = tree.nodes.items(.token);
    const ids = tree.nodes.items(.id);
    const data = tree.nodes.items(.data);
    var cur = node;
    while (true) switch (ids[cur]) {
        .bool_not_expr,
        .bit_not_expr,
        .negate_expr,
        .decl,
        .ident_expr,
        .discard_expr,
        .return_expr,
        .break_expr,
        .continue_expr,
        .throw_expr,
        .fn_expr,
        .fn_expr_one,
        .paren_expr,
        .tuple_expr,
        .tuple_expr_two,
        .list_expr,
        .list_expr_two,
        .map_expr,
        .map_expr_two,
        .error_expr,
        .string_expr,
        .int_expr,
        .num_expr,
        .true_expr,
        .false_expr,
        .this_expr,
        .null_expr,
        .if_expr,
        .if_else_expr,
        .if_let_expr,
        .if_let_else_expr,
        .while_expr,
        .while_let_expr,
        .for_expr,
        .for_let_expr,
        .match_expr,
        .match_expr_one,
        .try_expr,
        .try_expr_one,
        .catch_expr,
        .catch_let_expr,
        .block_stmt,
        .block_stmt_two,
        => return toks[cur],
        .mut_ident_expr,
        .enum_expr,
        .match_case_catch_all,
        => return tree.prevToken(toks[cur]),
        .import_expr => return tree.prevToken(tree.prevToken(toks[cur])),
        .call_expr => cur = tree.extra[data[cur].range.start],
        .member_access_expr => cur = data[cur].un,
        .assign,
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
        .array_access_expr,
        .call_expr_one,
        .match_case_one,
        .range_expr_end,
        .range_expr_step,
        .bool_or_expr,
        .bool_and_expr,
        .less_than_expr,
        .less_than_equal_expr,
        .greater_than_expr,
        .greater_than_equal_expr,
        .equal_expr,
        .not_equal_expr,
        .in_expr,
        .is_expr,
        .as_expr,
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
        .rem_expr,
        .pow_expr,
        => cur = data[cur].bin.lhs,
        .map_item_expr => if (data[cur].bin.lhs != 0) {
            cur = data[cur].bin.lhs;
        } else {
            cur = data[cur].bin.rhs;
        },
        .match_case_let => {
            cur = data[cur].bin.lhs;
            const first_token = tree.firstToken(cur);
            return tree.prevToken(first_token);
        },
        .match_case => {
            cur = tree.extra[data[cur].range.start];
            const first_token = tree.firstToken(cur);
            return tree.prevToken(first_token);
        },
        .format_expr => {
            const strs = data[cur].format.str(tree.extra);
            return strs[0];
        },
        .range_expr => cur = data[cur].cond.cond,
    };
}

pub fn lastToken(tree: Tree, node: Node.Index) Token.Index {
    const tokens = tree.nodes.items(.token);
    const ids = tree.nodes.items(.id);
    const data = tree.nodes.items(.data);
    const tok_ids = tree.tokens.items(.id);
    var cur = node;
    while (true) switch (ids[cur]) {
        .ident_expr,
        .mut_ident_expr,
        .discard_expr,
        .string_expr,
        .int_expr,
        .num_expr,
        .true_expr,
        .false_expr,
        .this_expr,
        .null_expr,
        .break_expr,
        .continue_expr,
        .member_access_expr,
        => return tokens[cur],
        .import_expr => return tree.nextToken(tokens[cur]),
        .match_case,
        .try_expr,
        .fn_expr,
        .match_expr,
        .block_stmt,
        => cur = tree.extra[data[cur].range.end - 1],
        .fn_expr_one => cur = data[cur].bin.rhs,
        .paren_expr => return tree.nextToken(tree.lastToken(data[cur].un)),
        .tuple_expr,
        .list_expr,
        .map_expr,
        .call_expr,
        => {
            const next = tree.nextToken(tree.lastToken(tree.extra[data[cur].range.end - 1]));
            return if (tok_ids[next] == .comma)
                tree.nextToken(next)
            else
                next;
        },
        .array_access_expr => return tree.nextToken(tree.lastToken(data[cur].bin.rhs)),
        .block_stmt_two => if (data[cur].bin.rhs != 0) {
            cur = data[cur].bin.rhs;
        } else {
            cur = data[cur].bin.lhs;
        },
        .tuple_expr_two,
        .list_expr_two,
        .map_expr_two,
        => if (data[cur].bin.rhs != 0) {
            const next = tree.nextToken(tree.lastToken(data[cur].bin.rhs));
            return if (tok_ids[next] == .comma)
                tree.nextToken(next)
            else
                next;
        } else if (data[cur].bin.lhs != 0) {
            const next = tree.nextToken(tree.lastToken(data[cur].bin.lhs));
            return if (tok_ids[next] == .comma)
                tree.nextToken(next)
            else
                next;
        } else {
            return tree.nextToken(tokens[cur]);
        },
        .call_expr_one => if (data[cur].bin.rhs != 0) {
            const next = tree.nextToken(tree.lastToken(data[cur].bin.rhs));
            return if (tok_ids[next] == .comma)
                tree.nextToken(next)
            else
                next;
        } else {
            return tree.nextToken(tokens[cur]);
        },
        .try_expr_one => if (data[cur].bin.rhs != 0) {
            cur = data[cur].bin.rhs;
        } else {
            cur = data[cur].bin.lhs;
        },
        .format_expr => {
            const strs = data[cur].format.str(tree.extra);
            return strs[strs.len - 1];
        },
        .if_let_else_expr => cur = tree.extra[data[cur].cond.extra + 1],
        .while_let_expr,
        .for_let_expr,
        .if_else_expr,
        .if_let_expr,
        .range_expr,
        => cur = tree.extra[data[cur].cond.extra + 1],
        .error_expr,
        .enum_expr,
        .return_expr,
        => if (data[cur].un != 0) {
            cur = data[cur].un;
        } else {
            return tokens[cur];
        },
        .throw_expr,
        .bool_not_expr,
        .bit_not_expr,
        .negate_expr,
        .match_case_catch_all,
        => cur = data[cur].un,
        .is_expr, .as_expr => return data[cur].ty_bin.rhs,
        .decl,
        .assign,
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
        .rem_expr,
        .pow_expr,
        .match_expr_one,
        .match_case_let,
        .match_case_one,
        .catch_expr,
        .catch_let_expr,
        .if_expr,
        .for_expr,
        .while_expr,
        .map_item_expr,
        => cur = data[cur].bin.rhs,
        .range_expr_step => if (data[cur].bin.rhs != 0) {
            cur = data[cur].bin.rhs;
        } else {
            const last = tree.lastToken(data[cur].bin.lhs);
            const next = tree.nextToken(last);
            return if (tok_ids[next] == .colon)
                next
            else
                last;
        },
        .range_expr_end => if (data[cur].bin.rhs != 0) {
            cur = data[cur].bin.rhs;
        } else {
            cur = data[cur].bin.lhs;
            const last = tree.lastToken(cur);
            const next = tree.nextToken(last);
            return if (tok_ids[next] == .colon)
                next
            else
                last;
        },
    };
}

pub fn prevToken(tree: Tree, tok: Token.Index) Token.Index {
    const ids = tree.tokens.items(.id);
    var i = tok - 1;
    while (true) switch (ids[i]) {
        // zig fmt: off
        .indent_1, .indent_2, .indent_3, .indent_4, .indent_5,
        .indent_6, .indent_7, .indent_8, .indent_9, .indent_10,
        .indent_11, .indent_12, .indent_13, .indent_14, .indent_15,
        .indent_16, .indent_17, .indent_18, .indent_19, .indent_20,
        .indent_21, .indent_22, .indent_23, .indent_24, .indent_25,
        .indent_26, .indent_27, .indent_28, .indent_29, .indent_30,
        .indent_31, .indent_32, .nl => i -= 1,
        else => return i,
        // zig fmt: on
    };
}

pub fn nextToken(tree: Tree, tok: Token.Index) Token.Index {
    const ids = tree.tokens.items(.id);
    var i = tok + 1;
    while (true) switch (ids[i]) {
        // zig fmt: off
        .indent_1, .indent_2, .indent_3, .indent_4, .indent_5,
        .indent_6, .indent_7, .indent_8, .indent_9, .indent_10,
        .indent_11, .indent_12, .indent_13, .indent_14, .indent_15,
        .indent_16, .indent_17, .indent_18, .indent_19, .indent_20,
        .indent_21, .indent_22, .indent_23, .indent_24, .indent_25,
        .indent_26, .indent_27, .indent_28, .indent_29, .indent_30,
        .indent_31, .indent_32, .nl => i += 1,
        else => return i,
        // zig fmt: on
    };
}

pub fn lineDist(tree: Tree, a: Token.Index, b: Token.Index) u32 {
    var count: u32 = 0;
    const ids = tree.tokens.items(.id);
    var i = a;
    while (i < b) : (i += 1) {
        count += @boolToInt(ids[i] == .nl);
    }
    return count;
}

pub const Node = struct {
    id: Id,
    token: Token.Index,
    data: Data,

    pub const Data = union {
        un: Index,
        bin: struct {
            lhs: Index,
            rhs: Index,
        },
        ty_bin: struct {
            lhs: Index,
            rhs: Token.Index,
        },
        range: struct {
            start: u32,
            end: u32,
        },
        cond: struct {
            cond: Index,
            extra: u32,
        },
        format: struct {
            fmt_start: u32,
            args_start: u32,

            pub fn str(f: @This(), extra: []Index) []Token.Index {
                return @bitCast([]Token.Index, extra[f.fmt_start..f.args_start]);
            }

            pub fn exprs(f: @This(), extra: []Index) []Index {
                // there are N -1 expressions for N string parts
                return extra[f.args_start..][0 .. f.args_start - f.fmt_start - 1];
            }
        },
    };

    pub const List = std.MultiArrayList(Node);

    pub const Index = u32;

    pub const Id = enum(u8) {
        /// let lhs = rhs
        decl,

        // statements

        /// NL extra[start..end] NL
        block_stmt,
        /// NL lhs NL rhs NL, rhs may be omitted
        block_stmt_two,

        // assignment expressions

        /// lhs = rhs
        assign,
        /// lhs += rhs
        add_assign,
        /// lhs -= rhs
        sub_assign,
        /// lhs *= rhs
        mul_assign,
        /// lhs **= rhs
        pow_assign,
        /// lhs /= rhs
        div_assign,
        /// lhs //= rhs
        div_floor_assign,
        /// lhs %= rhs
        rem_assign,
        /// lhs <<= rhs
        l_shift_assign,
        /// lhs >>= rhs
        r_shift_assign,
        /// lhs &= rhs
        bit_and_assign,
        /// lhs |= rhs
        bit_or_assign,
        /// lhs ^= rhs
        bit_xor_assign,

        // expressions

        /// token
        ident_expr,
        /// mut token
        mut_ident_expr,
        /// _
        discard_expr,
        /// return un, un may be omitted
        return_expr,
        /// break
        break_expr,
        /// continue
        continue_expr,
        /// throw un
        throw_expr,
        /// fn (extra[start..end-1]) extra[end]
        fn_expr,
        /// fn (lhs) rhs, lhs may be omitted
        fn_expr_one,
        /// ( data.un )
        paren_expr,
        /// ( extra[start..end])
        tuple_expr,
        /// (lhr, rhs) rhs may be omitted
        tuple_expr_two,
        /// [extra[start..end]]
        list_expr,
        /// [lhs, rhs] both may be omitted
        list_expr_two,
        /// { extra[start..end] }
        map_expr,
        /// { lhs, rhs } both may be omitted
        map_expr_two,
        /// lhs = rhs, lhs may be omitted
        map_item_expr,
        /// token
        string_expr,
        /// token
        int_expr,
        /// token
        num_expr,
        /// true
        true_expr,
        /// false
        false_expr,
        /// this
        this_expr,
        /// null
        null_expr,
        /// error un, un may be omitted
        error_expr,
        /// @token un, un may be omitted
        enum_expr,
        /// import(token)
        import_expr,
        /// lhs[rhs]
        array_access_expr,
        /// lhs.token
        member_access_expr,
        /// extra[start](extra[start+1]..extra[end])
        call_expr,
        /// lhs(rhs) rhs may be omitted
        call_expr_one,
        /// if lhs rhs
        if_expr,
        /// if cond extra[0] else extra[1]
        if_else_expr,
        /// if let extra[0] = cond extra[1]
        if_let_expr,
        /// if let extra[0] = cond extra[1] else extra[2]
        if_let_else_expr,
        /// while lhs rhs
        while_expr,
        /// while let extra[0] cond extra[1]
        while_let_expr,
        /// for lhs rhs
        for_expr,
        /// for let extra[0] cond extra[1]
        for_let_expr,
        /// match extra[start] extra[start+1..end]
        match_expr,
        /// match lhs rhs
        match_expr_one,
        /// let lhs => rhs
        match_case_let,
        /// _ => un
        match_case_catch_all,
        /// extra[start..end -1] => extra[end]
        match_case,
        /// lhs => rhs
        match_case_one,
        /// try extra[start] extra[start+1..end]
        try_expr,
        /// try lhs rhs, rhs may be omitted
        try_expr_one,
        /// catch lhs rhs, lhs may be omitted
        catch_expr,
        /// catch let lhs rhs
        catch_let_expr,
        /// f" format "
        format_expr,
        /// cond : extra[0] : extra[1]
        range_expr,
        /// lhs : rhs
        range_expr_end,
        /// lhs : : rhs
        range_expr_step,

        // unary expressions

        /// not un
        bool_not_expr,
        /// ~un
        bit_not_expr,
        /// -un
        negate_expr,

        // binary expressions

        /// lhs or rhs
        bool_or_expr,
        /// lhs and rhs
        bool_and_expr,
        /// lhs < rhs
        less_than_expr,
        /// lhs <= rhs
        less_than_equal_expr,
        /// lhs > rhs
        greater_than_expr,
        /// lhs >= rhs
        greater_than_equal_expr,
        /// lhs == rhs
        equal_expr,
        /// lhs != rhs
        not_equal_expr,
        /// lhs in rhs
        in_expr,
        /// lhs is type name
        is_expr,
        /// lhs as type name
        as_expr,
        /// lhs & rhs
        bit_and_expr,
        /// lhs | rhs
        bit_or_expr,
        /// lhs ^ rhs
        bit_xor_expr,
        /// lhs << rhs
        l_shift_expr,
        /// lhs >> rhs
        r_shift_expr,
        /// lhs + rhs
        add_expr,
        /// lhs - rhs
        sub_expr,
        /// lhs * rhs
        mul_expr,
        /// lhs / rhs
        div_expr,
        /// lhs // rhs
        div_floor_expr,
        /// lhs % rhs
        rem_expr,
        /// lhs ** rhs
        pow_expr,
    };
};

pub const Range = struct {
    start: Node.Index,
    colon_1: Token.Index,
    end: ?Node.Index,
    colon_2: ?Token.Index,
    step: ?Node.Index,

    pub fn get(tree: Tree, node: Node.Index) Range {
        const tokens = tree.nodes.items(.token);
        var range = Range{
            .start = undefined,
            .colon_1 = tokens[node],
            .end = null,
            .colon_2 = null,
            .step = null,
        };
        const data = tree.nodes.items(.data);
        switch (tree.nodes.items(.id)[node]) {
            .range_expr => {
                range.start = data[node].cond.cond;
                range.end = tree.extra[data[node].cond.extra];
                range.step = tree.extra[data[node].cond.extra + 1];
                range.colon_2 = tree.prevToken(tree.firstToken(range.step.?));
            },
            .range_expr_end => {
                range.start = data[node].bin.lhs;
                range.end = data[node].bin.rhs;
            },
            .range_expr_step => {
                range.start = data[node].bin.lhs;
                if (data[node].bin.rhs != 0) range.step = data[node].bin.rhs;
                if (range.step) |some| {
                    range.colon_2 = tree.prevToken(tree.firstToken(some));
                }
            },
            else => unreachable,
        }
        return range;
    }
};

pub const For = struct {
    for_tok: Token.Index,
    let_tok: Token.Index,
    capture: ?Node.Index,
    in_tok: Token.Index,
    cond: Node.Index,
    body: Node.Index,

    pub fn get(tree: Tree, node: Node.Index) For {
        const tokens = tree.nodes.items(.token);
        var for_expr: For = undefined;
        for_expr.for_tok = tokens[node];

        const data = tree.nodes.items(.data);
        if (tree.nodes.items(.id)[node] == .for_let_expr) {
            for_expr.let_tok = tree.nextToken(tokens[node]);
            for_expr.capture = tree.extra[data[node].cond.extra];
            for_expr.in_tok = tree.nextToken(tree.lastToken(for_expr.capture.?));
            for_expr.cond = data[node].cond.cond;
            for_expr.body = tree.extra[data[node].cond.extra + 1];
        } else {
            for_expr.cond = data[node].bin.lhs;
            for_expr.capture = null;
            for_expr.body = data[node].bin.rhs;
        }
        return for_expr;
    }
};

pub const While = struct {
    while_tok: Token.Index,
    let_tok: Token.Index,
    capture: ?Node.Index,
    eq_tok: Token.Index,
    cond: Node.Index,
    body: Node.Index,

    pub fn get(tree: Tree, node: Node.Index) While {
        const tokens = tree.nodes.items(.token);
        var while_expr: While = undefined;
        while_expr.while_tok = tokens[node];

        const data = tree.nodes.items(.data);
        if (tree.nodes.items(.id)[node] == .while_let_expr) {
            while_expr.let_tok = tree.nextToken(tokens[node]);
            while_expr.capture = tree.extra[data[node].cond.extra];
            while_expr.eq_tok = tree.nextToken(tree.lastToken(while_expr.capture.?));
            while_expr.cond = data[node].cond.cond;
            while_expr.body = tree.extra[data[node].cond.extra + 1];
        } else {
            while_expr.cond = data[node].bin.lhs;
            while_expr.capture = null;
            while_expr.body = data[node].bin.rhs;
        }
        return while_expr;
    }
};

pub const If = struct {
    if_tok: Token.Index,
    let_tok: Token.Index,
    capture: ?Node.Index,
    eq_tok: Token.Index,
    cond: Node.Index,
    then_body: Node.Index,
    else_tok: Token.Index,
    else_body: ?Node.Index,

    pub fn get(tree: Tree, node: Node.Index) If {
        const tokens = tree.nodes.items(.token);
        var if_expr: If = undefined;
        if_expr.if_tok = tokens[node];

        const data = tree.nodes.items(.data);
        switch (tree.nodes.items(.id)[node]) {
            .if_expr => {
                if_expr.capture = null;
                if_expr.cond = data[node].bin.lhs;
                if_expr.then_body = data[node].bin.rhs;
                if_expr.else_body = null;
            },
            .if_else_expr => {
                if_expr.capture = null;
                if_expr.cond = data[node].cond.cond;
                if_expr.then_body = tree.extra[data[node].cond.extra];
                if_expr.else_tok = tree.nextToken(tree.lastToken(if_expr.then_body));
                if_expr.else_body = tree.extra[data[node].cond.extra + 1];
            },
            .if_let_expr => {
                if_expr.let_tok = tree.nextToken(tokens[node]);
                if_expr.capture = tree.extra[data[node].cond.extra];
                if_expr.eq_tok = tree.nextToken(tree.lastToken(if_expr.capture.?));
                if_expr.cond = data[node].cond.cond;
                if_expr.then_body = tree.extra[data[node].cond.extra + 1];
                if_expr.else_body = null;
            },
            .if_let_else_expr => {
                if_expr.let_tok = tree.nextToken(tokens[node]);
                if_expr.capture = tree.extra[data[node].cond.extra];
                if_expr.eq_tok = tree.nextToken(tree.lastToken(if_expr.capture.?));
                if_expr.cond = data[node].cond.cond;
                if_expr.then_body = tree.extra[data[node].cond.extra + 1];
                if_expr.else_tok = tree.nextToken(tree.lastToken(if_expr.then_body));
                if_expr.else_body = tree.extra[data[node].cond.extra + 2];
            },
            else => unreachable,
        }
        return if_expr;
    }
};

pub fn nodeItems(tree: Tree, node: Node.Index, buf: *[2]Node.Index) []const Node.Index {
    const data = tree.nodes.items(.data);
    switch (tree.nodes.items(.id)[node]) {
        .tuple_expr_two,
        .list_expr_two,
        .map_expr_two,
        .block_stmt_two,
        .try_expr_one,
        .fn_expr_one,
        .call_expr_one,
        .match_expr_one,
        .match_case_one,
        => {
            buf[0] = data[node].bin.lhs;
            buf[1] = data[node].bin.rhs;
            if (buf[1] != 0) {
                return buf[0..2];
            } else if (buf[0] != 0) {
                return buf[0..1];
            } else {
                return buf[0..0];
            }
        },
        .match_case,
        .tuple_expr,
        .list_expr,
        .map_expr,
        .block_stmt,
        .try_expr,
        .fn_expr,
        .call_expr,
        .match_expr,
        => return tree.extra[data[node].range.start..data[node].range.end],
        else => unreachable,
    }
}
