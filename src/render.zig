const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const bog = @import("bog.zig");
const Tree = bog.Tree;
const Node = bog.Node;
const Token = bog.Token;
const changeDetectionWriter = std.io.changeDetectionStream;

const indent_delta = 4;

pub fn render(tree: Tree, writer: anytype) @TypeOf(writer).Error!bool {
    var change_writer = changeDetectionWriter(tree.source, writer);
    var aiw = AutoIndentingWriter(@TypeOf(change_writer.writer())){ .indent_delta = indent_delta, .underlying_writer = change_writer.writer() };

    const end = end: {
        if (tree.root_nodes.len == 0) break :end tree.source.len;
        const first_tok = tree.firstToken(tree.root_nodes[0]);
        const tok_starts = tree.tokens.items(.start);
        break :end tok_starts[first_tok];
    };
    _ = try renderComments(tree, 0, end, &aiw);
    for (tree.root_nodes) |decl, i| {
        try renderNode(tree, decl, &aiw, .newline);
        if (isBlock(tree, decl)) {
            // render extra newlines after blocks
            try aiw.insertNewline();
            try aiw.insertNewline();
            continue;
        }

        if (i + 1 == tree.nodes.len) break;
        const last_token = tree.lastToken(decl);
        if (tree.lineDist(last_token, tree.nextToken(last_token)) > 1) {
            try aiw.insertNewline();
        }
    }

    return change_writer.changeDetected();
}

fn renderNode(tree: Tree, node: Node.Index, aiw: anytype, space: Space) @TypeOf(aiw.*).Error!void {
    const ids = tree.nodes.items(.id);
    const tokens = tree.nodes.items(.token);
    const data = tree.nodes.items(.data);
    switch (ids[node]) {
        .ident_expr,
        .string_expr,
        .int_expr,
        .num_expr,
        .true_expr,
        .false_expr,
        .this_expr,
        .null_expr,
        .discard_expr,
        .break_expr,
        .continue_expr,
        => try renderToken(tree, tokens[node], aiw, space),
        .mut_ident_expr => {
            try renderToken(tree, tree.prevToken(tokens[node]), aiw, .space);
            try renderToken(tree, tokens[node], aiw, space);
        },
        .bool_not_expr, .throw_expr => {
            try renderToken(tree, tokens[node], aiw, .space);
            try renderNode(tree, data[node].un, aiw, space);
        },
        .bit_not_expr, .negate_expr => {
            try renderToken(tree, tokens[node], aiw, .none);
            try renderNode(tree, data[node].un, aiw, space);
        },
        .member_access_expr => {
            try renderNode(tree, data[node].un, aiw, .none);
            try renderToken(tree, tree.prevToken(tokens[node]), aiw, .none);
            try renderToken(tree, tokens[node], aiw, space);
        },
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
        .bit_xor_assign,
        => {
            try renderNode(tree, data[node].bin.lhs, aiw, .space);
            const after_op_space: Space = if (tree.lineDist(tokens[node], tree.nextToken(tokens[node])) == 0) .space else .newline;

            aiw.pushIndent();
            try renderToken(tree, tokens[node], aiw, after_op_space);
            aiw.popIndent();

            aiw.pushIndentOneShot();
            try renderNode(tree, data[node].bin.rhs, aiw, space);
        },
        .map_item_expr => {
            if (data[node].bin.lhs != 0) {
                try renderNode(tree, data[node].bin.lhs, aiw, .space);
                const after_op_space: Space = if (tree.lineDist(tokens[node], tree.nextToken(tokens[node])) == 0) .space else .newline;

                aiw.pushIndent();
                try renderToken(tree, tokens[node], aiw, after_op_space);
                aiw.popIndent();

                aiw.pushIndentOneShot();
            }

            try renderNode(tree, data[node].bin.rhs, aiw, space);
        },
        .array_access_expr => {
            try renderNode(tree, data[node].bin.lhs, aiw, .none);
            try renderToken(tree, tokens[node], aiw, .none);
            try renderNode(tree, data[node].bin.rhs, aiw, .none);
            try renderToken(tree, tree.nextToken(tree.lastToken(data[node].bin.rhs)), aiw, space);
        },
        .as_expr, .is_expr => {
            try renderNode(tree, data[node].ty_bin.lhs, aiw, .space);
            try renderToken(tree, tokens[node], aiw, .space);
            try renderToken(tree, data[node].ty_bin.rhs, aiw, space);
        },
        .paren_expr => {
            try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, data[node].un, .none));
            try renderNode(tree, data[node].un, aiw, .none);
            try renderToken(tree, tree.lastToken(node), aiw, space);
        },
        .decl => {
            try renderToken(tree, tokens[node], aiw, .space);
            try renderNode(tree, data[node].bin.lhs, aiw, .space);
            const init_node = data[node].bin.rhs;
            const eq_token = tree.prevToken(tree.firstToken(init_node));
            try renderToken(tree, eq_token, aiw, getBlockIndent(tree, init_node, .space));
            try renderNode(tree, init_node, aiw, space);
        },
        .import_expr => {
            const str = tokens[node];
            const l_paren = tree.prevToken(str);
            try renderToken(tree, tree.prevToken(l_paren), aiw, .none);
            try renderToken(tree, l_paren, aiw, .none);
            try renderToken(tree, str, aiw, .none);
            try renderToken(tree, tree.nextToken(str), aiw, space);
        },
        .error_expr => {
            const initializer = data[node].un;
            const after_tok_space = if (initializer == 0) space else .none;
            try renderToken(tree, tokens[node], aiw, after_tok_space);
            if (initializer != 0) try renderNode(tree, initializer, aiw, space);
        },
        .enum_expr => {
            try renderToken(tree, tree.prevToken(tokens[node]), aiw, .none);
            const initializer = data[node].un;
            const after_tok_space = if (initializer == 0) space else .none;
            try renderToken(tree, tokens[node], aiw, after_tok_space);
            if (initializer != 0) try renderNode(tree, initializer, aiw, space);
        },
        .return_expr => {
            const expr = data[node].un;
            if (expr != 0) {
                try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, expr, .space));
                return renderNode(tree, expr, aiw, space);
            } else {
                try renderToken(tree, tokens[node], aiw, space);
            }
        },
        .range_expr,
        .range_expr_start,
        .range_expr_end,
        .range_expr_step,
        => {
            const range = Tree.Range.get(tree, node);

            if (range.start) |some| try renderNode(tree, some, aiw, .none);
            try renderToken(tree, range.colon_1, aiw, if (range.end == null and
                range.colon_2 == null and
                range.step == null) space else .none);
            if (range.end) |some|
                try renderNode(tree, some, aiw, if (range.colon_2 == null and
                    range.step == null) space else .none);
            if (range.colon_2) |some| try renderToken(tree, some, aiw, if (range.step == null) space else .none);
            if (range.step) |some| try renderNode(tree, some, aiw, space);
        },
        .while_expr, .while_let_expr => {
            const while_expr = Tree.While.get(tree, node);

            try renderToken(tree, while_expr.while_tok, aiw, .space);
            if (while_expr.capture) |some| {
                try renderToken(tree, while_expr.let_tok, aiw, .space);
                try renderNode(tree, some, aiw, .space);
                try renderToken(tree, while_expr.eq_tok, aiw, .space);
            }
            try renderNode(tree, while_expr.cond, aiw, getBlockIndent(tree, while_expr.body, .space));
            try renderNode(tree, while_expr.body, aiw, space);
        },
        .for_expr, .for_let_expr => {
            const for_expr = Tree.For.get(tree, node);

            try renderToken(tree, for_expr.for_tok, aiw, .space);
            if (for_expr.capture) |some| {
                try renderToken(tree, for_expr.let_tok, aiw, .space);
                try renderNode(tree, some, aiw, .space);
                try renderToken(tree, for_expr.in_tok, aiw, .space);
            }
            try renderNode(tree, for_expr.cond, aiw, getBlockIndent(tree, for_expr.body, .space));
            try renderNode(tree, for_expr.body, aiw, space);
        },
        .if_expr,
        .if_else_expr,
        .if_let_expr,
        .if_let_else_expr,
        => {
            const if_expr = Tree.If.get(tree, node);

            try renderToken(tree, if_expr.if_tok, aiw, .space);
            if (if_expr.capture) |some| {
                try renderToken(tree, if_expr.let_tok, aiw, .space);
                try renderNode(tree, some, aiw, .space);
                try renderToken(tree, if_expr.eq_tok, aiw, .space);
            }

            try renderNode(tree, if_expr.cond, aiw, getBlockIndent(tree, if_expr.then_body, .space));

            if (if_expr.else_body) |some| {
                try renderNode(tree, if_expr.then_body, aiw, .space);

                try renderToken(tree, if_expr.else_tok, aiw, getBlockIndent(tree, some, .space));
                try renderNode(tree, some, aiw, space);
            } else {
                try renderNode(tree, if_expr.then_body, aiw, space);
            }
        },
        .match_expr, .match_expr_one => {
            var buf: [2]Node.Index = undefined;
            const items = tree.nodeItems(node, &buf);

            try renderToken(tree, tokens[node], aiw, .space);
            try renderNode(tree, items[0], aiw, .newline);

            aiw.pushIndent();
            for (items[1..]) |case| {
                try renderNode(tree, case, aiw, .newline);
            }
            aiw.popIndent();
        },
        .match_case_catch_all => {
            try renderToken(tree, tree.prevToken(tokens[node]), aiw, .space);
            try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, data[node].un, .space));
            try renderNode(tree, data[node].un, aiw, space);
        },
        .match_case_let => {
            try renderToken(tree, tree.prevToken(tree.firstToken(data[node].bin.lhs)), aiw, .space);
            try renderNode(tree, data[node].bin.lhs, aiw, .space);

            try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, data[node].bin.rhs, .space));
            try renderNode(tree, data[node].bin.rhs, aiw, space);
        },
        .match_case, .match_case_one => {
            var buf: [2]Node.Index = undefined;
            const items = tree.nodeItems(node, &buf);
            const body = items[items.len - 1];

            try renderCommaList(tree, items[0 .. items.len - 1], tokens[node], aiw, .space);

            try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, body, .space));
            try renderNode(tree, body, aiw, space);
        },
        .fn_expr, .fn_expr_one => {
            var buf: [2]Node.Index = undefined;
            const items = tree.nodeItems(node, &buf);
            const params = items[@boolToInt(items[0] == 0) .. items.len - 1];
            const body = items[items.len - 1];
            const r_paren = tree.prevToken(tree.firstToken(body));

            try renderToken(tree, tokens[node], aiw, .none);
            try renderToken(tree, tree.nextToken(tokens[node]), aiw, .none);

            try renderCommaList(tree, params, r_paren, aiw, .none);

            try renderToken(tree, r_paren, aiw, getBlockIndent(tree, body, .space));
            return renderNode(tree, body, aiw, space);
        },
        .call_expr, .call_expr_one => {
            var buf: [2]Node.Index = undefined;
            const items = tree.nodeItems(node, &buf);

            const callee = items[0];
            const args = items[1..];
            const r_paren = tree.lastToken(node);

            try renderNode(tree, callee, aiw, .none);
            try renderToken(tree, tokens[node], aiw, .none);
            try renderCommaList(tree, args, r_paren, aiw, .none);
            try renderToken(tree, r_paren, aiw, space);
        },
        .tuple_expr,
        .tuple_expr_two,
        .list_expr,
        .list_expr_two,
        .map_expr,
        .map_expr_two,
        => {
            var buf: [2]Node.Index = undefined;
            const items = tree.nodeItems(node, &buf);

            try renderToken(tree, tokens[node], aiw, .none);
            const last_token = tree.lastToken(node);
            try renderCommaList(tree, items, last_token, aiw, .none);
            try renderToken(tree, last_token, aiw, space);
        },
        .block_stmt_two, .block_stmt => {
            var buf: [2]Node.Index = undefined;
            const stmts = tree.nodeItems(node, &buf);

            aiw.pushIndent();
            for (stmts) |stmt, i| {
                try renderNode(tree, stmt, aiw, .newline);

                if (isBlock(tree, stmt)) {
                    // render extra newlines after blocks
                    try aiw.insertNewline();
                    try aiw.insertNewline();
                    continue;
                }

                if (i + 1 == stmts.len) break;
                const last_token = tree.lastToken(stmt);
                if (tree.lineDist(last_token, tree.nextToken(last_token)) > 1) {
                    try aiw.insertNewline();
                }
            }
            aiw.popIndent();
        },
        .try_expr, .try_expr_one => {
            var buf: [2]Node.Index = undefined;
            const stmts = tree.nodeItems(node, &buf);

            try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, stmts[0], .space));
            try renderNode(tree, stmts[0], aiw, .space);

            for (stmts[1..]) |catch_expr, i| {
                if (i + 1 == stmts.len - 1) {
                    try renderNode(tree, catch_expr, aiw, space);
                } else {
                    try renderNode(tree, catch_expr, aiw, .space);
                }
            }
        },
        .catch_let_expr => {
            const capture = data[node].bin.lhs;
            const body = data[node].bin.rhs;
            try renderToken(tree, tokens[node], aiw, .space);
            try renderToken(tree, tree.nextToken(tokens[node]), aiw, .space);
            try renderNode(tree, capture, aiw, getBlockIndent(tree, body, .space));
            try renderNode(tree, body, aiw, space);
        },
        .catch_expr => {
            const capture = data[node].bin.lhs;
            const body = data[node].bin.rhs;
            if (capture != 0) {
                try renderToken(tree, tokens[node], aiw, .space);
                try renderNode(tree, capture, aiw, getBlockIndent(tree, body, .space));
            } else {
                try renderToken(tree, tokens[node], aiw, getBlockIndent(tree, body, .space));
            }
            try renderNode(tree, body, aiw, space);
        },
        .format_expr => {
            const token_ids = tree.tokens.items(.id);
            const exprs = data[node].format.exprs(tree.extra);
            for (data[node].format.str(tree.extra)) |str, i| {
                if (token_ids[str] == .format_end) {
                    return renderToken(tree, str, aiw, space);
                }
                try renderToken(tree, str, aiw, .none);
                try renderNode(tree, exprs[i], aiw, .none);
            }
        },
    }
}

fn renderCommaList(tree: Tree, nodes: []const Node.Index, last_token: Token.Index, aiw: anytype, space: Space) !void {
    if (nodes.len == 0) return;

    const ids = tree.tokens.items(.id);
    const prev = ids[last_token - 1];
    if (prev == .comma or prev == .nl or hasComment(tree, tree.prevToken(last_token), last_token) or
        tree.lineDist(tree.firstToken(nodes[0]), last_token) > 0)
    {
        try aiw.insertNewline();
        aiw.pushIndent();
        for (nodes) |node| {
            try renderNode(tree, node, aiw, .comma);
        }
        aiw.popIndent();
    } else {
        for (nodes) |node, i| {
            if (i + 1 == nodes.len) {
                try renderNode(tree, node, aiw, space);
                break;
            }
            try renderNode(tree, node, aiw, .none);
            try renderToken(tree, tree.nextToken(tree.lastToken(node)), aiw, .space);
        }
    }
}

fn hasComment(tree: Tree, start_token: Token.Index, end_token: Token.Index) bool {
    const starts = tree.tokens.items(.start);
    const slice = tree.source[starts[start_token]..starts[end_token]];
    return mem.indexOfScalar(u8, slice, '#') != null;
}

/// Assumes that start is the first byte past the previous token and
/// that end is the last byte before the next token.
fn renderComments(tree: Tree, start: usize, end: usize, aiw: anytype) !bool {
    // borrowed from std/zig/render.zig
    var index: usize = start;
    while (mem.indexOfScalar(u8, tree.source[index..end], '#')) |offset| {
        const comment_start = index + offset;

        // If there is no newline, the comment ends with EOF
        const newline_index = mem.indexOfScalar(u8, tree.source[comment_start..end], '\n');
        const newline = if (newline_index) |i| comment_start + i else null;

        const untrimmed_comment = tree.source[comment_start .. newline orelse tree.source.len];
        const trimmed_comment = mem.trimRight(u8, untrimmed_comment, &std.ascii.spaces);

        // Don't leave any whitespace at the start of the file
        if (index != 0) {
            if (index == start and mem.containsAtLeast(u8, tree.source[index..comment_start], 2, "\n")) {
                // Leave up to one empty line before the first comment
                try aiw.insertNewline();
                try aiw.insertNewline();
            } else if (mem.indexOfScalar(u8, tree.source[index..comment_start], '\n') != null) {
                // Respect the newline directly before the comment.
                // Note: This allows an empty line between comments
                try aiw.insertNewline();
            } else if (index == start) {
                // Otherwise if the first comment is on the same line as
                // the token before it, prefix it with a single space.
                try aiw.writer().writeByte(' ');
            }
        }

        index = 1 + (newline orelse end - 1);

        const comment_content = mem.trimLeft(u8, trimmed_comment["//".len..], &std.ascii.spaces);
        if (aiw.disabled_offset != null and mem.eql(u8, comment_content, "bog fmt: on")) {
            // Write the source for which formatting was disabled directly
            // to the underlying writer, fixing up invaild whitespace.
            const disabled_source = tree.source[aiw.disabled_offset.?..comment_start];
            try writeFixingWhitespace(aiw.underlying_writer, disabled_source);
            // Write with the canonical single space.
            try aiw.underlying_writer.writeAll("// bog fmt: on\n");
            aiw.disabled_offset = null;
        } else if (aiw.disabled_offset == null and mem.eql(u8, comment_content, "bog fmt: off")) {
            // Write with the canonical single space.
            try aiw.writer().writeAll("// bog fmt: off\n");
            aiw.disabled_offset = index;
        } else {
            // Write the comment minus trailing whitespace.
            try aiw.writer().print("{s}\n", .{trimmed_comment});
        }
    }

    if (index != start and mem.containsAtLeast(u8, tree.source[index - 1 .. end], 2, "\n")) {
        try aiw.insertNewline();
    }

    return index != start;
}

fn writeFixingWhitespace(writer: anytype, slice: []const u8) !void {
    for (slice) |byte| switch (byte) {
        '\t' => try writer.writeAll(" " ** 4),
        '\r' => {},
        else => try writer.writeByte(byte),
    };
}

fn getBlockIndent(tree: Tree, node: Node.Index, space: Space) Space {
    const ids = tree.nodes.items(.id);
    return switch (ids[node]) {
        .match_expr,
        .match_expr_one,
        .block_stmt,
        .block_stmt_two,
        => .newline,
        else => space,
    };
}

fn isBlock(tree: Tree, node: Node.Index) bool {
    const ids = tree.nodes.items(.id);
    const data = tree.nodes.items(.data);
    var cur = node;
    while (true) switch (ids[cur]) {
        .match_expr => return !isBlock(tree, tree.extra[data[cur].range.end - 1]),
        .block_stmt => return !isBlock(tree, tree.extra[data[cur].range.end - 1]),
        .match_expr_one => return !isBlock(tree, data[cur].bin.rhs),
        .block_stmt_two => {
            const bin = data[cur].bin;
            if (bin.rhs != 0) cur = bin.rhs else cur = bin.lhs;
            return !isBlock(tree, cur);
        },
        .if_expr => cur = data[cur].bin.rhs,
        .if_else_expr, .if_let_expr => cur = tree.extra[data[cur].cond.extra + 1],
        .if_let_else_expr => cur = tree.extra[data[cur].cond.extra + 2],
        .for_expr => cur = data[cur].bin.rhs,
        .for_let_expr => cur = tree.extra[data[cur].cond.extra + 1],
        .while_expr => cur = data[cur].bin.rhs,
        .while_let_expr => cur = tree.extra[data[cur].cond.extra + 1],
        .decl => cur = data[cur].bin.rhs,
        .fn_expr => cur = tree.extra[data[cur].range.end - 1],
        .fn_expr_one => cur = data[cur].bin.rhs,
        .try_expr => cur = tree.extra[data[cur].range.end - 1],
        .try_expr_one => {
            const bin = data[cur].bin;
            if (bin.rhs != 0)
                cur = bin.rhs
            else
                cur = bin.lhs;
        },
        .catch_expr, .catch_let_expr => cur = data[cur].bin.rhs,
        else => return false,
    };
}

const Space = enum {
    none,
    newline,
    space,
    comma,
};

fn renderToken(tree: Tree, token: Token.Index, aiw: anytype, space: Space) !void {
    const slice = tree.tokenSlice(token);
    const starts = tree.tokens.items(.end);
    const ends = tree.tokens.items(.end);
    try aiw.writer().writeAll(slice);
    switch (space) {
        .none => {},
        .newline => try aiw.insertNewline(),
        .space => try aiw.writer().writeByte(' '),
        .comma => {
            try aiw.writer().writeByte(',');
            const comma = tree.nextToken(token);
            const saw_comments = try renderComments(tree, ends[token], starts[comma], aiw);
            if (!saw_comments) try aiw.insertNewline();
            return;
        },
    }
    const next = tree.nextToken(token);
    _ = try renderComments(tree, ends[token], starts[next], aiw);
}

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
fn AutoIndentingWriter(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const Error = UnderlyingWriter.Error;
        pub const Writer = std.io.Writer(*Self, Error, write);

        underlying_writer: UnderlyingWriter,

        /// Offset into the source at which formatting has been disabled with
        /// a `zig fmt: off` comment.
        ///
        /// If non-null, the AutoIndentingStream will not write any bytes
        /// to the underlying writer. It will however continue to track the
        /// indentation level.
        disabled_offset: ?usize = null,

        indent_count: usize = 0,
        indent_delta: usize,
        current_line_empty: bool = true,
        /// automatically popped when applied
        indent_one_shot_count: usize = 0,
        /// the most recently applied indent
        applied_indent: usize = 0,
        /// not used until the next line
        indent_next_line: usize = 0,

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            try self.applyIndent();
            return self.writeNoIndent(bytes);
        }

        // Change the indent delta without changing the final indentation level
        pub fn setIndentDelta(self: *Self, new_indent_delta: usize) void {
            if (self.indent_delta == new_indent_delta) {
                return;
            } else if (self.indent_delta > new_indent_delta) {
                assert(self.indent_delta % new_indent_delta == 0);
                self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
            } else {
                // assert that the current indentation (in spaces) in a multiple of the new delta
                assert((self.indent_count * self.indent_delta) % new_indent_delta == 0);
                self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
            }
            self.indent_delta = new_indent_delta;
        }

        fn writeNoIndent(self: *Self, bytes: []const u8) Error!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n')
                self.resetLine();
            return bytes.len;
        }

        pub fn insertNewline(self: *Self) Error!void {
            _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) Error!void {
            if (!self.current_line_empty)
                try self.insertNewline();
        }

        /// Push default indentation
        /// Doesn't actually write any indentation.
        /// Just primes the stream to be able to write the correct indentation if it needs to.
        pub fn pushIndent(self: *Self) void {
            self.indent_count += 1;
        }

        /// Push an indent that is automatically popped after being applied
        pub fn pushIndentOneShot(self: *Self) void {
            self.indent_one_shot_count += 1;
            self.pushIndent();
        }

        /// Turns all one-shot indents into regular indents
        /// Returns number of indents that must now be manually popped
        pub fn lockOneShotIndent(self: *Self) usize {
            var locked_count = self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            return locked_count;
        }

        /// Push an indent that should not take effect until the next line
        pub fn pushIndentNextLine(self: *Self) void {
            self.indent_next_line += 1;
            self.pushIndent();
        }

        pub fn popIndent(self: *Self) void {
            assert(self.indent_count != 0);
            self.indent_count -= 1;

            if (self.indent_next_line > 0)
                self.indent_next_line -= 1;
        }

        /// Writes ' ' bytes if the current line is empty
        fn applyIndent(self: *Self) Error!void {
            const current_indent = self.currentIndent();
            if (self.current_line_empty and current_indent > 0) {
                if (self.disabled_offset == null) {
                    try self.underlying_writer.writeByteNTimes(' ', current_indent);
                }
                self.applied_indent = current_indent;
            }

            self.indent_count -= self.indent_one_shot_count;
            self.indent_one_shot_count = 0;
            self.current_line_empty = false;
        }

        /// Checks to see if the most recent indentation exceeds the currently pushed indents
        pub fn isLineOverIndented(self: *Self) bool {
            if (self.current_line_empty) return false;
            return self.applied_indent > self.currentIndent();
        }

        fn currentIndent(self: *Self) usize {
            var indent_current: usize = 0;
            if (self.indent_count > 0) {
                const indent_count = self.indent_count - self.indent_next_line;
                indent_current = indent_count * self.indent_delta;
            }
            return indent_current;
        }
    };
}
