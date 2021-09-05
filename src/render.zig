const std = @import("std");
const bog = @import("bog.zig");
const Tree = bog.Tree;
const Node = bog.Node;
const TokenIndex = bog.Token.Index;
const changeDetectionWriter = std.io.changeDetectionStream;

const indent_delta = 4;

pub fn render(tree: *Tree, writer: anytype) @TypeOf(writer).Error!bool {
    var renderer = Renderer{
        .source = tree.source,
        .tokens = tree.tokens,
    };

    var change_writer = changeDetectionWriter(tree.source, writer);
    var indent_writer = AutoIndentingStream(@TypeOf(change_writer.writer())){ .indent_delta = indent_delta, .underlying_writer = change_writer.writer() };

    try renderer.renderComments(0, &indent_writer, .newline);
    for (tree.nodes) |node, i| {
        try renderer.renderNode(node, &indent_writer, .newline);
        if (Renderer.isBlock(node)) {
            // render extra newlines after blocks
            try indent_writer.insertNewline();
            try indent_writer.insertNewline();
            continue;
        }

        if (i + 1 == tree.nodes.len) break;
        const last_token = node.lastToken();
        if (renderer.lineDist(last_token, renderer.nextToken(last_token)) > 1) {
            try indent_writer.writer().writeByte('\n');
        }
    }

    return change_writer.changeDetected();
}

const Renderer = struct {
    source: []const u8,
    tokens: []const bog.Token,

    fn prevToken(self: *Renderer, tok: TokenIndex) TokenIndex {
        var index = tok - 1;
        while (true) {
            switch (self.tokens[index].id) {
                .Comment, .Nl, .Indent => index -= 1,
                else => break,
            }
        }
        return index;
    }

    fn nextToken(self: *Renderer, tok: TokenIndex) TokenIndex {
        var index = tok + 1;
        while (true) {
            switch (self.tokens[index].id) {
                .Comment, .Nl, .Indent => index += 1,
                else => break,
            }
        }
        return index;
    }

    fn lineDist(self: *Renderer, a: TokenIndex, b: TokenIndex) u32 {
        const first_end = self.tokens[a].end;
        const second_start = self.tokens[b].start;
        var i = first_end;
        var count: u32 = 0;
        while (i < second_start) : (i += 1) {
            if (self.source[i] == '\n') {
                count += 1;
            }
        }
        return count;
    }

    fn renderNode(self: *Renderer, node: *Node, writer: anytype, space: Space) @TypeOf(writer.*).WriteError!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                if (literal.kind == .none) {
                    try self.renderToken(self.prevToken(literal.tok), writer, .none);
                }

                return self.renderToken(literal.tok, writer, space);
            },
            .Infix => {
                const infix = @fieldParentPtr(Node.Infix, "base", node);

                try self.renderNode(infix.lhs, writer, .space);
                const after_op_space: Space = if (self.lineDist(infix.tok, self.nextToken(infix.tok)) == 0) .space else .newline;

                writer.pushIndent();
                try self.renderToken(infix.tok, writer, after_op_space);
                writer.popIndent();

                writer.pushIndentOneShot();
                return self.renderNode(infix.rhs, writer, space);
            },
            .Range => {
                const range = @fieldParentPtr(Node.Range, "base", node);

                if (range.start) |some| try self.renderNode(some, writer, .none);
                try self.renderToken(range.colon_1, writer, if (range.end == null and
                    range.colon_2 == null and
                    range.step == null) space else .none);
                if (range.end) |some|
                    try self.renderNode(some, writer, if (range.colon_2 == null and
                        range.step == null) space else .none);
                if (range.colon_2) |some| try self.renderToken(some, writer, if (range.step == null) space else .none);
                if (range.step) |some| try self.renderNode(some, writer, space);
            },
            .Prefix => {
                const prefix = @fieldParentPtr(Node.Prefix, "base", node);

                switch (prefix.op) {
                    .bool_not => try self.renderToken(prefix.tok, writer, .space),
                    .bit_not, .minus, .plus => try self.renderToken(prefix.tok, writer, .none),
                }
                return self.renderNode(prefix.rhs, writer, space);
            },
            .Grouped => {
                const grouped = @fieldParentPtr(Node.Grouped, "base", node);

                try self.renderToken(grouped.l_tok, writer, getBlockIndent(grouped.expr, .none));
                try self.renderNode(grouped.expr, writer, .none);
                return self.renderToken(grouped.r_tok, writer, space);
            },
            .TypeInfix => {
                const type_infix = @fieldParentPtr(Node.TypeInfix, "base", node);

                try self.renderNode(type_infix.lhs, writer, .space);
                try self.renderToken(type_infix.tok, writer, .space);
                return self.renderToken(type_infix.type_tok, writer, space);
            },
            .Discard, .Identifier, .This => {
                const single = @fieldParentPtr(Node.SingleToken, "base", node);

                return self.renderToken(single.tok, writer, space);
            },
            .Suffix => {
                const suffix = @fieldParentPtr(Node.Suffix, "base", node);

                try self.renderNode(suffix.lhs, writer, .none);
                try self.renderToken(suffix.l_tok, writer, .none);
                switch (suffix.op) {
                    .call => |params| try self.renderCommaList(params, suffix.r_tok, writer, .none),
                    .subscript => |arr_node| try self.renderNode(arr_node, writer, .none),
                    .member => {},
                }
                return self.renderToken(suffix.r_tok, writer, space);
            },
            .Decl => {
                const decl = @fieldParentPtr(Node.Decl, "base", node);

                try self.renderToken(decl.let_const, writer, .space);
                try self.renderNode(decl.capture, writer, .space);
                try self.renderToken(decl.eq_tok, writer, getBlockIndent(decl.value, .space));
                return self.renderNode(decl.value, writer, space);
            },
            .Import => {
                const import = @fieldParentPtr(Node.Import, "base", node);

                try self.renderToken(import.tok, writer, .none);
                try self.renderToken(self.nextToken(import.tok), writer, .none);
                try self.renderToken(import.str_tok, writer, .none);
                return self.renderToken(import.r_paren, writer, space);
            },
            .Error => {
                const err = @fieldParentPtr(Node.Error, "base", node);

                const after_tok_space = if (err.capture == null) space else .none;
                try self.renderToken(err.tok, writer, after_tok_space);
                if (err.capture) |some| try self.renderNode(some, writer, space);
            },
            .Tagged => {
                const tag = @fieldParentPtr(Node.Tagged, "base", node);

                try self.renderToken(tag.at, writer, .none);
                const after_tok_space = if (tag.capture == null) space else .none;
                try self.renderToken(tag.name, writer, after_tok_space);
                if (tag.capture) |some| try self.renderNode(some, writer, space);
            },
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);

                switch (jump.op) {
                    .Return => |expr| {
                        if (expr) |some| {
                            try self.renderToken(jump.tok, writer, getBlockIndent(some, .space));
                            return self.renderNode(some, writer, space);
                        } else {
                            try self.renderToken(jump.tok, writer, space);
                        }
                    },
                    .Continue, .Break => try self.renderToken(jump.tok, writer, space),
                }
            },
            .While => {
                const while_expr = @fieldParentPtr(Node.While, "base", node);

                try self.renderToken(while_expr.while_tok, writer, .space);
                try self.renderToken(self.nextToken(while_expr.while_tok), writer, .none);
                if (while_expr.capture) |some| {
                    try self.renderToken(while_expr.let_const.?, writer, .space);
                    try self.renderNode(some, writer, .space);
                    try self.renderToken(while_expr.eq_tok.?, writer, .space);
                }
                try self.renderNode(while_expr.cond, writer, .none);
                try self.renderToken(while_expr.r_paren, writer, getBlockIndent(while_expr.body, .space));
                return self.renderNode(while_expr.body, writer, space);
            },
            .For => {
                const for_expr = @fieldParentPtr(Node.For, "base", node);

                try self.renderToken(for_expr.for_tok, writer, .space);
                try self.renderToken(self.nextToken(for_expr.for_tok), writer, .none);
                if (for_expr.capture) |some| {
                    try self.renderToken(for_expr.let_const.?, writer, .space);
                    try self.renderNode(some, writer, .space);
                    try self.renderToken(for_expr.in_tok.?, writer, .space);
                }
                try self.renderNode(for_expr.cond, writer, .none);

                try self.renderToken(for_expr.r_paren, writer, getBlockIndent(for_expr.body, .space));
                return self.renderNode(for_expr.body, writer, space);
            },
            .Fn => {
                const fn_expr = @fieldParentPtr(Node.Fn, "base", node);

                try self.renderToken(fn_expr.fn_tok, writer, .none);
                try self.renderToken(self.nextToken(fn_expr.fn_tok), writer, .none);

                try self.renderCommaList(fn_expr.params, fn_expr.r_paren, writer, .none);

                try self.renderToken(fn_expr.r_paren, writer, getBlockIndent(fn_expr.body, .space));
                return self.renderNode(fn_expr.body, writer, space);
            },
            .List, .Tuple, .Map => {
                const ltm = @fieldParentPtr(Node.ListTupleMap, "base", node);

                try self.renderToken(ltm.l_tok, writer, .none);
                try self.renderCommaList(ltm.values, ltm.r_tok, writer, .none);

                return self.renderToken(ltm.r_tok, writer, space);
            },
            .Block => {
                const blk = @fieldParentPtr(Node.Block, "base", node);

                writer.pushIndent();
                for (blk.stmts) |stmt, i| {
                    try self.renderNode(stmt, writer, .newline);

                    if (isBlock(stmt)) {
                        // render extra newlines after blocks
                        try writer.insertNewline();
                        try writer.insertNewline();
                        continue;
                    }

                    if (i + 1 == blk.stmts.len) break;
                    const last_token = stmt.lastToken();
                    if (self.lineDist(last_token, self.nextToken(last_token)) > 1) {
                        try writer.insertNewline();
                    }
                }
                writer.popIndent();
            },
            .MapItem => {
                const item = @fieldParentPtr(Node.MapItem, "base", node);

                if (item.key) |some| {
                    try self.renderNode(some, writer, .none);
                    try self.renderToken(item.colon.?, writer, .space);
                }

                return self.renderNode(item.value, writer, space);
            },
            .Try => {
                const try_expr = @fieldParentPtr(Node.Try, "base", node);

                try self.renderToken(try_expr.tok, writer, getBlockIndent(try_expr.expr, .space));
                try self.renderNode(try_expr.expr, writer, .space);

                for (try_expr.catches) |catch_expr, i| {
                    if (i + 1 == try_expr.catches.len) {
                        try self.renderNode(catch_expr, writer, space);
                    } else {
                        try self.renderNode(catch_expr, writer, .space);
                    }
                }
            },
            .Catch => {
                const catch_expr = @fieldParentPtr(Node.Catch, "base", node);

                if (catch_expr.capture) |some| {
                    try self.renderToken(catch_expr.tok, writer, .space);

                    try self.renderToken(self.nextToken(catch_expr.tok), writer, .none);
                    if (catch_expr.let_const) |tok| {
                        try self.renderToken(tok, writer, .space);
                    }
                    try self.renderNode(some, writer, .none);
                    try self.renderToken(self.nextToken(some.lastToken()), writer, getBlockIndent(catch_expr.expr, .space));
                } else {
                    try self.renderToken(catch_expr.tok, writer, getBlockIndent(catch_expr.expr, .space));
                }
                return self.renderNode(catch_expr.expr, writer, space);
            },
            .If => {
                const if_expr = @fieldParentPtr(Node.If, "base", node);

                try self.renderToken(if_expr.if_tok, writer, .space);
                try self.renderToken(self.nextToken(if_expr.if_tok), writer, .none);
                if (if_expr.capture) |some| {
                    try self.renderToken(if_expr.let_const.?, writer, .space);
                    try self.renderNode(some, writer, .space);
                    try self.renderToken(if_expr.eq_tok.?, writer, .space);
                }

                try self.renderNode(if_expr.cond, writer, .none);
                try self.renderToken(if_expr.r_paren, writer, getBlockIndent(if_expr.if_body, .space));

                if (if_expr.else_body) |some| {
                    try self.renderNode(if_expr.if_body, writer, .space);

                    try self.renderToken(if_expr.else_tok.?, writer, getBlockIndent(some, .space));
                    return self.renderNode(some, writer, space);
                } else {
                    return self.renderNode(if_expr.if_body, writer, space);
                }
            },
            .Match => {
                const match_expr = @fieldParentPtr(Node.Match, "base", node);

                try self.renderToken(match_expr.match_tok, writer, .space);
                try self.renderToken(self.nextToken(match_expr.match_tok), writer, .none);
                try self.renderNode(match_expr.expr, writer, .none);
                try self.renderToken(match_expr.r_paren, writer, .newline);

                writer.pushIndent();
                for (match_expr.cases) |case| {
                    try self.renderNode(case, writer, .newline);
                }
                writer.popIndent();
            },
            .MatchCatchAll => {
                const case = @fieldParentPtr(Node.MatchCatchAll, "base", node);
                try self.renderToken(case.tok, writer, .space);

                try self.renderToken(case.eq_arr, writer, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, writer, space);
            },
            .MatchLet => {
                const case = @fieldParentPtr(Node.MatchLet, "base", node);

                try self.renderToken(case.let_const, writer, .space);
                try self.renderNode(case.capture, writer, .space);

                try self.renderToken(case.eq_arr, writer, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, writer, space);
            },
            .MatchCase => {
                const case = @fieldParentPtr(Node.MatchCase, "base", node);

                try self.renderCommaList(case.items, case.eq_arr, writer, .space);

                try self.renderToken(case.eq_arr, writer, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, writer, space);
            },
            .FormatString => {
                const fmt_str = @fieldParentPtr(Node.FormatString, "base", node);

                for (fmt_str.format) |str, i| {
                    if (self.tokens[str].id == .FormatEnd) {
                        return self.renderToken(str, writer, space);
                    }
                    try self.renderToken(str, writer, .none);
                    try self.renderNode(fmt_str.args[i], writer, .none);
                }
            },
        }
    }

    fn renderCommaList(self: *Renderer, nodes: []const *Node, last_token: TokenIndex, writer: anytype, space: Space) !void {
        if (nodes.len == 0) return;

        const prev = self.tokens[last_token - 1].id;
        if (prev == .Comma or prev == .Nl or prev == .Comment or self.lineDist(nodes[0].firstToken(), last_token) > 0) {
            try writer.insertNewline();
            writer.pushIndent();
            for (nodes) |node| {
                try self.renderNode(node, writer, .comma);
            }
            writer.popIndent();
        } else {
            for (nodes) |node, i| {
                if (i + 1 == nodes.len) {
                    try self.renderNode(node, writer, space);
                    break;
                }
                try self.renderNode(node, writer, .none);
                try self.renderToken(self.nextToken(node.lastToken()), writer, .space);
            }
        }
    }

    fn renderComments(self: *Renderer, token: TokenIndex, writer: anytype, space: Space) !void {
        _ = space;
        var i = token;
        var last_token = i;
        while (true) : (i += 1) {
            switch (self.tokens[i].id) {
                .Nl, .Indent => continue,
                .Comment => {},
                else => break,
            }
            var tok = self.tokens[i];

            const slice = self.source[tok.start..tok.end];
            const trimmed = std.mem.trimRight(u8, slice, " \t\r");
            if (trimmed.len == 1) continue;

            if (self.lineDist(last_token, i) > 1) {
                // insert extra new line between separate comments
                try writer.insertNewline();
            }
            last_token = i;

            try writer.writer().writeAll(trimmed);
            try writer.insertNewline();
        }
    }

    fn getBlockIndent(node: *Node, space: Space) Space {
        return switch (node.id) {
            .Block, .Match => .newline,
            else => space,
        };
    }

    fn isBlock(node: *Node) bool {
        switch (node.id) {
            .Match => {
                const match_node = @fieldParentPtr(Node.Match, "base", node);
                return !isBlock(match_node.cases[match_node.cases.len - 1]);
            },
            .Block => {
                const block_node = @fieldParentPtr(Node.Block, "base", node);
                return !isBlock(block_node.stmts[block_node.stmts.len - 1]);
            },
            .If => {
                const if_node = @fieldParentPtr(Node.If, "base", node);
                if (if_node.else_body) |some|
                    return isBlock(some);
                return isBlock(if_node.if_body);
            },
            .For => {
                const for_node = @fieldParentPtr(Node.For, "base", node);
                return isBlock(for_node.body);
            },
            .While => {
                const while_node = @fieldParentPtr(Node.While, "base", node);
                return isBlock(while_node.body);
            },
            .Decl => {
                const decl = @fieldParentPtr(Node.Decl, "base", node);
                return isBlock(decl.value);
            },
            .Fn => {
                const fn_node = @fieldParentPtr(Node.Fn, "base", node);
                return isBlock(fn_node.body);
            },
            .Try => {
                const try_node = @fieldParentPtr(Node.Try, "base", node);
                return isBlock(try_node.expr);
            },
            .Catch => {
                const catch_node = @fieldParentPtr(Node.Catch, "base", node);
                return isBlock(catch_node.expr);
            },
            else => return false,
        }
    }

    const Space = enum {
        none,
        newline,
        space,
        comma,
    };

    fn renderToken(self: *Renderer, token: TokenIndex, writer: anytype, space: Space) !void {
        var tok = self.tokens[token];
        try writer.writer().writeAll(self.source[tok.start..tok.end]);
        switch (space) {
            .none => {},
            .newline => try writer.insertNewline(),
            .space => try writer.writer().writeByte(' '),
            .comma => {
                try writer.writer().writeByte(',');
                var comments = token + 1;
                var i = comments;
                while (true) : (i += 1) {
                    switch (self.tokens[i].id) {
                        .Nl, .Indent => continue,
                        .Comma => comments += 1,
                        .Comment => {
                            try writer.writer().writeByte(' ');
                            break;
                        },
                        else => return writer.insertNewline(),
                    }
                }
                return self.renderComments(comments, writer, space);
            },
        }
        try self.renderComments(token + 1, writer, space);
    }
};

/// Automatically inserts indentation of written data by keeping
/// track of the current indentation level
fn AutoIndentingStream(comptime UnderlyingWriter: type) type {
    return struct {
        const Self = @This();
        pub const WriteError = UnderlyingWriter.Error;
        pub const Writer = std.io.Writer(*Self, WriteError, write);

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

        pub fn write(self: *Self, bytes: []const u8) WriteError!usize {
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
                std.debug.assert(self.indent_delta % new_indent_delta == 0);
                self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
            } else {
                // assert that the current indentation (in spaces) in a multiple of the new delta
                std.debug.assert((self.indent_count * self.indent_delta) % new_indent_delta == 0);
                self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
            }
            self.indent_delta = new_indent_delta;
        }

        fn writeNoIndent(self: *Self, bytes: []const u8) WriteError!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n')
                self.resetLine();
            return bytes.len;
        }

        pub fn insertNewline(self: *Self) WriteError!void {
            _ = try self.writeNoIndent("\n");
        }

        fn resetLine(self: *Self) void {
            self.current_line_empty = true;
            self.indent_next_line = 0;
        }

        /// Insert a newline unless the current line is blank
        pub fn maybeInsertNewline(self: *Self) WriteError!void {
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

        // Push an indent that should not take effect until the next line
        pub fn pushIndentNextLine(self: *Self) void {
            self.indent_next_line += 1;
            self.pushIndent();
        }

        pub fn popIndent(self: *Self) void {
            std.debug.assert(self.indent_count != 0);
            self.indent_count -= 1;

            if (self.indent_next_line > 0)
                self.indent_next_line -= 1;
        }

        /// Writes ' ' bytes if the current line is empty
        fn applyIndent(self: *Self) WriteError!void {
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

        // Checks to see if the most recent indentation exceeds the currently pushed indents
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
