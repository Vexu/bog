const std = @import("std");
const bog = @import("bog.zig");
const Tree = bog.Tree;
const Node = bog.Node;
const TokenIndex = bog.Token.Index;

pub fn render(tree: *Tree, writer: anytype) @TypeOf(writer).Error!void {
    var renderer = Renderer{
        .source = tree.source,
        .tokens = tree.tokens,
    };
    try renderer.renderComments(0, writer, 0, .newline);
    for (tree.nodes) |node, i| {
        try renderer.renderNode(node, writer, 0, .newline);
        if (Renderer.isBlock(node)) {
            // render extra newline after blocks
            try writer.writeAll("\n\n");
            continue;
        }

        if (i + 1 == tree.nodes.len) break;
        const last_token = node.lastToken();
        if (renderer.lineDist(last_token, renderer.nextToken(last_token)) > 1) {
            try writer.writeByte('\n');
        }
    }
}

const Renderer = struct {
    source: []const u8,
    tokens: []const bog.Token,

    const indent_delta = 4;

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

    fn renderNode(self: *Renderer, node: *Node, writer: anytype, indent: u32, space: Space) @TypeOf(writer).Error!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                if (literal.kind == .none) {
                    try self.renderToken(self.prevToken(literal.tok), writer, indent, .none);
                }

                return self.renderToken(literal.tok, writer, indent, space);
            },
            .Infix => {
                const infix = @fieldParentPtr(Node.Infix, "base", node);

                try self.renderNode(infix.lhs, writer, indent, .space);
                try self.renderToken(infix.tok, writer, indent, .space);
                return self.renderNode(infix.rhs, writer, indent, space);
            },
            .Range => {
                const range = @fieldParentPtr(Node.Range, "base", node);

                if (range.start) |some| try self.renderNode(some, stream, indent, .none);
                try self.renderToken(range.colon_1, stream, indent, if (range.end == null and
                    range.colon_2 == null and
                    range.step == null) space else .none);
                if (range.end) |some|
                    try self.renderNode(some, stream, indent, if (range.colon_2 == null and
                        range.step == null) space else .none);
                if (range.colon_2) |some| try self.renderToken(some, stream, indent, if (range.step == null) space else .none);
                if (range.step) |some| try self.renderNode(some, stream, indent, space);
            },
            .Prefix => {
                const prefix = @fieldParentPtr(Node.Prefix, "base", node);

                switch (prefix.op) {
                    .boolNot, .Try => try self.renderToken(prefix.tok, writer, indent, .space),
                    .bitNot, .minus, .plus => try self.renderToken(prefix.tok, writer, indent, .none),
                }
                return self.renderNode(prefix.rhs, writer, indent, space);
            },
            .Grouped => {
                const grouped = @fieldParentPtr(Node.Grouped, "base", node);

                try self.renderToken(grouped.l_tok, writer, indent, getBlockIndent(grouped.expr, .none));
                try self.renderNode(grouped.expr, writer, indent, .none);
                return self.renderToken(grouped.r_tok, writer, indent, space);
            },
            .TypeInfix => {
                const type_infix = @fieldParentPtr(Node.TypeInfix, "base", node);

                try self.renderNode(type_infix.lhs, writer, indent, .space);
                try self.renderToken(type_infix.tok, writer, indent, .space);
                return self.renderToken(type_infix.type_tok, writer, indent, space);
            },
            .Discard, .Identifier, .This => {
                const single = @fieldParentPtr(Node.SingleToken, "base", node);

                return self.renderToken(single.tok, writer, indent, space);
            },
            .Suffix => {
                const suffix = @fieldParentPtr(Node.Suffix, "base", node);

                try self.renderNode(suffix.lhs, writer, indent, .none);
                try self.renderToken(suffix.l_tok, writer, indent, .none);
                switch (suffix.op) {
                    .call => |params| try self.renderCommaList(params, suffix.r_tok, writer, indent, .none),
                    .subscript => |arr_node| try self.renderNode(arr_node, writer, indent, .none),
                    .member => {},
                }
                return self.renderToken(suffix.r_tok, writer, indent, space);
            },
            .Decl => {
                const decl = @fieldParentPtr(Node.Decl, "base", node);

                try self.renderToken(decl.let_const, writer, indent, .space);
                try self.renderNode(decl.capture, writer, indent, .space);
                try self.renderToken(decl.eq_tok, writer, indent, getBlockIndent(decl.value, .space));
                return self.renderNode(decl.value, writer, indent, space);
            },
            .Import => {
                const import = @fieldParentPtr(Node.Import, "base", node);

                try self.renderToken(import.tok, writer, indent, .none);
                try self.renderToken(self.nextToken(import.tok), writer, indent, .none);
                try self.renderToken(import.str_tok, writer, indent, .none);
                return self.renderToken(import.r_paren, writer, indent, space);
            },
            .Error => {
                const err = @fieldParentPtr(Node.Error, "base", node);

                const after_tok_space = if (err.capture == null) space else .none;
                try self.renderToken(err.tok, writer, indent, after_tok_space);
                if (err.capture) |some| try self.renderNode(some, writer, indent, space);
            },
            .Tagged => {
                const tag = @fieldParentPtr(Node.Tagged, "base", node);

                try self.renderToken(tag.at, writer, indent, .none);
                const after_tok_space = if (tag.capture == null) space else .none;
                try self.renderToken(tag.name, writer, indent, after_tok_space);
                if (tag.capture) |some| try self.renderNode(some, writer, indent, space);
            },
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);

                switch (jump.op) {
                    .Return => |expr| {
                        if (expr) |some| {
                            try self.renderToken(jump.tok, writer, indent, getBlockIndent(some, .space));
                            return self.renderNode(some, writer, indent, space);
                        } else {
                            try self.renderToken(jump.tok, writer, indent, space);
                        }
                    },
                    .Continue, .Break => try self.renderToken(jump.tok, writer, indent, space),
                }
            },
            .While => {
                const while_expr = @fieldParentPtr(Node.While, "base", node);

                try self.renderToken(while_expr.while_tok, writer, indent, .space);
                try self.renderToken(self.nextToken(while_expr.while_tok), writer, indent, .none);
                if (while_expr.capture) |some| {
                    try self.renderToken(while_expr.let_const.?, writer, indent, .space);
                    try self.renderNode(some, writer, indent, .space);
                    try self.renderToken(while_expr.eq_tok.?, writer, indent, .space);
                }
                try self.renderNode(while_expr.cond, writer, indent, .none);
                try self.renderToken(while_expr.r_paren, writer, indent, getBlockIndent(while_expr.body, .space));
                return self.renderNode(while_expr.body, writer, indent, space);
            },
            .For => {
                const for_expr = @fieldParentPtr(Node.For, "base", node);

                try self.renderToken(for_expr.for_tok, writer, indent, .space);
                try self.renderToken(self.nextToken(for_expr.for_tok), writer, indent, .none);
                if (for_expr.capture) |some| {
                    try self.renderToken(for_expr.let_const.?, writer, indent, .space);
                    try self.renderNode(some, writer, indent, .space);
                    try self.renderToken(for_expr.in_tok.?, writer, indent, .space);
                }
                try self.renderNode(for_expr.cond, writer, indent, .none);

                try self.renderToken(for_expr.r_paren, writer, indent, getBlockIndent(for_expr.body, .space));
                return self.renderNode(for_expr.body, writer, indent, space);
            },
            .Fn => {
                const fn_expr = @fieldParentPtr(Node.Fn, "base", node);

                try self.renderToken(fn_expr.fn_tok, writer, indent, .none);
                try self.renderToken(self.nextToken(fn_expr.fn_tok), writer, indent, .none);

                try self.renderCommaList(fn_expr.params, fn_expr.r_paren, writer, indent, .none);

                try self.renderToken(fn_expr.r_paren, writer, indent, getBlockIndent(fn_expr.body, .space));
                return self.renderNode(fn_expr.body, writer, indent, space);
            },
            .List, .Tuple, .Map => {
                const ltm = @fieldParentPtr(Node.ListTupleMap, "base", node);

                try self.renderToken(ltm.l_tok, writer, indent, .none);
                try self.renderCommaList(ltm.values, ltm.r_tok, writer, indent, .none);

                return self.renderToken(ltm.r_tok, writer, indent, space);
            },
            .Block => {
                const blk = @fieldParentPtr(Node.Block, "base", node);

                const new_indent = indent + indent_delta;
                for (blk.stmts) |stmt, i| {
                    try writer.writeByteNTimes(' ', new_indent);
                    try self.renderNode(stmt, writer, new_indent, .newline);

                    if (isBlock(stmt)) {
                        // render extra newline after blocks
                        try writer.writeAll("\n\n");
                        continue;
                    }

                    if (i + 1 == blk.stmts.len) break;
                    const last_token = stmt.lastToken();
                    if (self.lineDist(last_token, self.nextToken(last_token)) > 1) {
                        try writer.writeByte('\n');
                    }
                }
            },
            .MapItem => {
                const item = @fieldParentPtr(Node.MapItem, "base", node);

                if (item.key) |some| {
                    try self.renderNode(some, writer, indent, .none);
                    try self.renderToken(item.colon.?, writer, indent, .space);
                }

                return self.renderNode(item.value, writer, indent, space);
            },
            .Catch => {
                const catch_expr = @fieldParentPtr(Node.Catch, "base", node);

                try self.renderNode(catch_expr.lhs, writer, indent, .space);
                try self.renderToken(catch_expr.tok, writer, indent, .space);

                if (catch_expr.capture) |some| {
                    try self.renderToken(self.nextToken(catch_expr.tok), writer, indent, .none);
                    try self.renderToken(catch_expr.let_const.?, writer, indent, .space);
                    try self.renderNode(some, writer, indent, .none);

                    try self.renderToken(self.nextToken(some.lastToken()), writer, indent, getBlockIndent(catch_expr.rhs, .space));
                }
                return self.renderNode(catch_expr.rhs, writer, indent, space);
            },
            .If => {
                const if_expr = @fieldParentPtr(Node.If, "base", node);

                try self.renderToken(if_expr.if_tok, writer, indent, .space);
                try self.renderToken(self.nextToken(if_expr.if_tok), writer, indent, .none);
                if (if_expr.capture) |some| {
                    try self.renderToken(if_expr.let_const.?, writer, indent, .space);
                    try self.renderNode(some, writer, indent, .space);
                    try self.renderToken(if_expr.eq_tok.?, writer, indent, .space);
                }

                try self.renderNode(if_expr.cond, writer, indent, .none);
                try self.renderToken(if_expr.r_paren, writer, indent, getBlockIndent(if_expr.if_body, .space));

                if (if_expr.else_body) |some| {
                    try self.renderNode(if_expr.if_body, writer, indent, .space);
                    switch (if_expr.if_body.id) {
                        .Block, .Match => try writer.writeByteNTimes(' ', indent),
                        else => {},
                    }

                    try self.renderToken(if_expr.else_tok.?, writer, indent, getBlockIndent(some, .space));
                    return self.renderNode(some, writer, indent, space);
                } else {
                    return self.renderNode(if_expr.if_body, writer, indent, space);
                }
            },
            .Match => {
                const match_expr = @fieldParentPtr(Node.Match, "base", node);

                try self.renderToken(match_expr.match_tok, writer, indent, .space);
                try self.renderToken(self.nextToken(match_expr.match_tok), writer, indent, .none);
                try self.renderNode(match_expr.expr, writer, indent, .none);
                try self.renderToken(match_expr.r_paren, writer, indent, .newline);

                const new_indent = indent + indent_delta;
                for (match_expr.cases) |case| {
                    try writer.writeByteNTimes(' ', new_indent);
                    try self.renderNode(case, writer, new_indent, .newline);
                }
            },
            .MatchCatchAll => {
                const case = @fieldParentPtr(Node.MatchCatchAll, "base", node);

                if (self.tokens[case.tok].id != .Underscore) {
                    try self.renderToken(case.tok, writer, indent, .space);
                    try self.renderToken(self.nextToken(case.tok), writer, indent, .space);
                } else {
                    try self.renderToken(case.tok, writer, indent, .space);
                }

                try self.renderToken(case.eq_arr, writer, indent, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, writer, indent, space);
            },
            .MatchLet => {
                const case = @fieldParentPtr(Node.MatchLet, "base", node);

                try self.renderToken(case.let_const, writer, indent, .space);
                try self.renderNode(case.capture, writer, indent, .space);

                try self.renderToken(case.eq_arr, writer, indent, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, writer, indent, space);
            },
            .MatchCase => {
                const case = @fieldParentPtr(Node.MatchCase, "base", node);

                try self.renderCommaList(case.lhs, case.eq_arr, writer, indent, .space);

                try self.renderToken(case.eq_arr, writer, indent, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, writer, indent, space);
            },
        }
    }

    fn renderCommaList(self: *Renderer, nodes: []const *Node, last_token: TokenIndex, writer: anytype, indent: u32, space: Space) !void {
        const prev = self.tokens[last_token - 1].id;
        if (prev == .Comma or prev == .Nl or prev == .Comment) {
            try writer.writeByte('\n');
            const new_indent = indent + indent_delta;
            for (nodes) |node, i| {
                try writer.writeByteNTimes(' ', new_indent);
                try self.renderNode(node, writer, new_indent, .none);
                const comma = self.nextToken(node.lastToken());
                if (self.tokens[comma].id == .Comma)
                    try self.renderToken(comma, writer, indent, .newline)
                else
                    try writer.writeAll(",\n");
            }
            try writer.writeByteNTimes(' ', indent);
        } else {
            for (nodes) |node, i| {
                if (i + 1 == nodes.len) {
                    try self.renderNode(node, writer, indent, space);
                    break;
                }
                try self.renderNode(node, writer, indent, .none);
                try self.renderToken(self.nextToken(node.lastToken()), writer, indent, .space);
            }
        }
    }

    fn renderComments(self: *Renderer, token: TokenIndex, writer: anytype, indent: u32, space: Space) !void {
        var i = token;
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

            try writer.writeAll(trimmed);
            try writer.writeByte('\n');
            try writer.writeByteNTimes(' ', indent);

            if (space != .newline)
                try writer.writeByteNTimes(' ', indent_delta);
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
            .Catch => {
                const catch_node = @fieldParentPtr(Node.Catch, "base", node);
                return isBlock(catch_node.rhs);
            },
            else => return false,
        }
    }

    const Space = enum {
        none,
        newline,
        space,
    };

    fn renderToken(self: *Renderer, token: TokenIndex, writer: anytype, indent: u32, space: Space) !void {
        var tok = self.tokens[token];
        try writer.writeAll(self.source[tok.start..tok.end]);
        switch (space) {
            .none => {},
            .newline => try writer.writeByte('\n'),
            .space => try writer.writeByte(' '),
        }
        try self.renderComments(token + 1, writer, indent, space);
    }
};
