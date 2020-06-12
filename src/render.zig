const std = @import("std");
const bog = @import("bog.zig");
const Tree = bog.Tree;
const Node = bog.Node;
const TokenIndex = bog.Token.Index;

pub fn render(tree: *Tree, stream: var) @TypeOf(stream).Error!void {
    var renderer = Renderer{
        .source = tree.source,
        .tokens = tree.tokens,
    };
    try renderer.renderComments(0, stream, 0, .newline);
    for (tree.nodes) |node, i| {
        try renderer.renderNode(node, stream, 0, .newline);
        if (Renderer.isBlock(node)) {
            // render extra newline after blocks
            try stream.writeAll("\n\n");
            continue;
        }

        if (i + 1 == tree.nodes.len) break;
        const last_token = node.lastToken();
        if (renderer.lineDist(last_token, renderer.nextToken(last_token)) > 1) {
            try stream.writeByte('\n');
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

    fn renderNode(self: *Renderer, node: *Node, stream: var, indent: u32, space: Space) @TypeOf(stream).Error!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                if (literal.kind == .none) {
                    try self.renderToken(self.prevToken(literal.tok), stream, indent, .none);
                }

                return self.renderToken(literal.tok, stream, indent, space);
            },
            .Infix => {
                const infix = @fieldParentPtr(Node.Infix, "base", node);

                const after_tok_space: Space = if (infix.op == .Range) .none else .space;

                try self.renderNode(infix.lhs, stream, indent, after_tok_space);
                try self.renderToken(infix.tok, stream, indent, after_tok_space);
                return self.renderNode(infix.rhs, stream, indent, space);
            },
            .Prefix => {
                const prefix = @fieldParentPtr(Node.Prefix, "base", node);

                switch (prefix.op) {
                    .boolNot, .Try => try self.renderToken(prefix.tok, stream, indent, .space),
                    .bitNot, .minus, .plus => try self.renderToken(prefix.tok, stream, indent, .none),
                }
                return self.renderNode(prefix.rhs, stream, indent, space);
            },
            .Grouped => {
                const grouped = @fieldParentPtr(Node.Grouped, "base", node);

                try self.renderToken(grouped.l_tok, stream, indent, getBlockIndent(grouped.expr, .none));
                try self.renderNode(grouped.expr, stream, indent, .none);
                return self.renderToken(grouped.r_tok, stream, indent, space);
            },
            .TypeInfix => {
                const type_infix = @fieldParentPtr(Node.TypeInfix, "base", node);

                try self.renderNode(type_infix.lhs, stream, indent, .space);
                try self.renderToken(type_infix.tok, stream, indent, .space);
                return self.renderToken(type_infix.type_tok, stream, indent, space);
            },
            .Discard, .Identifier, .This => {
                const single = @fieldParentPtr(Node.SingleToken, "base", node);

                return self.renderToken(single.tok, stream, indent, space);
            },
            .Suffix => {
                const suffix = @fieldParentPtr(Node.Suffix, "base", node);

                try self.renderNode(suffix.lhs, stream, indent, .none);
                try self.renderToken(suffix.l_tok, stream, indent, .none);
                switch (suffix.op) {
                    .call => |params| try self.renderCommaList(params, suffix.r_tok, stream, indent, space),
                    .subscript => |arr_node| try self.renderNode(arr_node, stream, indent, .none),
                    .member => {},
                }
                return self.renderToken(suffix.r_tok, stream, indent, space);
            },
            .Decl => {
                const decl = @fieldParentPtr(Node.Decl, "base", node);

                try self.renderToken(decl.let_const, stream, indent, .space);
                try self.renderNode(decl.capture, stream, indent, .space);
                try self.renderToken(decl.eq_tok, stream, indent, getBlockIndent(decl.value, .space));
                return self.renderNode(decl.value, stream, indent, space);
            },
            .Import => {
                const import = @fieldParentPtr(Node.Import, "base", node);

                try self.renderToken(import.tok, stream, indent, .none);
                try self.renderToken(self.nextToken(import.tok), stream, indent, .none);
                try self.renderToken(import.str_tok, stream, indent, .none);
                return self.renderToken(import.r_paren, stream, indent, space);
            },
            .Native => {
                const native = @fieldParentPtr(Node.Native, "base", node);

                try self.renderToken(native.tok, stream, indent, .none);
                try self.renderToken(self.nextToken(native.tok), stream, indent, .none);
                try self.renderToken(native.str_tok, stream, indent, .none);
                return self.renderToken(native.r_paren, stream, indent, space);
            },
            .Error => {
                const err = @fieldParentPtr(Node.Error, "base", node);

                const after_tok_space = if (err.capture == null) space else .none;
                try self.renderToken(err.tok, stream, indent, after_tok_space);
                if (err.capture) |some| try self.renderNode(some, stream, indent, space);
            },
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);

                switch (jump.op) {
                    .Return => |expr| {
                        if (expr) |some| {
                            try self.renderToken(jump.tok, stream, indent, getBlockIndent(some, .space));
                            return self.renderNode(some, stream, indent, space);
                        } else {
                            try self.renderToken(jump.tok, stream, indent, space);
                        }
                    },
                    .Continue, .Break => try self.renderToken(jump.tok, stream, indent, space),
                }
            },
            .While => {
                const while_expr = @fieldParentPtr(Node.While, "base", node);

                try self.renderToken(while_expr.while_tok, stream, indent, .space);
                try self.renderToken(self.nextToken(while_expr.while_tok), stream, indent, .none);
                if (while_expr.capture) |some| {
                    try self.renderToken(while_expr.let_const.?, stream, indent, .space);
                    try self.renderNode(some, stream, indent, .space);
                    try self.renderToken(while_expr.eq_tok.?, stream, indent, .space);
                }
                try self.renderNode(while_expr.cond, stream, indent, .none);
                try self.renderToken(while_expr.r_paren, stream, indent, getBlockIndent(while_expr.body, .space));
                return self.renderNode(while_expr.body, stream, indent, space);
            },
            .For => {
                const for_expr = @fieldParentPtr(Node.For, "base", node);

                try self.renderToken(for_expr.for_tok, stream, indent, .space);
                try self.renderToken(self.nextToken(for_expr.for_tok), stream, indent, .none);
                if (for_expr.capture) |some| {
                    try self.renderToken(for_expr.let_const.?, stream, indent, .space);
                    try self.renderNode(some, stream, indent, .space);
                    try self.renderToken(for_expr.in_tok.?, stream, indent, .space);
                }
                try self.renderNode(for_expr.cond, stream, indent, .none);

                try self.renderToken(for_expr.r_paren, stream, indent, getBlockIndent(for_expr.body, .space));
                return self.renderNode(for_expr.body, stream, indent, space);
            },
            .Fn => {
                const fn_expr = @fieldParentPtr(Node.Fn, "base", node);

                try self.renderToken(fn_expr.fn_tok, stream, indent, .none);
                try self.renderToken(self.nextToken(fn_expr.fn_tok), stream, indent, .none);

                try self.renderCommaList(fn_expr.params, fn_expr.r_paren, stream, indent, space);

                try self.renderToken(fn_expr.r_paren, stream, indent, getBlockIndent(fn_expr.body, .space));
                return self.renderNode(fn_expr.body, stream, indent, space);
            },
            .List, .Tuple, .Map => {
                const ltm = @fieldParentPtr(Node.ListTupleMap, "base", node);

                try self.renderToken(ltm.l_tok, stream, indent, .none);
                try self.renderCommaList(ltm.values, ltm.r_tok, stream, indent, space);

                return self.renderToken(ltm.r_tok, stream, indent, space);
            },
            .Block => {
                const blk = @fieldParentPtr(Node.Block, "base", node);

                const new_indent = indent + indent_delta;
                for (blk.stmts) |stmt, i| {
                    try stream.writeByteNTimes(' ', new_indent);
                    try self.renderNode(stmt, stream, new_indent, .newline);

                    if (isBlock(stmt)) {
                        // render extra newline after blocks
                        try stream.writeAll("\n\n");
                        continue;
                    }

                    if (i + 1 == blk.stmts.len) break;
                    const last_token = stmt.lastToken();
                    if (self.lineDist(last_token, self.nextToken(last_token)) > 1) {
                        try stream.writeByte('\n');
                    }
                }
            },
            .MapItem => {
                const item = @fieldParentPtr(Node.MapItem, "base", node);

                if (item.key) |some| {
                    try self.renderNode(some, stream, indent, .none);
                    try self.renderToken(item.colon.?, stream, indent, .space);
                }

                return self.renderNode(item.value, stream, indent, space);
            },
            .Catch => {
                const catch_expr = @fieldParentPtr(Node.Catch, "base", node);

                try self.renderNode(catch_expr.lhs, stream, indent, .space);
                try self.renderToken(catch_expr.tok, stream, indent, .space);

                if (catch_expr.capture) |some| {
                    try self.renderToken(self.nextToken(catch_expr.tok), stream, indent, .none);
                    try self.renderToken(catch_expr.let_const.?, stream, indent, .space);
                    try self.renderNode(some, stream, indent, .none);

                    try self.renderToken(self.nextToken(some.lastToken()), stream, indent, getBlockIndent(catch_expr.rhs, .space));
                }
                return self.renderNode(catch_expr.rhs, stream, indent, space);
            },
            .If => {
                const if_expr = @fieldParentPtr(Node.If, "base", node);

                try self.renderToken(if_expr.if_tok, stream, indent, .space);
                try self.renderToken(self.nextToken(if_expr.if_tok), stream, indent, .none);
                if (if_expr.capture) |some| {
                    try self.renderToken(if_expr.let_const.?, stream, indent, .space);
                    try self.renderNode(some, stream, indent, .space);
                    try self.renderToken(if_expr.eq_tok.?, stream, indent, .space);
                }

                try self.renderNode(if_expr.cond, stream, indent, .none);
                try self.renderToken(if_expr.r_paren, stream, indent, getBlockIndent(if_expr.if_body, .space));

                if (if_expr.else_body) |some| {
                    try self.renderNode(if_expr.if_body, stream, indent, .space);
                    switch (if_expr.if_body.id) {
                        .Block, .Match => try stream.writeByteNTimes(' ', indent),
                        else => {},
                    }

                    try self.renderToken(if_expr.else_tok.?, stream, indent, getBlockIndent(some, .space));
                    return self.renderNode(some, stream, indent, space);
                } else {
                    return self.renderNode(if_expr.if_body, stream, indent, space);
                }
            },
            .Match => {
                const match_expr = @fieldParentPtr(Node.Match, "base", node);

                try self.renderToken(match_expr.match_tok, stream, indent, .space);
                try self.renderToken(self.nextToken(match_expr.match_tok), stream, indent, .none);
                try self.renderNode(match_expr.expr, stream, indent, .none);
                try self.renderToken(match_expr.r_paren, stream, indent, .newline);

                const new_indent = indent + indent_delta;
                for (match_expr.cases) |case| {
                    try stream.writeByteNTimes(' ', new_indent);
                    try self.renderNode(case, stream, new_indent, .newline);
                }
            },
            .MatchCatchAll => {
                const case = @fieldParentPtr(Node.MatchCatchAll, "base", node);

                if (self.tokens[case.tok].id != .Underscore) {
                    try self.renderToken(case.tok, stream, indent, .space);
                    try self.renderToken(self.nextToken(case.tok), stream, indent, .none);
                } else {
                    try self.renderToken(case.tok, stream, indent, .none);
                }

                try self.renderToken(case.colon, stream, indent, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, stream, indent, space);
            },
            .MatchLet => {
                const case = @fieldParentPtr(Node.MatchLet, "base", node);

                try self.renderToken(case.let_const, stream, indent, .space);
                try self.renderNode(case.capture, stream, indent, .none);

                try self.renderToken(case.colon, stream, indent, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, stream, indent, space);
            },
            .MatchCase => {
                const case = @fieldParentPtr(Node.MatchCase, "base", node);

                try self.renderCommaList(case.lhs, case.colon, stream, indent, space);

                try self.renderToken(case.colon, stream, indent, getBlockIndent(case.expr, .space));
                return self.renderNode(case.expr, stream, indent, space);
            },
        }
    }

    fn renderCommaList(self: *Renderer, nodes: []const *Node, last_token: TokenIndex, stream: var, indent: u32, space: Space) !void {
        const prev = self.tokens[last_token - 1].id;
        if (prev == .Comma or prev == .Nl or prev == .Comment) {
            try stream.writeByte('\n');
            const new_indent = indent + indent_delta;
            for (nodes) |node, i| {
                try stream.writeByteNTimes(' ', new_indent);
                try self.renderNode(node, stream, new_indent, .none);
                const comma = self.nextToken(node.lastToken());
                if (self.tokens[comma].id == .Comma)
                    try self.renderToken(comma, stream, indent, .newline)
                else
                    try stream.writeAll(",\n");
            }
            try stream.writeByteNTimes(' ', indent);
        } else {
            for (nodes) |node, i| {
                if (i + 1 == nodes.len) {
                    try self.renderNode(node, stream, indent, .none);
                    break;
                }
                try self.renderNode(node, stream, indent, .none);
                try self.renderToken(self.nextToken(node.lastToken()), stream, indent, .space);
            }
        }
    }

    fn renderComments(self: *Renderer, token: TokenIndex, stream: var, indent: u32, space: Space) !void {
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

            try stream.writeAll(trimmed);
            try stream.writeByte('\n');
            try stream.writeByteNTimes(' ', indent);

            if (space != .newline)
                try stream.writeByteNTimes(' ', indent_delta);
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

    fn renderToken(self: *Renderer, token: TokenIndex, stream: var, indent: u32, space: Space) !void {
        var tok = self.tokens[token];
        try stream.writeAll(self.source[tok.start..tok.end]);
        switch (space) {
            .none => {},
            .newline => try stream.writeByte('\n'),
            .space => try stream.writeByte(' '),
        }
        try self.renderComments(token + 1, stream, indent, space);
    }
};
