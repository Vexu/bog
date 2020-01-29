const std = @import("std");
const ast = @import("ast.zig");
const Tree = ast.Tree;
const Node = ast.Node;
const NodeList = ast.NodeList;
const TokenList = ast.TokenList;
const TokenIndex = ast.TokenIndex;

pub fn render(source: []const u8, tree: *Tree, stream: var) @TypeOf(stream).Child.Error!void {
    var it = tree.nodes.iterator(0);
    var renderer = Renderer{
        .source = source,
        .tokens = &tree.tokens,
    };
    while (it.next()) |node| {
        try renderer.renderNode(node.*, stream, 0, .Newline);
    }
}

pub const Renderer = struct {
    source: []const u8,
    tokens: *TokenList,

    const indent_delta = 4;

    fn renderNode(self: *Renderer, node: *Node, stream: var, indent: u32, space: Space) @TypeOf(stream).Child.Error!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                if (literal.kind == .None) {
                    try self.renderToken(literal.tok - 1, stream, indent, .None);
                }

                return self.renderToken(literal.tok, stream, indent, space);
            },
            .Infix => {
                const infix = @fieldParentPtr(Node.Infix, "base", node);

                try self.renderNode(infix.lhs, stream, indent, .Space);
                try self.renderToken(infix.tok, stream, indent, .Space);
                return self.renderNode(infix.rhs, stream, indent, space);
            },
            .Prefix => {
                const prefix = @fieldParentPtr(Node.Prefix, "base", node);

                switch (prefix.op) {
                    .BoolNot, .Try => try self.renderToken(prefix.tok, stream, indent, .Space),
                    .BitNot, .Minus, .Plus => try self.renderToken(prefix.tok, stream, indent, .None),
                }
                return self.renderNode(prefix.rhs, stream, indent, space);
            },
            .Grouped => {
                const grouped = @fieldParentPtr(Node.Grouped, "base", node);

                try self.renderToken(grouped.l_tok, stream, indent, .None);
                try self.renderNode(grouped.expr, stream, indent, .None);
                return self.renderToken(grouped.r_tok, stream, indent, space);
            },
            .TypeInfix => {
                const type_infix = @fieldParentPtr(Node.TypeInfix, "base", node);

                try self.renderNode(type_infix.lhs, stream, indent, .Space);
                try self.renderToken(type_infix.tok - 1, stream, indent, .Space);
                return self.renderToken(type_infix.tok, stream, indent, space);
            },
            .Discard, .Identifier => {
                const single = @fieldParentPtr(Node.SingleToken, "base", node);

                return self.renderToken(single.tok, stream, indent, space);
            },
            .Suffix => {
                const suffix = @fieldParentPtr(Node.Suffix, "base", node);

                try self.renderNode(suffix.lhs, stream, indent, .None);
                try self.renderToken(suffix.l_tok, stream, indent, .None);
                switch (suffix.op) {
                    .Call => |*params| {
                        var it = params.iterator(0);
                        const prev = self.tokens.at(suffix.r_tok - 1).id;
                        if (prev == .Comma or prev == .Nl) {
                            try stream.writeByte('\n');
                            const new_indet = indent + indent_delta;
                            while (it.next()) |param| {
                                try stream.writeByteNTimes(' ', new_indet);
                                try self.renderNode(param.*, stream, new_indet, .None);
                                try stream.write(",\n");
                            }
                        } else {
                            while (it.next()) |param| {
                                if (it.peek() == null) {
                                    try self.renderNode(param.*, stream, indent, .None);
                                    break;
                                }
                                try self.renderNode(param.*, stream, indent, .None);
                                try stream.write(", ");
                            }
                        }
                    },
                    .ArrAccess => |arr_node| try self.renderNode(arr_node, stream, indent, .None),
                    .Member => {},
                }
                return self.renderToken(suffix.r_tok, stream, indent, space);
            },
            .Let => {
                const let = @fieldParentPtr(Node.Let, "base", node);

                try self.renderToken(let.let_tok, stream, indent, .Space);
                try self.renderNode(let.unwrap, stream, indent, .Space);
                try self.renderToken(let.eq_tok, stream, indent, .Space);
                return self.renderNode(let.body, stream, indent, space);
            },
            .Import => {
                const import = @fieldParentPtr(Node.Import, "base", node);

                try self.renderToken(import.tok, stream, indent, .None);
                try self.renderToken(import.tok + 1, stream, indent, .None);
                try self.renderToken(import.str_tok, stream, indent, .None);
                return self.renderToken(import.str_tok + 1, stream, indent, space);
            },
            .Error => {
                const err = @fieldParentPtr(Node.Error, "base", node);

                try self.renderToken(err.tok, stream, indent, .None);
                try self.renderToken(err.tok + 1, stream, indent, .None);
                try self.renderNode(err.value, stream, indent, .None);
                return self.renderToken(err.r_paren, stream, indent, space);
            },
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);

                switch (jump.op) {
                    .Return, .Break => |expr| {
                        const after_tok_space = if (expr != null) .Space else Space.Newline;
                        try self.renderToken(jump.tok, stream, indent, after_tok_space);
                        if (expr) |some| try self.renderNode(some, stream, indent, space);
                    },
                    .Continue => try self.renderToken(jump.tok, stream, indent, space),
                }
            },
            .While => {
                const while_expr = @fieldParentPtr(Node.While, "base", node);

                try self.renderToken(while_expr.while_tok, stream, indent, .Space);
                try self.renderToken(while_expr.while_tok + 1, stream, indent, .None);
                try self.renderNode(while_expr.cond, stream, indent, .None);
                try self.renderToken(while_expr.r_paren, stream, indent, .Space);
                return self.renderNode(while_expr.body, stream, indent, space);
            },
            .For => {
                const for_expr = @fieldParentPtr(Node.For, "base", node);

                try self.renderToken(for_expr.for_tok, stream, indent, .Space);
                try self.renderToken(for_expr.for_tok + 1, stream, indent, .None);
                if (for_expr.unwrap) |some| {
                    try self.renderToken(for_expr.for_tok + 2, stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(for_expr.in_tok.?, stream, indent, .Space);
                }
                try self.renderNode(for_expr.cond, stream, indent, .None);
                try self.renderToken(for_expr.r_paren, stream, indent, .Space);
                return self.renderNode(for_expr.body, stream, indent, space);
            },
            .Fn,
            .Unwrap,
            .List,
            .Tuple,
            .Map,
            .Block,
            .MapItem,
            .Catch,
            .If,
            .Match,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            => @panic("TODO"),
        }
    }

    const Space = enum {
        None,
        Newline,
        Space,
    };

    // TODO keep comments
    fn renderToken(self: *Renderer, token: TokenIndex, stream: var, indent: u32, space: Space) !void {
        const tok = self.tokens.at(token);
        try stream.write(self.source[tok.start..tok.end]);
        switch (space) {
            .None => {},
            .Newline => try stream.writeByte('\n'),
            .Space => try stream.writeByte(' '),
        }
    }
};
