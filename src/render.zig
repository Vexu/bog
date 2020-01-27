const std = @import("std");
const ast = @import("ast.zig");
const Node = ast.Node;
const NodeList = ast.NodeList;
const tokenizer = @import("tokenizer.zig");
const TokenIndex = tokenizer.TokenIndex;
const TokenList = tokenizer.TokenList;

pub fn render(source: []const u8, tokens: TokenList, nodes: *NodeList, stream: var) @TypeOf(stream).Child.Error!void {
    var it = nodes.iterator(0);
    var renderer = Renderer{
        .source = source,
        .tokens = tokens,
    };
    while (it.next()) |node| {
        try renderer.renderNode(node.*, stream, 0, .Newline);
    }
}

pub const Renderer = struct {
    source: []const u8,
    tokens: TokenList,

    fn renderNode(self: *Renderer, node: *Node, stream: var, indent: u32, space: Space) @TypeOf(stream).Child.Error!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);

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
                        // TODO trailing comma
                        var it = params.iterator(0);
                        while (it.next()) |param| {
                            try self.renderNode(param.*, stream, indent, .Space);
                        }
                    },
                    .ArrAccess => |arr_node| try self.renderNode(arr_node, stream, indent, .None),
                    .Member => {},
                }
                return self.renderToken(suffix.r_tok, stream, indent, space);
            },
            .ListTupleCallItem => {
                const item = @fieldParentPtr(Node.ListTupleCallItem, "base", node);

                try self.renderNode(item.value, stream, indent, .None);
                if (item.comma) |some| {
                    try self.renderToken(some, stream, indent, space);
                }
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
            .Fn,
            .Unwrap,
            .List,
            .Tuple,
            .Map,
            .Block,
            .MapItem,
            .Catch,
            .If,
            .For,
            .While,
            .Match,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            .Jump,
            => @panic("TODO"),
        }
    }

    const Space = enum {
        None,
        Newline,
        Space,
    };

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
