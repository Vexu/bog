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
                try self.renderNode(infix.rhs, stream, indent, space);
            },
            .Let,
            .Fn,
            .Unwrap,
            .Discard,
            .Identifier,
            .Prefix,
            .TypeInfix,
            .Suffix,
            .Import,
            .Error,
            .List,
            .Tuple,
            .Map,
            .Block,
            .Grouped,
            .ListTupleCallItem,
            .MapItem,
            .Catch,
            .If,
            .For,
            .While,
            .Match,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            .Jump, => @panic("TODO"),
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
