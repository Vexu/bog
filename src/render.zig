const std = @import("std");
const ast = @import("ast.zig");
const Node = ast.Node;
const NodeList = ast.NodeList;
const TokenList = @import("tokenizer.zig").TokenList;

pub fn render(source: []const u8, tokens: TokenList, nodes: *NodeList, stream: var) @TypeOf(stream).Child.Error!void {
    var it = nodes.iterator(0);
    while (it.next()) |node| {
        try renderNode(source, tokens, node.*, stream, 0);
    }
}

pub fn renderNode(source: []const u8, tokens: TokenList, node: *Node, stream: var, indent: u32) @TypeOf(stream).Child.Error!void {
    switch (node.id) {
        .Literal => {
            const literal = @fieldParentPtr(Node.Literal, "base", node);
            const tok = tokens.at(literal.tok);
            try stream.write(source[tok.start..tok.end]);
        },
        else => @panic("TODO"),
    }
}
