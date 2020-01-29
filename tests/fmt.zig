test "unicode identifiers" {
    try testTransform(
        \\öäöäö;
    ,
        \\öäöäö
        \\
    );
}

test "trailing comma in call" {
    try testCanonical(
        \\foo(2, 3)
        \\bar(
        \\    2,
        \\    3,
        \\)
        \\
    );
    try testTransform(
        \\foo(2,3,)
        \\bar(
        \\    2,
        \\    3
        \\)
        \\
    ,
        \\foo(
        \\    2,
        \\    3,
        \\)
        \\bar(
        \\    2,
        \\    3,
        \\)
        \\
    );
}

test "loops" {
    try testCanonical(
        \\while (true) break
        \\return (123 // 4)
        \\for (let foo in arr) foo + 2
        \\for (1 ... 3) continue
        \\
    );
}

test "declarations" {
    try testCanonical(
        \\let bar = import("args")
        \\let foo = bar + 2
        \\let err = error(foo)
        \\
    );
}

test "suffix ops" {
    try testCanonical(
        \\foo[2].bar(2).baz[5 + 5]
        \\
    );
}

test "prefix ops" {
    try testCanonical(
        \\not true
        \\-2
        \\
    );
}

test "infix ops" {
    try testCanonical(
        \\123 + 2 * 3 / (4 as num) + ()
        \\
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const lang = @import("lang");
const Tree = lang.Tree;
const Tokenizer = lang.Tokenizer;
const parser = lang.parser;
const render = lang.render;

var buffer: [10 * 1024]u8 = undefined;

fn fmt(source: []const u8) ![]u8 {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var tree = Tree.init(alloc);
    _ = try Tokenizer.init(&tree, false).tokenize(source);
    var arena = std.heap.ArenaAllocator.init(alloc);
    try parser.parse(&arena, &tree);

    var out_buf = try std.Buffer.initSize(alloc, 0);
    var out_stream = std.io.BufferOutStream.init(&out_buf);
    try render.render(source, &tree, &out_stream.stream);
    return out_buf.toOwnedSlice();
}

fn testTransform(source: []const u8, expected: []const u8) !void {
    const result = try fmt(source);
    if (!mem.eql(u8, result, expected)) {
        warn("\nexpected:\n{}\nfound:\n{}\n", .{ expected, result });
        return error.TestFailed;
    }
}

fn testCanonical(source: []const u8) !void {
    return testTransform(source, source);
}
