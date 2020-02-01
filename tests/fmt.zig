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
        \\while true { break }
        \\return 123 // 4
        \\for let foo in arr { foo + 2 }
        \\for 1 ... 3 { continue }
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

var buffer: [10 * 1024]u8 = undefined;

fn fmt(source: []const u8) ![]u8 {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var tree = try lang.parse(alloc, source);

    var out_buf = try std.Buffer.initSize(alloc, 0);
    var out_stream = std.io.BufferOutStream.init(&out_buf);
    try tree.render(&out_stream.stream);
    return out_buf.toOwnedSlice();
}

fn testTransform(source: []const u8, expected: []const u8) !void {
    const result = try fmt(source);
    if (!mem.eql(u8, result, expected)) {
        warn("\n---expected----\n{}\n-----found-----\n{}\n---------------\n", .{ expected, result });
        return error.TestFailed;
    }
}

fn testCanonical(source: []const u8) !void {
    return testTransform(source, source);
}
