test "basic math" {
    try expectOutput(
        \\let x = 2
        \\let y = x * 5
        \\let z = 90
        \\y // x * z
    ,
        \\450
    );
}

test "basic variables" {
    try expectOutput(
        \\let x = 12
        \\x
    ,
        \\12
    );
    try expectOutput(
        \\let x = true
        \\not x
    ,
        \\false
    );
}

test "number literals" {
    try expectOutput(
        \\12
    ,
        \\12
    );
    try expectOutput(
        \\0x12
    ,
        \\18
    );
    try expectOutput(
        \\0o12
    ,
        \\10
    );
}

test "constant values" {
    try expectOutput(
        \\true
    ,
        \\true
    );
    try expectOutput(
        \\not true
    ,
        \\false
    );
    try expectOutput(
        \\-12
    ,
        \\-12
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const testing = std.testing;
const lang = @import("lang");
const Vm = lang.Vm;

var buffer: [10 * 1024]u8 = undefined;

fn expectOutput(source: []const u8, expected: []const u8) !void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var vm = Vm.init(alloc, true);

    // TODO move this
    // TODO move this
    try vm.gc.stackAlloc(250);

    var tree = try lang.parse(alloc, source);
    var module = try tree.compile(alloc);

    try vm.exec(&module);
    if (vm.result) |some| {
        var out_buf = try std.Buffer.initSize(alloc, 0);
        var out_stream = std.io.BufferOutStream.init(&out_buf);
        try some.dump(&out_stream.stream, 2);
        const result = out_buf.toSliceConst();
        if (!mem.eql(u8, result, expected)) {
            warn("\n---expected----\n{}\n-----found-----\n{}\n---------------\n", .{ expected, result });
            return error.TestFailed;
        }
    } else {
        return error.TestFailed;
    }
}
