const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    var stdin_unbuf = std.io.getStdIn().inStream();
    const in = &std.io.BufferedInStream(@TypeOf(stdin_unbuf).Error).init(&stdin_unbuf.stream).stream;
    var stdout = std.io.getStdOut().outStream();
    const alloc = std.heap.page_allocator;

    try repl.run(alloc, in, &stdout.stream);
}

comptime {
    _ = @import("parser.zig");
    _ = @import("tokenizer.zig");
    _ = @import("value.zig");
}
