const std = @import("std");
const Repl = @import("repl.zig").Repl;

pub fn main() !void {
    var stdin_unbuf = std.io.getStdIn().inStream();
    const in = &std.io.BufferedInStream(@TypeOf(stdin_unbuf).Error).init(&stdin_unbuf.stream).stream;
    var stdout = std.io.getStdOut().outStream();

    var repl = Repl.init();
    try repl.run(in, &stdout.stream);
}

comptime {
    _ = @import("parser.zig");
    _ = @import("tokenizer.zig");
    _ = @import("value.zig");
}
