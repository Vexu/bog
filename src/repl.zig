const std = @import("std");
const Parser = @import("parser.zig").Parser;

pub const Repl = struct {
    buf: [512]u8,
    parser: Parser,

    pub fn init() Repl {
        return .{
            .buf = undefined,
            .parser = Parser.init(),
        };
    }

    pub fn run(repl: *Repl, in_stream: var, out_stream: var) !void {
        while (true) {
            try out_stream.write("<<< ");
            const input = std.io.readLineSliceFrom(in_stream, &repl.buf) catch return;
            try repl.parser.parse(input);
        }
    }
};
