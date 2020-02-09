const std = @import("std");
const process = std.process;
const mem = std.mem;
const repl = @import("repl.zig");
const lang = @import("lang.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    const args = try process.argsAlloc(alloc);
    defer process.argsFree(alloc, args);
    if (args.len > 1) {
        if (mem.eql(u8, args[1], "fmt")) {
            return fmt(alloc, args[2..]);
        } else if (mem.eql(u8, args[1], "help") or mem.eql(u8, args[1], "--help")) {
            return help();
        } else if (!mem.startsWith(u8, "-", args[1])) {
            return run(alloc, args[1..]);
        }
    }

    var stdin_unbuf = std.io.getStdIn().inStream();
    const in = &std.io.BufferedInStream(@TypeOf(stdin_unbuf).Error).init(&stdin_unbuf.stream).stream;
    var stdout = std.io.getStdOut().outStream();

    try repl.run(alloc, in, &stdout.stream);
}

const usage =
    \\usage: lang [command] [options] [-- [args]]
    \\
    \\Commands:
    \\
    \\  fmt        [source]      Parse file and render it
    \\  run        [source]      Run file
    \\
    \\
;

fn help() !void {
    const stdout = &std.io.getStdOut().outStream().stream;
    try stdout.print(usage, .{});
    process.exit(0);
}

fn run(alloc: *std.mem.Allocator, args: [][]const u8) void {
    @panic("TODO: run");
}

const usage_fmt =
    \\usage: lang fmt [file]...
    \\
    \\   Formats the input files.
    \\
    \\
;

fn fmt(alloc: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) {
        const stdout = &std.io.getStdOut().outStream().stream;
        try stdout.print(usage_fmt, .{});
        process.exit(0);
    }
    @panic("TODO: fmt");

    // var tree = try lang.parse(alloc, source);

    // var out_buf = try std.Buffer.initSize(alloc, 0);
    // var out_stream = std.io.BufferOutStream.init(&out_buf);
    // try tree.render(&out_stream.stream);
    // return out_buf.toOwnedSlice();
}

comptime {
    _ = @import("tokenizer.zig");
    _ = @import("value.zig");
}
