const std = @import("std");
const process = std.process;
const mem = std.mem;
const repl = @import("repl.zig");
const bog = @import("bog.zig");

const is_debug = @import("builtin").mode == .Debug;

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    const args = try process.argsAlloc(alloc);
    defer process.argsFree(alloc, args);
    if (args.len > 1) {
        if (mem.eql(u8, args[1], "fmt")) {
            return fmt(alloc, args[2..]);
        }
        if (mem.eql(u8, args[1], "help") or mem.eql(u8, args[1], "--help")) {
            return help();
        }
        if (is_debug) {
            if (mem.eql(u8, args[1], "debug:dump")) {
                return debug_dump(alloc, args[2..]);
            }
        }
        if (!mem.startsWith(u8, "-", args[1])) {
            return run(alloc, args[1..]);
        }
    }

    var stdin_unbuf = std.io.getStdIn().inStream();
    const in = &std.io.BufferedInStream(@TypeOf(stdin_unbuf).Error).init(&stdin_unbuf.stream).stream;
    var stdout = std.io.getStdOut().outStream();

    try repl.run(alloc, in, &stdout.stream);
}

const usage =
    \\usage: bog [command] [options] [-- [args]]
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
    \\usage: bog fmt [file]...
    \\
    \\   Formats the input files.
    \\
    \\
;

fn fmt(alloc: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) {
        print_and_exit(usage_fmt);
    }
    // TODO handle dirs
    const source = try std.fs.cwd().readFileAlloc(alloc, args[0], 1024 * 1024);

    // var tree = bog.parse(alloc, source) catch |e| switch (e) {
    //     error.TokenizeError, error.ParseError => try bog.Error.render(tree.errors, repl.buffer.toSliceConst(), out_stream),
    //     error.OutOfMemory => return error.OutOfMemory,
    // };

    // const file = try std.fs.cwd().openFile(args[0], .{ .write = true, .read = false });

    // var out_stream = &file.outStream().stream;
    // try tree.render(out_stream);

    // file.close();
    // process.exit(0);
}

fn print_and_exit(msg: []const u8) noreturn {
    const stderr = &std.io.getStdErr().outStream().stream;
    stderr.print("{}\n", .{msg}) catch {};
    process.exit(1);
}

fn debug_dump(alloc: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len != 1) {
        print_and_exit("expected one argument");
    }
    const source = try std.fs.cwd().readFileAlloc(alloc, args[0], 1024 * 1024);

    var errors = bog.Errors.init(alloc);
    var module = bog.compile(alloc, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError, error.ParseError, error.CompileError => {
            const stream = &std.io.getStdErr().outStream().stream;
            try errors.render(source, stream);
            process.exit(1);
        },
    };

    const stream = &std.io.getStdOut().outStream().stream;
    try module.dump(stream);
    process.exit(0);
}

comptime {
    _ = @import("tokenizer.zig");
    _ = @import("value.zig");
}
