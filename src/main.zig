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
    try stdout.write(usage);
    process.exit(0);
}

fn run(alloc: *std.mem.Allocator, args: [][]const u8) !void {
    std.debug.assert(args.len > 0);
    var bytecode = false;
    if (mem.endsWith(u8, args[0], bog.bytecode_extension)) {
        bytecode = true;
    }

    var vm = bog.Vm.init(alloc, false);
    defer vm.deinit();

    const source = std.fs.cwd().readFileAlloc(alloc, args[0], 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            print_and_exit("unable to open '{}': {}", .{ args[0], err });
        },
    };
    const res = run_source(alloc, &vm, source) catch |e| switch (e) {
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => print_errors_and_exit(&vm.errors, source),
        error.MalformedByteCode => if (is_debug) @panic("malformed") else print_and_exit("attempted to execute invalid bytecode", .{}),
        error.OutOfMemory => return error.OutOfMemory,
    };
    if (res) |some| {
        if (some.kind == .Int and some.kind.Int >= 0 and some.kind.Int < std.math.maxInt(u8)) {
            process.exit(@intCast(u8, some.kind.Int));
        } else if (some.kind == .Error) {
            const stderr = &std.io.getStdErr().outStream().stream;
            try stderr.write("script exited with error: ");
            try some.kind.Error.dump(stderr, 4);
            try stderr.write("\n");
            process.exit(1);
        }
        print_and_exit("invalid return type '{}'", .{@tagName(some.kind)});
    }
}

fn run_source(alloc: *std.mem.Allocator, vm: *bog.Vm, source: []const u8) !?*bog.Value {
    var module = try bog.compile(alloc, source, &vm.errors);

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.start_index;
    return try vm.exec(&module);
}

const usage_fmt =
    \\usage: bog fmt [file]...
    \\
    \\   Formats the input files.
    \\
;

fn fmt(alloc: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) {
        print_and_exit(usage_fmt, .{});
    }
    // TODO handle dirs
    const source = std.fs.cwd().readFileAlloc(alloc, args[0], 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.IsDir => print_and_exit("TODO fmt dirs", .{}),
        else => |err| {
            print_and_exit("unable to open '{}': {}", .{ args[0], err });
        },
    };

    var errors = bog.Errors.init(alloc);
    var tree = bog.parse(alloc, source, &errors) catch |e| switch (e) {
        error.TokenizeError, error.ParseError => print_errors_and_exit(&errors, source),
        error.OutOfMemory => return error.OutOfMemory,
    };

    const file = try std.fs.cwd().createFile(args[0], .{});

    var out_stream = &file.outStream().stream;
    try tree.render(out_stream);

    file.close();
    process.exit(0);
}

fn print_errors_and_exit(errors: *bog.Errors, source: []const u8) noreturn {
    const stderr = &std.io.getStdErr().outStream().stream;
    errors.render(source, stderr) catch {};
    process.exit(1);
}

fn print_and_exit(comptime msg: []const u8, args: var) noreturn {
    const stderr = &std.io.getStdErr().outStream().stream;
    stderr.print(msg ++ "\n", args) catch {};
    process.exit(1);
}

fn debug_dump(alloc: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len != 1) {
        print_and_exit("expected one argument", .{});
    }
    const source = std.fs.cwd().readFileAlloc(alloc, args[0], 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.IsDir => print_and_exit("TODO fmt dirs", .{}),
        else => |err| {
            print_and_exit("unable to open '{}': {}", .{ args[0], err });
        },
    };

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
