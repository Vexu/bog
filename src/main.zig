const std = @import("std");
const process = std.process;
const mem = std.mem;
const bog = @import("bog.zig");
const repl = bog.repl;

const is_debug = @import("builtin").mode == .Debug;

pub fn main() !void {
    const gpa = std.heap.c_allocator;

    const args = try process.argsAlloc(gpa);
    defer process.argsFree(gpa, args);

    if (args.len > 1) {
        if (mem.eql(u8, args[1], "fmt")) {
            return fmt(gpa, args[2..]);
        }
        if (mem.eql(u8, args[1], "help") or mem.eql(u8, args[1], "--help")) {
            return help();
        }
        if (is_debug) {
            if (mem.eql(u8, args[1], "debug:dump")) {
                return debugDump(gpa, args[2..]);
            }
            if (mem.eql(u8, args[1], "debug:tokens")) {
                return debugTokens(gpa, args[2..]);
            }
            if (mem.eql(u8, args[1], "debug:write")) {
                return debugWrite(gpa, args[2..]);
            }
            if (mem.eql(u8, args[1], "debug:read")) {
                return debugRead(gpa, args[2..]);
            }
        }
        if (!mem.startsWith(u8, "-", args[1])) {
            return run(gpa, args[1..]);
        }
    }

    const in = std.io.bufferedInStream(std.io.getStdIn().inStream()).inStream();
    var stdout = std.io.getStdOut().outStream();

    try repl.run(gpa, in, stdout);
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
    const stdout = &std.io.getStdOut().outStream();
    try stdout.writeAll(usage);
    process.exit(0);
}

fn run(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    std.debug.assert(args.len > 0);
    const file_name = args[0];

    var vm = bog.Vm.init(gpa, .{ .import_files = true });
    defer vm.deinit();
    try vm.addPackage("std.io", bog.std.io);
    const S = struct {
        var _args: [][]const u8 = undefined;

        fn argsToBog(_vm: *bog.Vm) bog.Vm.Error!*bog.Value {
            var list = bog.Value.List.init(_vm.gc.gpa);
            errdefer list.deinit();
            try list.ensureCapacity(_args.len);

            for (_args) |arg| {
                const str = try _vm.gc.alloc();
                str.* = .{
                    .str = arg,
                };
                list.appendAssumeCapacity(str);
            }
            const ret = try _vm.gc.alloc();
            ret.* = .{
                .list = list,
            };
            return ret;
        }
    };
    S._args = args[0..];
    try vm.imports.putNoClobber("args", S.argsToBog);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            printAndExit("unable to open '{}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var module = bog.Module.read(source) catch |e| switch (e) {
        // not a bog bytecode file
        error.InvalidMagic => null,
        else => |err| printAndExit("cannot execute file '{}': {}", .{ file_name, err }),
    };

    // TODO this doesn't cast nicely for some reason
    const res_with_err = (if (module) |*some| blk: {
        vm.ip = some.entry;
        break :blk vm.exec(some);
    } else
        vm.run(source));
    const res = res_with_err catch |e| switch (e) {
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => printErrorsAndExit(&vm.errors, source),
        error.MalformedByteCode => if (is_debug) @panic("malformed") else printAndExit("attempted to execute invalid bytecode", .{}),
        error.OutOfMemory => return error.OutOfMemory,
    };

    switch (res.*) {
        .int => |int| {
            if (int >= 0 and int < std.math.maxInt(u8)) {
                process.exit(@intCast(u8, int));
            } else {
                printAndExit("invalid exit code: {}", .{int});
            }
        },
        .err => |err| {
            const stderr = std.io.getStdErr().outStream();
            try stderr.writeAll("script exited with error: ");
            try err.dump(stderr, 4);
            try stderr.writeAll("\n");
            process.exit(1);
        },
        .none => {},
        else => printAndExit("invalid return type '{}'", .{@tagName(res.*)}),
    }
}

const usage_fmt =
    \\usage: bog fmt [file]...
    \\
    \\   Formats the input files.
    \\
;

fn fmt(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_fmt, args);

    // TODO handle dirs
    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.IsDir => printAndExit("TODO fmt dirs", .{}),
        else => |err| {
            printAndExit("unable to open '{}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    var tree = bog.parse(gpa, source, &errors) catch |e| switch (e) {
        error.TokenizeError, error.ParseError => printErrorsAndExit(&errors, source),
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer tree.deinit();

    const file = try std.fs.cwd().createFile(file_name, .{});
    defer file.close();

    try tree.render(file.outStream());
}

fn printErrorsAndExit(errors: *bog.Errors, source: []const u8) noreturn {
    errors.render(source, std.io.getStdErr().outStream()) catch {};
    process.exit(1);
}

fn printAndExit(comptime msg: []const u8, args: var) noreturn {
    std.io.getStdErr().outStream().print(msg ++ "\n", args) catch {};
    process.exit(1);
}

fn getFileName(usage_arg: []const u8, args: [][]const u8) []const u8 {
    if (args.len != 1) {
        printAndExit("{}", .{usage_arg});
    }
    return args[0];
}

const usage_debug =
    \\usage: bog fmt [file]...
    \\
    \\   Formats the input files.
    \\
;

fn debugDump(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            printAndExit("unable to open '{}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    var module = bog.compile(gpa, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError, error.ParseError, error.CompileError => {
            try errors.render(source, std.io.getStdErr().outStream());
            process.exit(1);
        },
    };
    defer module.deinit(gpa);

    try module.dump(gpa, std.io.getStdOut().outStream());
}

fn debugTokens(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            printAndExit("unable to open '{}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    const tokens = bog.tokenize(gpa, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError => {
            try errors.render(source, std.io.getStdErr().outStream());
            process.exit(1);
        },
    };
    defer gpa.free(tokens);

    const stream = std.io.getStdOut().outStream();
    for (tokens) |tok| {
        switch (tok.id) {
            .Nl, .Eof => try stream.print("{}\n", .{@tagName(tok.id)}),
            .Indent => |level| try stream.print("{} {}\n", .{ @tagName(tok.id), level }),
            else => try stream.print("{} |{}|\n", .{ @tagName(tok.id), source[tok.start..tok.end] }),
        }
    }
}

fn debugWrite(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            printAndExit("unable to open '{}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    var module = bog.compile(gpa, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError, error.ParseError, error.CompileError => {
            try errors.render(source, std.io.getStdErr().outStream());
            process.exit(1);
        },
    };
    defer module.deinit(gpa);

    const file = try std.fs.cwd().createFile(args[1], .{});
    defer file.close();

    try module.write(file.outStream());
}

fn debugRead(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            printAndExit("unable to open '{}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    const module = try bog.Module.read(source);

    try module.dump(gpa, std.io.getStdOut().outStream());
}

comptime {
    _ = @import("tokenizer.zig");
    _ = @import("value.zig");
}
