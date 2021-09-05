const std = @import("std");
const process = std.process;
const mem = std.mem;
const bog = @import("bog.zig");
const repl = bog.repl;

const is_debug = @import("builtin").mode == .Debug;
var state = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const gpa = &state.allocator;

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

    const in = std.io.bufferedReader(std.io.getStdIn().reader()).reader();
    var stdout = std.io.getStdOut().writer();

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
    try std.io.getStdOut().writer().writeAll(usage);
    process.exit(0);
}

fn run(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    std.debug.assert(args.len > 0);
    const file_name = args[0];

    var vm = bog.Vm.init(gpa, .{ .import_files = true });
    defer vm.deinit();
    try vm.addStd();
    const S = struct {
        var _args: [][]const u8 = undefined;

        fn argsToBog(_vm: *bog.Vm) bog.Vm.Error!*bog.Value {
            const ret = try _vm.gc.alloc();
            ret.* = .{ .list = .{} };
            var list = &ret.list;
            errdefer list.deinit(_vm.gc.gpa);
            try list.ensureCapacity(_vm.gc.gpa, _args.len);

            for (_args) |arg| {
                const str = try _vm.gc.alloc();
                str.* = bog.Value.string(arg);
                list.appendAssumeCapacity(str);
            }
            return ret;
        }
    };
    S._args = args[0..];
    try vm.imports.putNoClobber(vm.gc.gpa, "args", S.argsToBog);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            fatal("unable to open '{s}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var module = bog.Module.read(source) catch |e| switch (e) {
        // not a bog bytecode file
        error.InvalidMagic => null,
        else => |err| fatal("cannot execute file '{s}': {}", .{ file_name, err }),
    };

    // TODO this doesn't cast nicely for some reason
    const res_with_err = (if (module) |*some| blk: {
        vm.ip = some.entry;
        break :blk vm.exec(some);
    } else vm.run(source));
    const res = res_with_err catch |e| switch (e) {
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            vm.errors.render(source, std.io.getStdErr().writer()) catch {};
            process.exit(1);
        },
        error.MalformedByteCode => if (is_debug) @panic("malformed") else fatal("attempted to execute invalid bytecode", .{}),
        error.OutOfMemory => return error.OutOfMemory,
    };

    switch (res.*) {
        .int => |int| {
            if (int >= 0 and int < std.math.maxInt(u8)) {
                process.exit(@intCast(u8, int));
            } else {
                fatal("invalid exit code: {}", .{int});
            }
        },
        .err => |err| {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeAll("script exited with error: ");
            try err.dump(stderr, 4);
            try stderr.writeAll("\n");
            process.exit(1);
        },
        .none => {},
        else => fatal("invalid return type '{s}'", .{@tagName(res.*)}),
    }
}

const usage_fmt =
    \\usage: bog fmt [file]...
    \\
    \\   Formats the input files.
    \\
;

fn fmt(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len == 0) fatal("expected at least one file", .{});

    var any_err = false;
    for (args) |arg| {
        any_err = (try fmtFile(gpa, arg)) or any_err;
    }
    if (any_err) process.exit(1);
}

const FmtError = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.Writer.Error ||
    std.fs.Dir.OpenError || std.fs.File.GetSeekPosError || std.fs.Dir.Iterator.Error ||
    std.fs.File.Reader.Error || error{EndOfStream};

fn fmtFile(gpa: *std.mem.Allocator, name: []const u8) FmtError!bool {
    const source = std.fs.cwd().readFileAlloc(gpa, name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.IsDir => {
            var dir = std.fs.cwd().openDir(name, .{ .iterate = true }) catch |e2| {
                try std.io.getStdErr().writer().print("unable to open '{s}': {}\n", .{ name, e2 });
                return e2;
            };
            var any_err = false;
            defer dir.close();
            var it = dir.iterate();
            while (try it.next()) |entry| if (entry.kind == .Directory or std.mem.endsWith(u8, entry.name, bog.extension)) {
                const full_path = try std.fs.path.join(gpa, &[_][]const u8{ name, entry.name });
                defer gpa.free(full_path);
                any_err = (try fmtFile(gpa, full_path)) or any_err;
            };
            return any_err;
        },
        else => |err| {
            try std.io.getStdErr().writer().print("unable to open '{s}': {}\n", .{ name, err });
            return err;
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    var tree = bog.parse(gpa, source, &errors) catch |e| switch (e) {
        error.TokenizeError, error.ParseError => {
            try errors.render(source, std.io.getStdErr().writer());
            return true;
        },
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer tree.deinit();

    const file = try std.fs.cwd().createFile(name, .{});
    defer file.close();

    // TODO add check mode
    _ = try tree.render(file.writer());
    return false;
}

fn fatal(comptime msg: []const u8, args: anytype) noreturn {
    std.io.getStdErr().writer().print(msg ++ "\n", args) catch {};
    process.exit(1);
}

fn getFileName(usage_arg: []const u8, args: [][]const u8) []const u8 {
    if (args.len != 1) {
        fatal("{s}", .{usage_arg});
    }
    return args[0];
}

const usage_debug =
    \\usage: bog debug:[command] [file]
    \\
;

fn debugDump(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            fatal("unable to open '{s}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    var module = bog.compile(gpa, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError, error.ParseError, error.CompileError => {
            try errors.render(source, std.io.getStdErr().writer());
            process.exit(1);
        },
    };
    defer module.deinit(gpa);

    try module.dump(gpa, std.io.getStdOut().writer());
}

fn debugTokens(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            fatal("unable to open '{s}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    const tokens = bog.tokenize(gpa, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError => {
            try errors.render(source, std.io.getStdErr().writer());
            process.exit(1);
        },
    };
    defer gpa.free(tokens);

    const stream = std.io.getStdOut().writer();
    for (tokens) |tok| {
        switch (tok.id) {
            .Nl, .Eof => try stream.print("{s}\n", .{@tagName(tok.id)}),
            .Indent => |level| try stream.print("{s} {}\n", .{ @tagName(tok.id), level }),
            else => try stream.print("{s} |{s}|\n", .{ @tagName(tok.id), source[tok.start..tok.end] }),
        }
    }
}

const usage_debug_write =
    \\usage: bog debug:write [file] [out]
    \\
;

fn debugWrite(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    if (args.len != 2) {
        fatal("{s}", .{usage_debug_write});
    }
    const file_name = args[0];

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            fatal("unable to open '{s}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    var errors = bog.Errors.init(gpa);
    defer errors.deinit();

    var module = bog.compile(gpa, source, &errors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TokenizeError, error.ParseError, error.CompileError => {
            try errors.render(source, std.io.getStdErr().writer());
            process.exit(1);
        },
    };
    defer module.deinit(gpa);

    const file = try std.fs.cwd().createFile(args[1], .{});
    defer file.close();

    try module.write(file.writer());
}

fn debugRead(gpa: *std.mem.Allocator, args: [][]const u8) !void {
    const file_name = getFileName(usage_debug, args);

    const source = std.fs.cwd().readFileAlloc(gpa, file_name, 1024 * 1024) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            fatal("unable to open '{s}': {}", .{ file_name, err });
        },
    };
    defer gpa.free(source);

    const module = try bog.Module.read(source);

    try module.dump(gpa, std.io.getStdOut().writer());
}

comptime {
    _ = main;
}
