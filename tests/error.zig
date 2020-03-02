test "invalid namespace for native" {
    try expectError(
        \\native("foo")
    ,
        \\invalid namespace
    );
}

test "invalid type" {
    try expectError(
        \\1 + true
    ,
        \\expected a number
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const testing = std.testing;
const bog = @import("bog");
const Vm = bog.Vm;
const Errors = bog.Errors;

var buffer: [10 * 1024]u8 = undefined;

fn expectError(source: []const u8, expected: []const u8) !void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var vm = Vm.init(alloc, .{});

    run(alloc, source, &vm) catch |e| switch (e) {
        else => return e,
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            const result = vm.errors.list.at(0).*.msg;
            if (!mem.eql(u8, result, expected)) {
                warn("\n---expected----\n{}\n-----found-----\n{}\n---------------\n", .{ expected, result });
                return error.TestFailed;
            }
            return;
        },
    };
    return error.TestFailed;
}

fn run(alloc: *mem.Allocator, source: []const u8, vm: *Vm) !void {
    var module = try bog.compile(alloc, source, &vm.errors);

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.entry;
    _ = try vm.exec(&module);
}
