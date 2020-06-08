test "break outside loop" {
    expectError(
        \\break
    ,
        \\break outside of loop
    );
}

test "invalid type" {
    expectError(
        \\foo
    ,
        \\use of undeclared identifier
    );
}

test "invalid map" {
    expectError(
        \\let y = {1}
    ,
        \\expected a key
    );
}

test "index out of bounds" {
    expectError(
        \\let y = [0,0,0]
        \\y[y["len"]] = true
    ,
        \\index out of bounds
    );
}

test "invalid namespace for native" {
    expectError(
        \\native("foo")
    ,
        \\invalid namespace
    );
}

test "invalid type" {
    expectError(
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

fn expectError(source: []const u8, expected: []const u8) void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var vm = Vm.init(alloc, .{});

    run(alloc, source, &vm) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            const result = vm.errors.list.at(0).*.msg;
            std.testing.expectEqualStrings(expected, result);
            return;
        },
    };
    @panic("test failed: expected error");
}

fn run(alloc: *mem.Allocator, source: []const u8, vm: *Vm) !void {
    var module = try bog.compile(alloc, source, &vm.errors);

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.entry;
    _ = try vm.exec(module);
}
