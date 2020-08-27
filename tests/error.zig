test "invalid tag unwrap" {
    expectError(
        \\let foo = @bar[2]
        \\let @foo[baz] = foo
    ,
        \\invalid tag
    );
}

test "missing capture" {
    expectError(
        \\let error = [2]
    ,
        \\expected a capture
    );
    expectError(
        \\let @foo = [2]
    ,
        \\expected a capture
    );
}

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

var state = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = &state.allocator;

fn expectError(source: []const u8, expected: []const u8) void {
    var vm = Vm.init(alloc, .{});
    defer vm.deinit();

    _ = vm.run(source) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            const result = vm.errors.list.at(0).*.msg;
            std.testing.expectEqualStrings(expected, result);
            return;
        },
    };
    @panic("test failed: expected error");
}
