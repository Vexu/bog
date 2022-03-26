test "redeclaration" {
    try expectError(
        \\let i = 0
        \\let i = 1
    ,
        \\redeclaration of 'i'
    );
}

test "unexpected token" {
    try expectError(
        \\if (a b
    ,
        \\expected ',', found 'Identifier'
    );
}

test "unexpected arg count" {
    try expectError(
        \\let foo = fn (a, b) a + b
        \\foo(1)
    ,
        \\expected 2 args, got 1
    );
}

test "extra cases after catch-all" {
    try expectError(
        \\match (1)
        \\    let val => null
        \\    1 => null
        \\
    ,
        \\additional cases after a catch-all case
    );
}

test "extra handlers after catch-all" {
    try expectError(
        \\let foo = fn() null
        \\try
        \\    foo()
        \\catch
        \\    2
        \\catch 1
        \\    3
        \\
    ,
        \\additional handlers after a catch-all handler
    );
}

test "invalid tag unwrap" {
    try expectError(
        \\let foo = @bar[2]
        \\let @foo[baz] = foo
    ,
        \\invalid tag
    );
}

test "missing capture" {
    try expectError(
        \\let error = [2]
    ,
        \\expected a destructuring
    );
    try expectError(
        \\let @foo = [2]
    ,
        \\expected a destructuring
    );
}

test "break outside loop" {
    try expectError(
        \\break
    ,
        \\break outside of loop
    );
}

test "invalid type" {
    try expectError(
        \\foo
    ,
        \\use of undeclared identifier
    );
}

test "invalid map" {
    try expectError(
        \\let y = {1}
    ,
        \\expected a key
    );
}

test "index out of bounds" {
    try expectError(
        \\let y = [0, 0, 0]
        \\y[y["len"]] = true
    ,
        \\index out of bounds
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

fn expectError(source: []const u8, expected: []const u8) !void {
    var vm = Vm.init(std.testing.allocator, .{});
    defer vm.deinit();

    var mod = bog.compile(vm.gc.gpa, source, "<test buf>", &vm.errors) catch |e| switch (e) {
        else => return error.UnexpectedError,
        error.TokenizeError, error.ParseError, error.CompileError => {
            const result = vm.errors.list.items[0].msg;
            try std.testing.expectEqualStrings(expected, result.data);
            return;
        },
    };
    defer {
        mod.debug_info.source = "";
        mod.deinit(vm.gc.gpa);
    }

    var frame = bog.Vm.Frame{
        .mod = &mod,
        .body = mod.main,
        .caller_frame = null,
        .module_frame = undefined,
        .captures = &.{},
    };
    defer frame.deinit(&vm);
    frame.module_frame = &frame;

    vm.gc.stack_protect_start = @frameAddress();

    var frame_val = try vm.gc.alloc();
    frame_val.* = .{ .frame = &frame };

    _ = vm.run(&frame) catch |e| switch (e) {
        else => return error.UnexpectedError,
        error.FatalError => {
            const result = vm.errors.list.items[0].msg;
            try std.testing.expectEqualStrings(expected, result.data);
            return;
        },
    };

    return error.ExpectedError;
}
