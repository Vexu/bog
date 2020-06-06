const std = @import("std");
const span = std.mem.span;
const bog = @import("bog.zig");
const gpa = std.heap.c_allocator;

//! ABI WARNING -- REMEMBER TO CHANGE include/bog.h

const Error = extern enum {
    None,
    OutOfMemory,
    TokenizeError,
    ParseError,
    CompileError,
    RuntimeError,
    MalformedByteCode,
    NotAMap,
    NoSuchMember,
    NotAFunction,
    InvalidArgCount,
    NativeFunctionsUnsupported,
    IoError,
};

export fn bog_Vm_init(vm: **bog.Vm, import_files: bool) Error {
    const ptr = gpa.create(bog.Vm) catch |e| switch (e) {
        error.OutOfMemory => return .OutOfMemory,
    };
    ptr.* = bog.Vm.init(gpa, .{ .import_files = import_files });

    vm.* = ptr;
    return .None;
}

export fn bog_Vm_deinit(vm: *bog.Vm) void {
    vm.deinit();
}

export fn bog_Vm_addStd(vm: *bog.Vm) Error {
    bog.std.registerAll(&vm.native_registry) catch |e| switch (e) {
        error.OutOfMemory => return .OutOfMemory,
    };
    return .None;
}

export fn bog_Vm_run(vm: *bog.Vm, res: **bog.Value, source: [*:0]const u8) Error {
    res.* = vm.run(span(source)) catch |e| switch (e) {
        error.OutOfMemory => return .OutOfMemory,
        error.TokenizeError => return .TokenizeError,
        error.ParseError => return .ParseError,
        error.CompileError => return .CompileError,
        error.RuntimeError => return .RuntimeError,
        error.MalformedByteCode => return .MalformedByteCode,
    };

    return .None;
}

export fn bog_Vm_call(vm: *bog.Vm, res: **bog.Value, container: *bog.Value, func_name: [*:0]const u8) Error {
    res.* = vm.call(container, span(func_name), .{}) catch |e| switch (e) {
        error.OutOfMemory => return .OutOfMemory,
        error.RuntimeError => return .RuntimeError,
        error.MalformedByteCode => return .MalformedByteCode,
        error.NotAMap => return .NotAMap,
        error.NoSuchMember => return .NoSuchMember,
        error.NotAFunction => return .NotAFunction,
        error.InvalidArgCount => return .InvalidArgCount,
        error.NativeFunctionsUnsupported => return .NativeFunctionsUnsupported,
    };

    return .None;
}

export fn bog_Vm_renderErrors(vm: *bog.Vm, source: [*:0]const u8) Error {
    vm.errors.render(span(source), std.io.getStdErr().outStream()) catch return .IoError;

    return .None;
}
