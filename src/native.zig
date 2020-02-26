const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const lang = @import("lang.zig");
const Value = lang.Value;
const Vm = lang.Vm;

pub const NativeError = error{
    TypeError,
    RuntimeError,
} || Allocator.Error;

pub const NativeFn = fn (*Vm) NativeError!*Value;

pub fn bind(func: var) NativeFn {
    const Fn = @typeInfo(@TypeOf(func)).Fn;
    if (Fn.is_generic or Fn.is_var_args or Fn.return_type == null)
        @compileError("invalid function");

    return struct {
        fn native(vm: *Vm) NativeError!*Value {
            switch (Fn.args.len) {
                0 => return valueFromType(vm, func()),
                1 => return valueFromType(vm, func(try typeFromValue(Fn.args[0].arg_type.?, undefined))),
                else => @compileError("TODO"),
            }
        }
    }.native;
}

fn valueFromType(vm: *Vm, val: var) *Value {
    switch (@TypeOf(val)) {
        void => return &Value.None,
        bool => return if (val) &Value.True else &Value.False,
        else => @compileError("TODO unsupported type"),
    }
}

fn typeFromValue(T: type, val: *Value) !T {
    switch (T) {
        void => {
            if (val.kind != .None) return error.TypeError;
        },
        bool => {
            if (val.kind != .Bool) return error.TypeError;
            return val.kind.Bool;
        },
        f32 => {
            if (val.kind == .Num) {
                return @floatCast(f32, val.kind.Num);
            } else if (val.kind == .Int) {
                return @intToFloat(f32, val.kind.Int);
            } else return error.TypeError;
        },
        f64 => {
            if (val.kind == .Num) {
                return val.kind.Num;
            } else if (val.kind == .Int) {
                return @intToFloat(f64, val.kind.Int);
            } else return error.TypeError;
        },
        []const u8 => {
            if (val.kind != .Str) return error.TypeError;
            return val.kind.Str;
        },
        else => @compileError("TODO unsupported type"),
    }
}
