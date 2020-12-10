const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub const pi = std.math.pi;
pub const e = std.math.e;
pub const sqrt2 = std.math.sqrt2;
pub const ln2 = std.math.ln2;

pub fn ln(vm: *Vm, val: Value) !*Value {
    return switch (val) {
        // TODO fix zig std
        // .int => |i| Value{
        //     .int = std.math.ln(i),
        // },
        .num => |n| {
            const res = try vm.gc.alloc();
            res.* = Value{ .num = std.math.ln(n) };
            return res;
        },
        else => vm.typeError(.num, val),
    };
}

pub fn sqrt(vm: *Vm, val: Value) !*Value {
    return switch (val) {
        .int => |i| {
            const res = try vm.gc.alloc();
            res.* = Value{ .int = std.math.sqrt(i) };
            return res;
        },
        .num => |n| {
            const res = try vm.gc.alloc();
            res.* = Value{ .num = std.math.sqrt(n) };
            return res;
        },
        else => vm.typeError(.num, val),
    };
}

pub fn round(val: f64) f64 {
    return std.math.round(val);
}

pub fn cos(val: f64) f64 {
    return std.math.cos(val);
}

pub fn sin(val: f64) f64 {
    return std.math.sin(val);
}

pub fn tan(val: f64) f64 {
    return std.math.tan(val);
}
