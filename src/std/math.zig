const std = @import("std");
const math = std.math;
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

/// Euler's number (e)
pub const e = math.e;

/// Archimedes' constant (π)
pub const pi = math.pi;

/// Circle constant (τ)
pub const tau = math.tau;

/// log2(e)
pub const log2e = math.log2e;

/// log10(e)
pub const log10e = math.log10e;

/// ln(2)
pub const ln2 = math.ln2;

/// ln(10)
pub const ln10 = math.ln10;

/// 2/sqrt(π)
pub const two_sqrtpi = math.two_sqrtpi;

/// sqrt(2)
pub const sqrt2 = math.sqrt2;

/// 1/sqrt(2)
pub const sqrt1_2 = math.sqrt1_2;

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
