const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub const pi = std.math.pi;
pub const e = std.math.e;

pub fn ln(vm: *Vm, val: Value) !Value {
    return switch (val) {
        // TODO fix zig std
        // .int => |i| Value{
        //     .int = std.math.ln(i),
        // },
        .num => |n| Value{
            .num = std.math.ln(n),
        },
        else => vm.reportErr("expected a number"),
    };
}
