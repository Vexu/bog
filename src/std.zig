const std = @import("std");
const bog = @import("bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;
const Registry = bog.native.Registry;

/// std.io
pub const io = struct {
    fn print(str: []const u8) void {
        const stream = &std.io.getStdOut().outStream().stream;
        stream.write(str) catch return;
    }

    pub fn register(reg: *Registry) !void {
        try reg.register("std.print", print);
    }
};

pub fn registerAll(reg: *Registry) !void {
    try io.register(reg);
}
