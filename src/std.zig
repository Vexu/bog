const std = @import("std");
const bog = @import("bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;
const Registry = bog.native.Registry;

/// std.io
pub const io = struct {
    fn print(val: *Value) !void {
        const stream = std.io.getStdOut().outStream();
        try val.dump(stream, 4);
        try stream.writeByte('\n');
    }

    fn input(vm: *Vm, prompt: []const u8) ![]u8 {
        try std.io.getStdOut().outStream().writeAll(prompt);

        // TODO properly gc this
        return try std.io.getStdIn().inStream().readUntilDelimiterAlloc(vm.gc.gpa, '\n', 1024 * 1024);
    }

    pub fn register(reg: *Registry) !void {
        try reg.register("std.print", print);
        try reg.register("std.input", input);
    }
};

pub fn registerAll(reg: *Registry) !void {
    try io.register(reg);
}
