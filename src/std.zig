const std = @import("std");
const bog = @import("bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

/// std.io
pub const io = struct {
    pub fn print(val: *Value) !void {
        const stream = std.io.getStdOut().outStream();
        try val.dump(stream, 4);
        try stream.writeByte('\n');
    }

    pub fn input(vm: *Vm, prompt: []const u8) ![]u8 {
        try std.io.getStdOut().outStream().writeAll(prompt);

        // TODO properly gc this
        return try std.io.getStdIn().inStream().readUntilDelimiterAlloc(vm.gc.gpa, '\n', 1024 * 1024);
    }
};
