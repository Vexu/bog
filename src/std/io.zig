const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub fn print(val: *Value) !void {
    const writer = std.io.getStdOut().writer();
    if (val.* == .str) {
        try writer.writeAll(val.str.data);
    } else {
        try val.dump(writer, 4);
    }
    try writer.writeByte('\n');
}

pub fn input(ctx: Vm.Context, prompt: []const u8) ![]u8 {
    try std.io.getStdOut().writer().writeAll(prompt);

    return try std.io.getStdIn().reader().readUntilDelimiterAlloc(ctx.vm.gc.gpa, '\n', 1024 * 1024);
}
