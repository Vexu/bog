const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub fn print(vals: Value.Variadic(*Value)) !void {
    var buf_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const writer = buf_writer.writer();
    for (vals.t, 0..) |val, i| {
        if (i != 0) try writer.writeByte(' ');
        if (val.* == .str) {
            try writer.writeAll(val.str.data);
        } else {
            try val.dump(writer, 4);
        }
    }
    try writer.writeByte('\n');
    try buf_writer.flush();
}

pub fn input(ctx: Vm.Context, prompt: []const u8) ![]u8 {
    try std.io.getStdOut().writer().writeAll(prompt);

    return try std.io.getStdIn().reader().readUntilDelimiterAlloc(ctx.vm.gc.gpa, '\n', 1024 * 1024);
}
