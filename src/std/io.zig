const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub fn print(val: *Value) !void {
    const stream = std.io.getStdOut().outStream();
    try val.dump(stream, 4);
    try stream.writeByte('\n');
}

pub fn input(vm: *Vm, prompt: Value.String) ![]u8 {
    try prompt.print(std.io.getStdOut().writer());

    return try std.io.getStdIn().inStream().readUntilDelimiterAlloc(vm.gc.gpa, '\n', 1024 * 1024);
}
