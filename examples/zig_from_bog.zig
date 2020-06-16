const std = @import("std");
const bog = @import("bog");

pub fn pow(val: i64) i64 {
    return val * val;
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    const source = try std.fs.cwd().readFileAlloc(allocator, "examples/zig_from_bog.bog", 1024);
    defer allocator.free(source);

    var vm = bog.Vm.init(allocator, .{});
    defer vm.deinit();
    try vm.addPackage("pow", pow);

    const res = vm.run(source) catch |e| switch (e) {
        else => |err| return err,
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            try vm.errors.render(source, std.io.getStdErr().outStream());
            return error.RunningBogFailed;
        },
    };

    const bog_integer = try res.bogToZig(i64, &vm);
    std.debug.assert(bog_integer == 8);
}
