const std = @import("std");
const bog = @import("bog");

pub fn pow(val: i64) i64 {
    return val * val;
}

pub fn main() !void {
    var state = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = state.allocator();

    var vm = bog.Vm.init(allocator, .{});
    defer vm.deinit();
    try vm.addPackage("pow", pow);

    const path = "examples/zig_from_bog.bog";
    var mod = mod: {
        const source = try std.fs.cwd().readFileAlloc(vm.gc.gpa, path, vm.options.max_import_size);
        errdefer vm.gc.gpa.free(source);

        break :mod bog.compile(vm.gc.gpa, source, path, &vm.errors) catch |e| switch (e) {
            else => |err| return err,
            error.TokenizeError, error.ParseError, error.CompileError => {
                try vm.errors.render(std.io.getStdErr().writer());
                return error.RunningBogFailed;
            },
        };
    };
    errdefer mod.deinit(vm.gc.gpa);

    var frame = bog.Vm.Frame{
        .this = bog.Value.Null,
        .mod = &mod,
        .body = mod.main,
        .caller_frame = null,
        .module_frame = undefined,
        .captures = &.{},
        .params = 0,
    };
    defer frame.deinit(&vm);
    frame.module_frame = &frame;

    vm.gc.stack_protect_start = @frameAddress();

    const frame_val = try vm.gc.alloc(.frame);
    frame_val.* = .{ .frame = &frame };
    defer frame_val.* = .{ .int = 0 }; // clear frame

    const res = try vm.run(&frame);

    const bog_integer = try res.bogToZig(i64, frame.ctx(&vm));
    std.debug.assert(bog_integer == 8);
}
