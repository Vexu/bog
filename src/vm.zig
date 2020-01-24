const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bytecode = @import("bytecode.zig");
const Instruction = bytecode.Instruction;
const value = @import("value.zig");
const Value = value.Value;
const Ref = value.Ref;

pub const Vm = struct {
    /// Instruction pointer
    ip: usize,

    /// Stack pointer
    sp: usize,
    code: []const u32,
    stack: Stack,

    // TODO *Ref
    const Stack = std.SegmentedList(Value, 256);

    pub fn init(allocator: *Allocator) Vm {
        return Vm{
            .ip = 0,
            .sp = 0,
            .code = @as([0]u32, undefined)[0..0],
            .stack = Stack.init(allocator),
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.stack.deinit();
    }

    // TODO some safety
    pub fn exec(vm: *Vm) anyerror!void {
        while (vm.ip < vm.code.len) : (vm.ip += 1) {
            const inst = @bitCast(Instruction, vm.code[vm.ip]);
            switch (inst.op) {
                .ConstSmallInt => {
                    vm.ip += 1;
                    // TODO frame[vm.sp + inst.A]
                    try vm.stack.push(.{
                        .kind = .{
                            .Int = vm.code[vm.ip],
                        },
                    });
                },
                .Add => {
                    // TODO frame[vm.sp + inst.A]
                    const arg1 = vm.stack.pop().?;
                    const arg2 = vm.stack.pop().?;
                    try vm.stack.push(.{
                        .kind = .{
                            .Int = arg1.kind.Int + arg2.kind.Int,
                        },
                    });
                },
                .Discard => {
                    const arg = vm.stack.pop().?;
                    std.debug.warn("discarded value: {}\n", .{arg});
                },
                else => {
                    std.debug.warn("Unimplemented: {}\n", .{inst.op});
                },
            }
        }
    }
};
