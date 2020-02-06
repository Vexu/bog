const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const lang = @import("lang.zig");
const Op = lang.Op;
const Value = lang.Value;
const Ref = lang.Ref;
const RegRef = lang.RegRef;
const Gc = @import("gc.zig").Gc;

pub const Vm = struct {
    /// Instruction pointer
    ip: usize,
    call_stack: CallStack,
    gc: Gc,
    repl: bool,
    result: ?Ref = null,

    const CallStack = std.SegmentedList(FunctionFrame, 16);

    const FunctionFrame = struct {
        return_ip: ?usize,
        result_reg: u8,
    };

    pub const ExecError = error{
        MalformedByteCode,
        TypeError,
        OtherError, // TODO

        // TODO remove possibility
        Unimplemented,
    } || Allocator.Error;

    pub fn init(allocator: *Allocator, repl: bool) Vm {
        return Vm{
            .ip = 0,
            .gc = Gc.init(allocator),
            .call_stack = CallStack.init(allocator),
            .repl = repl,
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.call_stack.deinit();
        vm.gc.deinit();
    }

    // TODO some safety
    // TODO rename to step and execute 1 instruction
    pub fn exec(vm: *Vm, module: *lang.Module) ExecError!void {
        // TODO
        const stack = vm.gc.stack.toSlice();
        try vm.call_stack.push(.{
            .return_ip = null,
            .result_reg = 0,
        });
        defer _ = vm.call_stack.pop();
        while (vm.ip < module.code.len) {
            const op = @intToEnum(Op, vm.getVal(module, u8));
            switch (op) {
                .ConstInt8 => {
                    const reg = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, i8);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                    stack[reg] = ref;
                },
                .ConstInt32 => {
                    const reg = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, i32);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                    stack[reg] = ref;
                },
                .ConstInt64 => {
                    const reg = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, i64);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                    stack[reg] = ref;
                },
                .ConstPrimitive => {
                    const reg = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, u8);

                    if (val == 0) {
                        stack[reg].value.? = &Value.None;
                    } else {
                        const ref = try vm.gc.alloc();
                        ref.value.?.* = .{
                            .kind = .{
                                .Bool = (val - 1) != 0,
                            },
                        };
                        stack[reg] = ref;
                        // TODO https://github.com/ziglang/zig/issues/4295
                        // frame.stack[inst.A].value = if (inst.B != 0) &Value.True else &Value.False;
                    }
                },
                // .Add => {
                //     // TODO check numeric
                //     const ref = try vm.gc.alloc();
                //     ref.value.?.* = .{
                //         .kind = .{
                //             .Int = frame.stack[inst.B].value.?.kind.Int + frame.stack[inst.C].value.?.kind.Int,
                //         },
                //     };
                //     frame.stack[inst.A] = ref;
                // },
                .BoolNot => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    if (stack[B].value.?.kind != .Bool) {
                        // TODO error expected boolean
                        return error.TypeError;
                    }

                    // TODO https://github.com/ziglang/zig/issues/4295
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Bool = !stack[B].value.?.kind.Bool,
                        },
                    };
                    stack[A] = ref;
                },
                .Discard => {
                    const reg = vm.getVal(module, RegRef);
                    if (vm.repl and vm.call_stack.len == 1) {
                        vm.result = stack[reg];
                    } else {
                        const val = stack[reg].value.?;
                        if (val.kind == .Error) {
                            // TODO error discarded
                        }
                        // val.deref();
                        return error.Unimplemented;
                    }
                },
                else => {
                    std.debug.warn("Unimplemented: {}\n", .{op});
                },
            }
        }
    }

    fn getVal(vm: *Vm, module: *lang.Module, comptime T: type) T {
        const val = @ptrCast(*const align(1) T, module.code[vm.ip..].ptr).*;
        vm.ip += @sizeOf(T);
        return val;
    }
};
