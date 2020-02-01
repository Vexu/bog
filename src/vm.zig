const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const lang = @import("lang.zig");
const Op = lang.Op;
const Value = lang.Value;
const Ref = lang.Ref;
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
        stack: []Ref, // slice of vm.gc.stack
    };

    pub const ExecError = error{
        MalformedByteCode,
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
    pub fn exec(vm: *Vm, module: *lang.Module) ExecError!void {
        const frame = vm.call_stack.uncheckedAt(0);
        while (vm.ip < module.code.len) : (vm.ip += 1) {
            const op = @intToEnum(Op, module.code[vm.ip]);
            switch (op) {
                .ConstSmallInt => {
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = @bitCast(i32, arg),
                        },
                    };
                    frame.stack[inst.A] = ref;
                },
                .ConstNone => {
                    frame.stack[inst.A].value.? = &Value.None;
                },
                .ConstBool => {
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Bool = inst.B != 0,
                        },
                    };
                    frame.stack[inst.A] = ref;
                    // TODO https://github.com/ziglang/zig/issues/4295
                    // frame.stack[inst.A].value = if (inst.B != 0) &Value.True else &Value.False;
                },
                .Add => {
                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = frame.stack[inst.B].value.?.kind.Int + frame.stack[inst.C].value.?.kind.Int,
                        },
                    };
                    frame.stack[inst.A] = ref;
                },
                .Discard => {
                    if (vm.repl and vm.call_stack.len == 1) {
                        vm.result = frame.stack[inst.A];
                    } else {
                        const val = frame.stack[inst.A].value.?;
                        if (val.kind == .Error) {
                            // TODO error discarded
                        }
                        // val.deref();
                        return error.Unimplemented;
                    }
                },
                else => {
                    std.debug.warn("Unimplemented: {}\n", .{inst.op});
                },
            }
        }
    }
};
