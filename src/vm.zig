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
    call_stack: CallStack,
    stack: Stack,
    repl: bool,
    result: ?*Value = null, // TODO

    const Stack = std.ArrayList(Value); // TODO *Ref once gc is made
    const CallStack = std.SegmentedList(FunctionFrame, 16);

    const FunctionFrame = struct {
        return_ip: ?usize,
        result_reg: u8,
        stack: []Value, // slice of vm.stack
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
            .stack = Stack.init(allocator),
            .call_stack = CallStack.init(allocator),
            .repl = repl,
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.stack.deinit();
        vm.call_stack.deinit();
    }

    // TODO some safety
    pub fn exec(vm: *Vm, code: []const u32) ExecError!void {
        vm.result = null;
        const frame = vm.call_stack.uncheckedAt(0);
        while (vm.ip < code.len) : (vm.ip += 1) {
            const inst = @bitCast(Instruction, code[vm.ip]);
            const arg = if (inst.op.hasArg()) blk: {
                vm.ip += 1;
                break :blk code[vm.ip];
            } else undefined;
            switch (inst.op) {
                .ConstSmallInt => {
                    frame.stack[inst.A] = .{
                        .kind = .{
                            .Int = arg,
                        },
                    };
                },
                .ConstBool => {
                    frame.stack[inst.A] = .{
                        .kind = .{
                            .Bool = inst.B != 0,
                        },
                    };
                },
                .Add => {
                    // TODO check numeric
                    frame.stack[inst.A] = .{
                        .kind = .{
                            .Int = frame.stack[inst.A].kind.Int + frame.stack[inst.B].kind.Int,
                        },
                    };
                },
                .Discard => {
                    if (vm.repl and vm.call_stack.len == 1) {
                        vm.result = &frame.stack[inst.A];
                    } else {
                        const val = frame.stack[inst.A];
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
