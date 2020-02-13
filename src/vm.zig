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
                    const A = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, i8);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                    stack[A] = ref;
                },
                .ConstInt32 => {
                    const A = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, i32);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                    stack[A] = ref;
                },
                .ConstInt64 => {
                    const A = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, i64);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                    stack[A] = ref;
                },
                .ConstNum => {
                    const A = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, f64);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Num = val,
                        },
                    };
                    stack[A] = ref;
                },
                .ConstPrimitive => {
                    const A = vm.getVal(module, RegRef);
                    const val = vm.getVal(module, u8);

                    if (val == 0) {
                        stack[A].value.? = &Value.None;
                    } else {
                        stack[A].value = if (val == 2) &Value.True else &Value.False;
                    }
                },
                .Add => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = stack[B].value.?.kind.Int + stack[C].value.?.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Sub => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = stack[B].value.?.kind.Int - stack[C].value.?.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Mul => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = stack[B].value.?.kind.Int * stack[C].value.?.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Pow => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = std.math.powi(i64, stack[B].value.?.kind.Int, stack[C].value.?.kind.Int) catch @panic("TODO: overflow"),
                        },
                    };
                    stack[A] = ref;
                },
                .DivFloor => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = @divFloor(stack[B].value.?.kind.Int, stack[C].value.?.kind.Int),
                        },
                    };
                    stack[A] = ref;
                },
                .Move => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    stack[A] = stack[B];
                },
                .DirectAdd => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    stack[A].value.?.kind.Int += stack[B].value.?.kind.Int;
                },
                .DirectSub => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    stack[A].value.?.kind.Int -= stack[B].value.?.kind.Int;
                },
                .DirectMul => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    stack[A].value.?.kind.Int *= stack[B].value.?.kind.Int;
                },
                .DirectPow => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    stack[A].value.?.kind.Int = std.math.powi(i64, stack[A].value.?.kind.Int, stack[B].value.?.kind.Int) catch @panic("TODO: overflow");
                },
                .DirectDivFloor => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    stack[A].value.?.kind.Int *= stack[B].value.?.kind.Int;
                    stack[A].value.?.kind.Int = @divFloor(stack[A].value.?.kind.Int, stack[B].value.?.kind.Int);
                },
                .BoolNot => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    if (stack[B].value.?.kind != .Bool) {
                        // TODO error expected boolean
                        return error.TypeError;
                    }

                    stack[A].value = if (stack[B].value.?.kind.Bool) &Value.False else &Value.True;
                },
                .BitNot => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = ~stack[B].value.?.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Negate => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);

                    // TODO check numeric
                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = -stack[B].value.?.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .JumpFalse => {
                    const A = vm.getVal(module, RegRef);
                    const addr = vm.getVal(module, u32);

                    if (stack[A].value.?.kind.Bool == false) {
                        vm.ip += addr;
                    }
                },
                .Jump => {
                    const addr = vm.getVal(module, u32);
                    vm.ip += addr;
                },
                .Discard => {
                    const A = vm.getVal(module, RegRef);
                    if (vm.repl and vm.call_stack.len == 1) {
                        vm.result = stack[A];
                    } else {
                        const val = stack[A].value.?;
                        if (val.kind == .Error) {
                            // TODO error discarded
                        }
                        // val.deref();
                        return error.Unimplemented;
                    }
                },
                .BuildTuple => {
                    const A = vm.getVal(module, RegRef);
                    const args = vm.getVal(module, []align(1) const RegRef);

                    // TODO gc this
                    const vals = try vm.call_stack.allocator.alloc(Ref, args.len);
                    for (args) |a, i| {
                        vals[i] = stack[a];
                    }

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Tuple = vals,
                        },
                    };
                    stack[A] = ref;
                },
                .Subscript => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    stack[A] = switch (stack[B].value.?.kind) {
                        .Tuple => |val| val[@intCast(u32, stack[C].value.?.kind.Int)],
                        else => @panic("TODO: subscript for more types"),
                    };
                },
                .As => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const type_id = vm.getVal(module, Value.TypeId);

                    if (type_id == .None) {
                        stack[A].value.? = &Value.None;
                        continue;
                    }

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = switch (type_id) {
                        .None => unreachable,
                        else => @panic("TODO more casts"),
                    };
                    stack[A] = ref;
                },
                .Is => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const type_id = vm.getVal(module, Value.TypeId);

                    stack[A].value = if (stack[B].value.?.kind == type_id) &Value.True else &Value.False;
                },
                else => {
                    std.debug.warn("Unimplemented: {}\n", .{op});
                },
            }
        }
    }

    fn getVal(vm: *Vm, module: *lang.Module, comptime T: type) T {
        if (T == []align(1) const RegRef) {
            const len = vm.getVal(module, u16);
            const val = @ptrCast([*]align(1) const RegRef, module.code[vm.ip..].ptr);
            vm.ip += @sizeOf(RegRef) * len;
            return val[0..len];
        }
        const val = @ptrCast(*align(1) const T, module.code[vm.ip..].ptr).*;
        vm.ip += @sizeOf(T);
        return val;
    }
};
