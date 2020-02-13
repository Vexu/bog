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

    errors: lang.Error.List,

    const CallStack = std.SegmentedList(FunctionFrame, 16);

    const FunctionFrame = struct {
        return_ip: ?usize,
        result_reg: u8,
    };

    pub const Error = error{
        RuntimeError,
        MalformedByteCode,
    } || Allocator.Error;

    pub fn init(allocator: *Allocator, repl: bool) Vm {
        return Vm{
            .ip = 0,
            .gc = Gc.init(allocator),
            .call_stack = CallStack.init(allocator),
            .repl = repl,
            .errors = lang.Error.List.init(allocator),
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.call_stack.deinit();
        vm.errors.deinit();
        vm.gc.deinit();
    }

    // TODO some safety
    // TODO rename to step and execute 1 instruction
    pub fn exec(vm: *Vm, module: *lang.Module) Error!void {
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
                .ConstString => return vm.reportErr("TODO Op.ConstString"),
                .Add => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val.kind.Int + C_val.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Sub => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val.kind.Int - C_val.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Mul => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val.kind.Int * C_val.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Pow => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = std.math.powi(i64, B_val.kind.Int, C_val.kind.Int) catch @panic("TODO: overflow"),
                        },
                    };
                    stack[A] = ref;
                },
                .DivFloor => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = @divFloor(B_val.kind.Int, C_val.kind.Int),
                        },
                    };
                    stack[A] = ref;
                },
                .Div => return vm.reportErr("TODO Op.Div"),
                .Mod => return vm.reportErr("TODO Op.Mod"),
                .And => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getBool(module);
                    const C_val = try vm.getBool(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Bool = B_val and C_val,
                        },
                    };
                    stack[A] = ref;
                },
                .Or => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getBool(module);
                    const C_val = try vm.getBool(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Bool = B_val or C_val,
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
                    const A_val = try vm.getNumeric(module);
                    const B_val = try vm.getNumeric(module);

                    A_val.kind.Int += B_val.kind.Int;
                },
                .DirectSub => {
                    const A_val = try vm.getNumeric(module);
                    const B_val = try vm.getNumeric(module);

                    A_val.kind.Int -= B_val.kind.Int;
                },
                .DirectMul => {
                    const A_val = try vm.getNumeric(module);
                    const B_val = try vm.getNumeric(module);

                    A_val.kind.Int *= B_val.kind.Int;
                },
                .DirectPow => {
                    const A_val = try vm.getNumeric(module);
                    const B_val = try vm.getNumeric(module);

                    A_val.kind.Int = std.math.powi(i64, A_val.kind.Int, B_val.kind.Int) catch @panic("TODO: overflow");
                },
                .DirectDivFloor => {
                    const A_val = try vm.getNumeric(module);
                    const B_val = try vm.getNumeric(module);

                    A_val.kind.Int = @divFloor(A_val.kind.Int, B_val.kind.Int);
                },
                .DirectBitAnd => {
                    const A_val = try vm.getIntRef(module);
                    const B_val = try vm.getInt(module);

                    A_val.kind.Int &= B_val;
                },
                .DirectBitOr => {
                    const A_val = try vm.getIntRef(module);
                    const B_val = try vm.getInt(module);

                    A_val.kind.Int |= B_val;
                },
                .DirectBitXor => {
                    const A_val = try vm.getIntRef(module);
                    const B_val = try vm.getInt(module);

                    A_val.kind.Int ^= B_val;
                },
                .DirectLShift => {
                    const A_val = try vm.getIntRef(module);
                    const B_val = try vm.getInt(module);

                    // TODO check that B_val is small enough
                    A_val.kind.Int <<= @intCast(u6, B_val);
                },
                .DirectRShift => {
                    const A_val = try vm.getIntRef(module);
                    const B_val = try vm.getInt(module);

                    A_val.kind.Int >>= @intCast(u6, B_val);
                },
                .DirectDiv => return vm.reportErr("TODO Op.DirectDiv"),
                .DirectMod => return vm.reportErr("TODO Op.DirectMod"),
                .BoolNot => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getBool(module);

                    stack[A].value = if (B_val) &Value.False else &Value.True;
                },
                .BitNot => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getInt(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = ~B_val,
                        },
                    };
                    stack[A] = ref;
                },
                .BitAnd => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val & C_val,
                        },
                    };
                    stack[A] = ref;
                },
                .BitOr => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val | C_val,
                        },
                    };
                    stack[A] = ref;
                },
                .BitXor => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val ^ C_val,
                        },
                    };
                    stack[A] = ref;
                },
                .LShift => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val << @intCast(u6, B_val),
                        },
                    };
                    stack[A] = ref;
                },
                .RShift => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = B_val >> @intCast(u6, B_val),
                        },
                    };
                    stack[A] = ref;
                },
                .Negate => {
                    const A = vm.getVal(module, RegRef);
                    const B_val = try vm.getNumeric(module);

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = .{
                        .kind = .{
                            .Int = -B_val.kind.Int,
                        },
                    };
                    stack[A] = ref;
                },
                .Try => return vm.reportErr("TODO Op.Try"),
                .JumpFalse => {
                    const A_val = try vm.getBool(module);
                    const addr = vm.getVal(module, u32);

                    if (A_val == false) {
                        vm.ip += addr;
                    }
                },
                .Jump => {
                    const addr = vm.getVal(module, u32);
                    vm.ip += addr;
                },
                .JumpTrue => {
                    const A_val = try vm.getBool(module);
                    const addr = vm.getVal(module, u32);

                    if (A_val == true) {
                        vm.ip += addr;
                    }
                },
                .JumpNotError => {
                    const A = vm.getVal(module, RegRef);
                    const addr = vm.getVal(module, u32);

                    if (stack[A].value.?.kind != .Error) {
                        vm.ip += addr;
                    }
                },
                .Import => return vm.reportErr("TODO Op.Import"),
                .Discard => {
                    const A = vm.getVal(module, RegRef);
                    if (vm.repl and vm.call_stack.len == 1) {
                        vm.result = stack[A];
                    } else {
                        const val = stack[A].value.?;
                        if (val.kind == .Error) {
                            return vm.reportErr("error discarded");
                        }
                        // val.deref();
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
                .BuildError => return vm.reportErr("TODO Op.BuildError"),
                .Subscript => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const C = vm.getVal(module, RegRef);

                    stack[A] = switch (stack[B].value.?.kind) {
                        .Tuple => |val| val[@intCast(u32, stack[C].value.?.kind.Int)],
                        else => return vm.reportErr("TODO: subscript for more types"),
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
                    const B_kind = stack[B].value.?.kind;

                    if (type_id == .Bool) {
                        const bool_res = switch (B_kind) {
                            .Int => |val| val != 0,
                            .Num => |val| val != 0,
                            .Bool => |val| val,
                            .Str => |val| if (mem.eql(u8, val, "true"))
                                true
                            else if (mem.eql(u8, val, "false"))
                                false
                            else
                                return vm.reportErr("cannot cast string to bool"),
                            else => return vm.reportErr("invalid cast to bool"),
                        };

                        stack[A].value = if (bool_res) &Value.True else &Value.False;
                        continue;
                    }

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = switch (type_id) {
                        .Bool, .None => unreachable,
                        .Int => .{
                            .kind = .{
                                .Int = switch (B_kind) {
                                    .Int => |val| val,
                                    .Num => |val| @floatToInt(i64, val),
                                    .Bool => |val| @boolToInt(val),
                                    // .Str => parseInt
                                    else => return vm.reportErr("invalid cast to int"),
                                },
                            },
                        },
                        .Num => .{
                            .kind = .{
                                .Num = switch (B_kind) {
                                    .Num => |val| val,
                                    .Int => |val| @intToFloat(f64, val),
                                    .Bool => |val| @intToFloat(f64, @boolToInt(val)),
                                    // .Str => parseNum
                                    else => return vm.reportErr("invalid cast to num"),
                                },
                            },
                        },
                        .Str,
                        .Tuple,
                        .Map,
                        .List,
                        => return vm.reportErr("TODO more casts"),
                        else => return error.MalformedByteCode,
                    };
                    stack[A] = ref;
                },
                .Is => {
                    const A = vm.getVal(module, RegRef);
                    const B = vm.getVal(module, RegRef);
                    const type_id = vm.getVal(module, Value.TypeId);

                    stack[A].value = if (stack[B].value.?.kind == type_id) &Value.True else &Value.False;
                },
                _ => {
                    return error.MalformedByteCode;
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

    fn getBool(vm: *Vm, module: *lang.Module) !bool {
        // TODO
        const stack = vm.gc.stack.toSlice();
        const val = stack[vm.getVal(module, RegRef)].value.?;

        if (val.kind != .Bool) {
            return vm.reportErr("expected a boolean");
        }
        return val.kind.Bool;
    }

    fn getInt(vm: *Vm, module: *lang.Module) !i64 {
        // TODO
        const stack = vm.gc.stack.toSlice();
        const val = stack[vm.getVal(module, RegRef)].value.?;

        if (val.kind != .Int) {
            return vm.reportErr("expected an integer");
        }
        return val.kind.Int;
    }

    fn getIntRef(vm: *Vm, module: *lang.Module) !*Value {
        // TODO
        const stack = vm.gc.stack.toSlice();
        const val = stack[vm.getVal(module, RegRef)].value.?;

        if (val.kind != .Int) {
            return vm.reportErr("expected an integer");
        }
        return val;
    }

    fn getNumeric(vm: *Vm, module: *lang.Module) !*Value {
        // TODO
        const stack = vm.gc.stack.toSlice();
        const val = stack[vm.getVal(module, RegRef)].value.?;

        if (val.kind != .Int and val.kind != .Num) {
            return vm.reportErr("expected a number");
        }
        return val;
    }

    fn reportErr(vm: *Vm, msg: []const u8) Error {
        try vm.errors.push(.{
            .msg = msg,
            .kind = .Error,
            .index = 0, // TODO debug info
        });
        while (vm.call_stack.pop()) |some| {
            if (vm.call_stack.len == 0) break;
            try vm.errors.push(.{
                .msg = "called here",
                .kind = .Trace,
                .index = 0, // TODO debug info
            });
        }
        return error.RuntimeError;
    }
};
