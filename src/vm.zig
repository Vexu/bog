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

    /// Stack pointer
    sp: usize,

    call_stack: CallStack,
    gc: Gc,

    repl: bool,
    result: ?Ref = null,

    errors: lang.Error.List,

    const CallStack = std.SegmentedList(FunctionFrame, 16);

    const FunctionFrame = struct {
        ip: usize,
        sp: usize,
    };

    pub const Error = error{
        RuntimeError,
        MalformedByteCode,
    } || Allocator.Error;

    pub fn init(allocator: *Allocator, repl: bool) Vm {
        return Vm{
            .ip = 0,
            .sp = 0,
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
        while (vm.ip < module.code.len) {
            const op = @intToEnum(Op, vm.getArg(module, u8));
            switch (op) {
                .ConstInt8 => {
                    const A_val = try vm.getNewVal(module);
                    const val = vm.getArg(module, i8);

                    A_val.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                },
                .ConstInt32 => {
                    const A_val = try vm.getNewVal(module);
                    const val = vm.getArg(module, i32);

                    A_val.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                },
                .ConstInt64 => {
                    const A_val = try vm.getNewVal(module);
                    const val = vm.getArg(module, i64);

                    A_val.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                },
                .ConstNum => {
                    const A_val = try vm.getNewVal(module);
                    const val = vm.getArg(module, f64);

                    A_val.* = .{
                        .kind = .{
                            .Num = val,
                        },
                    };
                },
                .ConstPrimitive => {
                    const A_ref = vm.getRef(module);
                    const val = vm.getArg(module, u8);

                    if (val == 0) {
                        A_ref.value = &Value.None;
                    } else {
                        A_ref.value = if (val == 2) &Value.True else &Value.False;
                    }
                },
                .ConstString => return vm.reportErr("TODO Op.ConstString"),
                .Add => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val.kind.Int + C_val.kind.Int,
                        },
                    };
                },
                .Sub => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val.kind.Int - C_val.kind.Int,
                        },
                    };
                },
                .Mul => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val.kind.Int * C_val.kind.Int,
                        },
                    };
                },
                .Pow => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = std.math.powi(i64, B_val.kind.Int, C_val.kind.Int) catch @panic("TODO: overflow"),
                        },
                    };
                },
                .DivFloor => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNumeric(module);
                    const C_val = try vm.getNumeric(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = @divFloor(B_val.kind.Int, C_val.kind.Int),
                        },
                    };
                },
                .Div => return vm.reportErr("TODO Op.Div"),
                .Mod => return vm.reportErr("TODO Op.Mod"),
                .And => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getBool(module);
                    const C_val = try vm.getBool(module);

                    A_val.* = .{
                        .kind = .{
                            .Bool = B_val and C_val,
                        },
                    };
                },
                .Or => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getBool(module);
                    const C_val = try vm.getBool(module);

                    A_val.* = .{
                        .kind = .{
                            .Bool = B_val or C_val,
                        },
                    };
                },
                .Move => {
                    const A_ref = vm.getRef(module);
                    const B_ref = vm.getRef(module);

                    A_ref.* = B_ref.*;
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
                    const A_ref = vm.getRef(module);
                    const B_val = try vm.getBool(module);

                    A_ref.value = if (B_val) &Value.False else &Value.True;
                },
                .BitNot => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = ~B_val,
                        },
                    };
                },
                .BitAnd => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val & C_val,
                        },
                    };
                },
                .BitOr => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val | C_val,
                        },
                    };
                },
                .BitXor => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val ^ C_val,
                        },
                    };
                },
                .LShift => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val << @intCast(u6, B_val),
                        },
                    };
                },
                .RShift => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = B_val >> @intCast(u6, B_val),
                        },
                    };
                },
                .Negate => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNumeric(module);

                    A_val.* = .{
                        .kind = .{
                            .Int = -B_val.kind.Int,
                        },
                    };
                },
                .Try => return vm.reportErr("TODO Op.Try"),
                .JumpFalse => {
                    const A_val = try vm.getBool(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val == false) {
                        vm.ip += addr;
                    }
                },
                .Jump => {
                    const addr = vm.getArg(module, u32);
                    vm.ip += addr;
                },
                .JumpTrue => {
                    const A_val = try vm.getBool(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val == true) {
                        vm.ip += addr;
                    }
                },
                .JumpNotError => {
                    const A_val = vm.getVal(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val.kind != .Error) {
                        vm.ip += addr;
                    }
                },
                .Import => return vm.reportErr("TODO Op.Import"),
                .Discard => {
                    const A_ref = vm.getRef(module);
                    if (vm.repl and vm.call_stack.len == 0) {
                        vm.result = A_ref.*;
                    } else {
                        if (A_ref.value.?.kind == .Error) {
                            return vm.reportErr("error discarded");
                        }
                    }
                },
                .BuildTuple => {
                    const A_val = try vm.getNewVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    // TODO
                    const stack = vm.gc.stack.toSlice();

                    // TODO gc this
                    const vals = try vm.call_stack.allocator.alloc(Ref, arg_count);
                    var i: u32 = 0;
                    while (i < arg_count) : (i += 1) {
                        vals[i] = stack[B + vm.sp + i];
                    }

                    A_val.* = .{
                        .kind = .{
                            .Tuple = vals,
                        },
                    };
                },
                .BuildError => return vm.reportErr("TODO Op.BuildError"),
                .BuildFn => {
                    const A_val = try vm.getNewVal(module);
                    const arg_count = vm.getArg(module, u8);
                    const offset = vm.getArg(module, u32);

                    A_val.* = .{
                        .kind = .{
                            .Fn = .{
                                .arg_count = arg_count,
                                .offset = offset,
                            },
                        },
                    };
                },
                .Subscript => {
                    const A_ref = vm.getRef(module);
                    const B_val = vm.getVal(module);
                    const C_val = vm.getVal(module);

                    A_ref.* = switch (B_val.kind) {
                        .Tuple => |val| val[@intCast(u32, C_val.kind.Int)],
                        else => return vm.reportErr("TODO: subscript for more types"),
                    };
                },
                .As => {
                    const A_ref = vm.getRef(module);
                    const B_val = vm.getVal(module);
                    const type_id = vm.getArg(module, Value.TypeId);

                    if (type_id == .None) {
                        A_ref.value = &Value.None;
                        continue;
                    }

                    if (type_id == .Bool) {
                        const bool_res = switch (B_val.kind) {
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

                        A_ref.value = if (bool_res) &Value.True else &Value.False;
                        continue;
                    }

                    const ref = try vm.gc.alloc();
                    ref.value.?.* = switch (type_id) {
                        .Bool, .None => unreachable,
                        .Int => .{
                            .kind = .{
                                .Int = switch (B_val.kind) {
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
                                .Num = switch (B_val.kind) {
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
                    A_ref.* = ref;
                },
                .Is => {
                    const A_ref = vm.getRef(module);
                    const B_val = vm.getVal(module);
                    const type_id = vm.getArg(module, Value.TypeId);

                    A_ref.value = if (B_val.kind == type_id) &Value.True else &Value.False;
                },
                .Call => {
                    const A_val = vm.getVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    if (A_val.kind != .Fn) {
                        return vm.reportErr("attempt to call non function type");
                    }

                    if (A_val.kind.Fn.arg_count != arg_count) {
                        // TODO improve this error message to tell the expected and given counts
                        return vm.reportErr("unexpecte arg count");
                    }

                    try vm.call_stack.push(.{
                        .sp = vm.sp,
                        .ip = vm.ip,
                    });
                    vm.sp += B;
                    vm.ip = A_val.kind.Fn.offset;
                },
                .Return => {
                    const A_val = vm.getVal(module);

                    // TODO
                    const stack = vm.gc.stack.toSlice();
                    stack[vm.sp].value = A_val;

                    const frame = vm.call_stack.pop() orelse return error.MalformedByteCode;
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                },
                .ReturnNone => {
                    // TODO
                    const stack = vm.gc.stack.toSlice();
                    stack[vm.sp].value = &Value.None;

                    const frame = vm.call_stack.pop() orelse return error.MalformedByteCode;
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                },
                _ => {
                    return error.MalformedByteCode;
                },
            }
        }
    }

    fn getArg(vm: *Vm, module: *lang.Module, comptime T: type) T {
        if (T == []align(1) const RegRef) {
            const len = vm.getArg(module, u16);
            const val = @ptrCast([*]align(1) const RegRef, module.code[vm.ip..].ptr);
            vm.ip += @sizeOf(RegRef) * len;
            return val[0..len];
        }
        const val = @ptrCast(*align(1) const T, module.code[vm.ip..].ptr).*;
        vm.ip += @sizeOf(T);
        return val;
    }

    fn getVal(vm: *Vm, module: *lang.Module) *Value {
        // TODO
        const stack = vm.gc.stack.toSlice();
        return stack[vm.getArg(module, RegRef) + vm.sp].value.?;
    }

    fn getRef(vm: *Vm, module: *lang.Module) *Ref {
        // TODO
        const stack = vm.gc.stack.toSlice();
        return &stack[vm.getArg(module, RegRef) + vm.sp];
    }

    fn getNewVal(vm: *Vm, module: *lang.Module) !*Value {
        // TODO
        const stack = vm.gc.stack.toSlice();
        const ref = try vm.gc.alloc();
        stack[vm.getArg(module, RegRef) + vm.sp] = ref;
        return ref.value.?;
    }

    fn getBool(vm: *Vm, module: *lang.Module) !bool {
        const val = vm.getVal(module);

        if (val.kind != .Bool) {
            return vm.reportErr("expected a boolean");
        }
        return val.kind.Bool;
    }

    fn getInt(vm: *Vm, module: *lang.Module) !i64 {
        const val = vm.getVal(module);

        if (val.kind != .Int) {
            return vm.reportErr("expected an integer");
        }
        return val.kind.Int;
    }

    fn getIntRef(vm: *Vm, module: *lang.Module) !*Value {
        const val = vm.getVal(module);

        if (val.kind != .Int) {
            return vm.reportErr("expected an integer");
        }
        return val;
    }

    fn getNumeric(vm: *Vm, module: *lang.Module) !*Value {
        const val = vm.getVal(module);

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
            try vm.errors.push(.{
                .msg = "called here",
                .kind = .Trace,
                .index = 0, // TODO debug info
            });
        }
        return error.RuntimeError;
    }
};
