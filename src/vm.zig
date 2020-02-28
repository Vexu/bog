const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Op = bog.Op;
const Value = bog.Value;
const RegRef = bog.RegRef;
const Module = bog.Module;
const Gc = bog.Gc;
const Errors = bog.Errors;

pub const Vm = struct {
    /// Instruction pointer
    ip: usize,

    /// Stack pointer
    sp: usize,

    call_stack: CallStack,
    gc: Gc,

    repl: bool,

    errors: Errors,

    // TODO come up with better debug info
    line_loc: u32 = 0,

    const CallStack = std.SegmentedList(FunctionFrame, 16);
    const max_depth = 512;

    const FunctionFrame = struct {
        ip: usize,
        sp: usize,
        line_loc: u32,
        ret_loc: *?*Value,
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
            .errors = Errors.init(allocator),
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.call_stack.deinit();
        vm.errors.deinit();
        vm.gc.deinit();
    }

    // TODO rename to step and execute 1 instruction
    pub fn exec(vm: *Vm, module: *Module) Error!?*Value {
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
                    const A_ref = try vm.getRef(module);
                    const val = vm.getArg(module, u8);

                    if (val == 0) {
                        A_ref.* = &Value.None;
                    } else {
                        A_ref.* = if (val == 2) &Value.True else &Value.False;
                    }
                },
                .ConstString => {
                    const A_val = try vm.getNewVal(module);
                    const val = vm.getArg(module, u32);

                    const len = @ptrCast(*align(1) const u32, module.strings[val..].ptr).*;
                    const slice = module.strings[val + @sizeOf(u32) ..][0..len];
                    A_val.* = .{
                        .kind = .{
                            .Str = slice,
                        },
                    };
                },
                .Add => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    // TODO https://github.com/ziglang/zig/issues/3234 on all of these
                    const copy = Value{
                        .kind = if (needNum(B_val, C_val))
                            .{ .Num = asNum(B_val) + asNum(C_val) }
                        else
                            .{ .Int = B_val.kind.Int + C_val.kind.Int },
                    };
                    A_val.* = copy;
                },
                .Sub => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = if (needNum(B_val, C_val))
                            .{ .Num = asNum(B_val) - asNum(C_val) }
                        else
                            .{ .Int = B_val.kind.Int - C_val.kind.Int },
                    };
                    A_val.* = copy;
                },
                .Mul => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = if (needNum(B_val, C_val))
                            .{ .Num = asNum(B_val) * asNum(C_val) }
                        else
                            .{ .Int = B_val.kind.Int * C_val.kind.Int },
                    };
                    A_val.* = copy;
                },
                .Pow => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = if (needNum(B_val, C_val))
                            .{ .Num = std.math.pow(f64, asNum(B_val), asNum(C_val)) }
                        else
                            .{ .Int = std.math.powi(i64, B_val.kind.Int, C_val.kind.Int) catch @panic("TODO: overflow") },
                    };
                    A_val.* = copy;
                },
                .DivFloor => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = if (needNum(B_val, C_val))
                            .{ .Int = @floatToInt(i64, @divFloor(asNum(B_val), asNum(C_val))) }
                        else
                            .{ .Int = @divFloor(B_val.kind.Int, C_val.kind.Int) },
                    };
                    A_val.* = copy;
                },
                .Div => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = .{ .Num = asNum(B_val) / asNum(C_val) },
                    };
                    A_val.* = copy;
                },
                .Mod => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = if (needNum(B_val, C_val))
                            .{ .Num = @rem(asNum(B_val), asNum(C_val)) }
                        else
                            .{ .Int = std.math.rem(i64, B_val.kind.Int, C_val.kind.Int) catch @panic("TODO: overflow") },
                    };
                    A_val.* = copy;
                },
                .BoolAnd => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getBool(module);
                    const C_val = try vm.getBool(module);

                    A_ref.* = if (B_val and C_val) &Value.True else &Value.False;
                },
                .BoolOr => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getBool(module);
                    const C_val = try vm.getBool(module);

                    A_ref.* = if (B_val or C_val) &Value.True else &Value.False;
                },
                .Move => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);

                    A_ref.* = B_val;
                },
                .Copy => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getVal(module);

                    A_val.* = B_val.*;
                },
                .BoolNot => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getBool(module);

                    A_ref.* = if (B_val) &Value.False else &Value.True;
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
                .Equal => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    A_ref.* = if (B_val.eql(C_val)) &Value.True else &Value.False;
                },
                .NotEqual => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    A_ref.* = if (B_val.eql(C_val)) &Value.False else &Value.True;
                },
                .LessThan => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const bool_val = if (needNum(B_val, C_val))
                        asNum(B_val) < asNum(C_val)
                    else
                        B_val.kind.Int < C_val.kind.Int;

                    A_ref.* = if (bool_val) &Value.True else &Value.False;
                },
                .LessThanEqual => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const bool_val = if (needNum(B_val, C_val))
                        asNum(B_val) <= asNum(C_val)
                    else
                        B_val.kind.Int <= C_val.kind.Int;

                    A_ref.* = if (bool_val) &Value.True else &Value.False;
                },
                .GreaterThan => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const bool_val = if (needNum(B_val, C_val))
                        asNum(B_val) > asNum(C_val)
                    else
                        B_val.kind.Int > C_val.kind.Int;

                    A_ref.* = if (bool_val) &Value.True else &Value.False;
                },
                .GreaterThanEqual => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getNum(module);
                    const C_val = try vm.getNum(module);

                    const bool_val = if (needNum(B_val, C_val))
                        asNum(B_val) >= asNum(C_val)
                    else
                        B_val.kind.Int >= C_val.kind.Int;

                    A_ref.* = if (bool_val) &Value.True else &Value.False;
                },
                .In => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    switch (C_val.kind) {
                        .Str, .Tuple, .List, .Map, .Range => {},
                        else => return vm.reportErr("invalid type for 'in'"),
                    }

                    A_ref.* = if (B_val.in(C_val)) &Value.True else &Value.False;
                },
                .LShift => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    if (C_val < 0)
                        return vm.reportErr("shift by negative amount");
                    const val = if (C_val > std.math.maxInt(u6)) 0 else B_val << @intCast(u6, C_val);
                    A_val.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                },
                .RShift => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getInt(module);
                    const C_val = try vm.getInt(module);

                    if (C_val < 0)
                        return vm.reportErr("shift by negative amount");
                    const val = if (C_val > std.math.maxInt(u6)) 0 else B_val >> @intCast(u6, C_val);
                    A_val.* = .{
                        .kind = .{
                            .Int = val,
                        },
                    };
                },
                .Negate => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getNum(module);

                    const copy = Value{
                        .kind = if (B_val.kind == .Num)
                            .{ .Num = -B_val.kind.Num }
                        else
                            .{ .Int = -B_val.kind.Int },
                    };
                    A_val.* = copy;
                },
                .Try => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);

                    if (B_val.kind != .Error) {
                        A_ref.* = B_val;
                        continue;
                    }

                    if (vm.call_stack.len == 0) {
                        // module result
                        return B_val;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    frame.ret_loc.* = try vm.gc.alloc();
                    frame.ret_loc.*.?.* = B_val.*;

                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;
                },
                .JumpFalse => {
                    const A_val = try vm.getBool(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val == false) {
                        vm.ip += addr;
                    }
                },
                .Jump => {
                    const addr = vm.getArg(module, i32);
                    vm.ip = @intCast(usize, @intCast(isize, vm.ip) + addr);
                },
                .JumpTrue => {
                    const A_val = try vm.getBool(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val == true) {
                        vm.ip += addr;
                    }
                },
                .JumpNotError => {
                    const A_val = try vm.getVal(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val.kind != .Error) {
                        vm.ip += addr;
                    }
                },
                .JumpNone => {
                    const A_val = try vm.getVal(module);
                    const addr = vm.getArg(module, u32);

                    if (A_val.kind == .None) {
                        vm.ip += addr;
                    }
                },
                .UnwrapError => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);

                    if (B_val.kind != .Error) return error.MalformedByteCode;
                    A_ref.* = B_val.kind.Error;
                },
                .Import => return vm.reportErr("TODO Op.Import"),
                .Native => return vm.reportErr("TODO Op.Native"),
                .Discard => {
                    const A_val = try vm.getVal(module);

                    if (A_val.kind == .Error) {
                        return vm.reportErr("error discarded");
                    }
                    if (vm.repl and vm.call_stack.len == 0) {
                        return A_val;
                    }
                },
                .BuildTuple => {
                    const A_val = try vm.getNewVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    // TODO gc this
                    const vals = try vm.call_stack.allocator.alloc(*Value, arg_count);
                    var i: u32 = 0;
                    while (i < arg_count) : (i += 1) {
                        vals[i] = vm.gc.stackGet(B + vm.sp + i) catch
                            return error.MalformedByteCode;
                    }

                    A_val.* = .{
                        .kind = .{
                            .Tuple = vals,
                        },
                    };
                },
                .BuildList => {
                    const A_val = try vm.getNewVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    A_val.* = .{
                        .kind = .{
                            // TODO gc this
                            .List = try Value.List.initCapacity(vm.call_stack.allocator, arg_count),
                        },
                    };

                    var i: u32 = 0;
                    while (i < arg_count) : (i += 1) {
                        A_val.kind.List.append(vm.gc.stackGet(B + vm.sp + i) catch
                            return error.MalformedByteCode) catch unreachable;
                    }
                },
                .BuildError => {
                    const A_val = try vm.getNewVal(module);
                    const B_val = try vm.getVal(module);

                    A_val.* = .{
                        .kind = .{
                            .Error = B_val,
                        },
                    };
                },
                .BuildFn => {
                    const A_val = try vm.getNewVal(module);
                    const arg_count = vm.getArg(module, u8);
                    const offset = vm.getArg(module, u32);

                    A_val.* = .{
                        .kind = .{
                            .Fn = .{
                                .arg_count = arg_count,
                                .offset = offset,
                                .module = module,
                            },
                        },
                    };
                },
                .Get => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    A_ref.* = try B_val.get(C_val, vm);
                },
                .Set => {
                    const A_val = try vm.getVal(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    try A_val.set(B_val, C_val, vm);
                },
                .As => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const type_id = vm.getArg(module, Value.TypeId);

                    // Value.as will hit unreachable on invalid type_id
                    switch (type_id) {
                        .None, .Int, .Num, .Bool, .Str, .Tuple, .Map, .List => {},
                        .Error, .Range, .Fn, .Native => return error.MalformedByteCode,
                        _ => return error.MalformedByteCode,
                    }

                    A_ref.* = try B_val.as(type_id, vm);
                },
                .Is => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const type_id = vm.getArg(module, Value.TypeId);

                    A_ref.* = if (B_val.kind == type_id) &Value.True else &Value.False;
                },
                .Call => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const C = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    if (B_val.kind != .Fn) {
                        return vm.reportErr("attempt to call non function type");
                    }

                    if (B_val.kind.Fn.arg_count != arg_count) {
                        // TODO improve this error message to tell the expected and given counts
                        return vm.reportErr("unexpected arg count");
                    }

                    if (vm.call_stack.len > max_depth) {
                        return vm.reportErr("maximum depth exceeded");
                    }

                    try vm.call_stack.push(.{
                        .sp = vm.sp,
                        .ip = vm.ip,
                        .line_loc = vm.line_loc,
                        .ret_loc = A_ref,
                    });
                    vm.sp += C;
                    vm.ip = B_val.kind.Fn.offset;
                },
                .Return => {
                    const A_val = try vm.getVal(module);

                    if (vm.call_stack.len == 0) {
                        // module result
                        return A_val;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    if (A_val.kind != .Bool and A_val.kind != .None) {
                        frame.ret_loc.* = try vm.gc.alloc();
                        frame.ret_loc.*.?.* = A_val.*;
                    } else {
                        frame.ret_loc.* = A_val;
                    }
                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;
                },
                .ReturnNone => {
                    if (vm.call_stack.len == 0) {
                        // module result
                        return &Value.None;
                    }

                    const frame = vm.call_stack.pop() orelse return error.MalformedByteCode;
                    frame.ret_loc.* = &Value.None;
                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;
                },
                .LineInfo => {
                    const line = vm.getArg(module, u32);
                    vm.line_loc = line;
                },
                _ => {
                    return error.MalformedByteCode;
                },
            }
        }
        return null;
    }

    fn getArg(vm: *Vm, module: *Module, comptime T: type) T {
        const val = @ptrCast(*align(1) const T, module.code[vm.ip..].ptr).*;
        vm.ip += @sizeOf(T);
        return val;
    }

    fn getVal(vm: *Vm, module: *Module) !*Value {
        return vm.gc.stackGet(vm.getArg(module, RegRef) + vm.sp) catch
            return error.MalformedByteCode;
    }

    fn getRef(vm: *Vm, module: *Module) !*?*Value {
        return try vm.gc.stackRef(vm.getArg(module, RegRef) + vm.sp);
    }

    fn getNewVal(vm: *Vm, module: *Module) !*Value {
        return vm.gc.stackAlloc(vm.getArg(module, RegRef) + vm.sp) catch
            return error.MalformedByteCode;
    }

    fn getBool(vm: *Vm, module: *Module) !bool {
        const val = try vm.getVal(module);

        if (val.kind != .Bool) {
            return vm.reportErr("expected a boolean");
        }
        return val.kind.Bool;
    }

    fn getInt(vm: *Vm, module: *Module) !i64 {
        const val = try vm.getVal(module);

        if (val.kind != .Int) {
            return vm.reportErr("expected an integer");
        }
        return val.kind.Int;
    }

    fn getIntRef(vm: *Vm, module: *Module) !*Value {
        const val = try vm.getVal(module);

        if (val.kind != .Int) {
            return vm.reportErr("expected an integer");
        }
        return val;
    }

    fn getNum(vm: *Vm, module: *Module) !*Value {
        const val = try vm.getVal(module);

        if (val.kind != .Int and val.kind != .Num) {
            return vm.reportErr("expected a number");
        }
        return val;
    }

    fn needNum(a: *Value, b: *Value) bool {
        return a.kind == .Num or b.kind == .Num;
    }

    fn asNum(val: *Value) f64 {
        return switch (val.kind) {
            .Int => |v| @intToFloat(f64, v),
            .Num => |v| v,
            else => unreachable,
        };
    }

    fn reportErr(vm: *Vm, msg: []const u8) Error {
        try vm.errors.add(msg, vm.line_loc, .Error);
        var i: u8 = 0;
        while (vm.call_stack.pop()) |some| {
            try vm.errors.add("called here", some.line_loc, .Trace);
            i += 1;
            if (i > 32) {
                try vm.errors.add("too many calls, stopping now", some.line_loc, .Note);
                break;
            }
        }
        return error.RuntimeError;
    }
};
