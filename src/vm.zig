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

    errors: Errors,

    // TODO come up with better debug info
    line_loc: u32 = 0,

    /// all currently registered native functions
    native_registry: bog.native.Registry,

    /// all currently loaded packages and files
    imported_modules: std.StringHashMap(*Module),

    allocator: *Allocator,

    options: Options,

    // TODO gc can't see this and it will be invalidated on collect
    this: ?*Value = null,

    const CallStack = std.SegmentedList(FunctionFrame, 16);
    const max_depth = 512;

    pub const Options = struct {
        /// can files be imported
        import_files: bool = false,

        /// Should gc default to using page_allocator
        gc_page_allocator: bool = true,

        /// run vm in repl mode
        repl: bool = false,

        /// maximum size of imported files
        max_import_size: u32 = 1024 * 1024,
    };

    const FunctionFrame = struct {
        ip: usize,
        sp: usize,
        line_loc: u32,
        ret_reg: RegRef,
        module: *Module,
        // TODO gc can't see these and they will be invalidated on collect
        captures: []*Value,
    };

    pub const Error = error{
        RuntimeError,
        MalformedByteCode,
    } || Allocator.Error;

    pub fn init(allocator: *Allocator, options: Options) !Vm {
        return Vm{
            .ip = 0,
            .sp = 0,
            .gc = try Gc.init(if (options.gc_page_allocator) std.heap.page_allocator else allocator),
            .call_stack = CallStack.init(allocator),
            .errors = Errors.init(allocator),
            .options = options,
            .allocator = allocator,
            .native_registry = try bog.native.Registry.init(allocator),
            .imported_modules = std.StringHashMap(*Module).init(allocator),
        };
    }

    pub fn deinit(vm: *Vm) void {
        vm.call_stack.deinit();
        vm.errors.deinit();
        vm.gc.deinit();
        vm.native_registry.deinit();
        var it = vm.imported_modules.iterator();
        while (it.next()) |mod| {
            mod.value.deinit(vm.allocator);
        }
        vm.imported_modules.deinit();
    }

    // TODO rename to step and execute 1 instruction
    pub fn exec(vm: *Vm, mod: *Module) Error!?*Value {
        const start_len = vm.call_stack.len;
        var module = mod;
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
                    const str = try vm.getString(module);

                    A_val.* = .{
                        .kind = .{
                            .Str = str,
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

                    if (vm.call_stack.len == start_len) {
                        if (start_len == 0) {
                            vm.gc.stackShrink(0);
                        }
                        // module result
                        return B_val;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    module = frame.module;

                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;

                    const ret_val = try vm.gc.stackAlloc(vm.sp + frame.ret_reg);
                    ret_val.* = B_val.*;
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
                .IterInit => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);

                    A_ref.* = try B_val.iterator(vm);
                },
                .IterNext => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);

                    if (B_val.kind != .Iterator)
                        return error.MalformedByteCode;

                    try B_val.kind.Iterator.next(vm, A_ref);
                },
                .UnwrapError => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);

                    if (B_val.kind != .Error)
                        return vm.reportErr("expected an error");
                    A_ref.* = B_val.kind.Error;
                },
                .Import => {
                    const A_ref = try vm.getRef(module);
                    const str = try vm.getString(module);

                    A_ref.* = try vm.import(str);
                },
                .BuildNative => {
                    const A_val = try vm.getNewVal(module);
                    const str = try vm.getString(module);

                    A_val.* = .{
                        .kind = .{
                            .Native = vm.native_registry.map.getValue(str) orelse
                                return vm.reportErr("native function not registered"),
                        },
                    };
                },
                .Discard => {
                    const A_val = try vm.getVal(module);

                    if (A_val.kind == .Error) {
                        return vm.reportErr("error discarded");
                    }
                    if (vm.options.repl and vm.call_stack.len == 0) {
                        return A_val;
                    }
                },
                .BuildTuple => {
                    const A_val = try vm.getNewVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    const vals = try vm.allocator.alloc(*Value, arg_count);
                    var i: u32 = 0;
                    while (i < arg_count) : (i += 1) {
                        vals[i] = vm.gc.stackGet(B + vm.sp + i) catch
                            return error.MalformedByteCode;
                    }

                    A_val.* = .{
                        .kind = .{
                            .Tuple = .{
                                .values = vals,
                                .allocator = vm.allocator,
                            },
                        },
                    };
                },
                .BuildList => {
                    const A_val = try vm.getNewVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    A_val.* = .{
                        .kind = .{
                            .List = try Value.List.initCapacity(vm.allocator, arg_count),
                        },
                    };

                    var i: u32 = 0;
                    while (i < arg_count) : (i += 1) {
                        A_val.kind.List.append(vm.gc.stackGet(B + vm.sp + i) catch
                            return error.MalformedByteCode) catch unreachable;
                    }
                },
                .BuildMap => {
                    const A_val = try vm.getNewVal(module);
                    const B = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    if (arg_count & 1 != 0) return error.MalformedByteCode;
                    A_val.* = .{
                        .kind = .{
                            .Map = Value.Map.init(vm.allocator),
                        },
                    };

                    const map = &A_val.kind.Map;
                    try map.ensureCapacity(arg_count);

                    // TODO maps lists and tuples need to be initialized differently or
                    // we'll quickly run out of registers.
                    var i: u32 = 0;
                    while (i < arg_count) : (i += 2) {
                        const key = vm.gc.stackGet(B + vm.sp + i) catch
                            return error.MalformedByteCode;
                        const val = vm.gc.stackGet(B + vm.sp + i + 1) catch
                            return error.MalformedByteCode;
                        _ = try map.put(key, val);
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
                    const captures = vm.getArg(module, u8);
                    const offset = vm.getArg(module, u32);

                    A_val.* = .{
                        .kind = .{
                            .Fn = .{
                                .arg_count = arg_count,
                                .offset = offset,
                                .module = module,
                                .allocator = vm.allocator,
                                .captures = try vm.allocator.alloc(*Value, captures),
                            },
                        },
                    };
                },
                .Get => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    try B_val.get(vm, C_val, A_ref);

                    // set this on load
                    vm.this = B_val;
                },
                .Set => {
                    const A_val = try vm.getVal(module);
                    const B_val = try vm.getVal(module);
                    const C_val = try vm.getVal(module);

                    try A_val.set(vm, B_val, C_val);
                },
                .As => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const type_id = vm.getArg(module, Value.TypeId);

                    // Value.as will hit unreachable on invalid type_id
                    switch (type_id) {
                        .None, .Int, .Num, .Bool, .Str, .Tuple, .Map, .List => {},
                        .Error, .Range, .Fn, .Native, .Iterator => return error.MalformedByteCode,
                        _ => return error.MalformedByteCode,
                    }

                    A_ref.* = try B_val.as(vm, type_id);
                },
                .Is => {
                    const A_ref = try vm.getRef(module);
                    const B_val = try vm.getVal(module);
                    const type_id = vm.getArg(module, Value.TypeId);

                    switch (type_id) {
                        .None, .Int, .Num, .Bool, .Str, .Tuple, .Map, .List, .Error, .Range, .Fn => {},
                        .Iterator, .Native => return error.MalformedByteCode,
                        _ => return error.MalformedByteCode,
                    }

                    A_ref.* = if (B_val.is(type_id)) &Value.True else &Value.False;
                },
                .Call => {
                    const A = vm.getArg(module, RegRef);
                    const B_val = try vm.getVal(module);
                    const C = vm.getArg(module, RegRef);
                    const arg_count = vm.getArg(module, u16);

                    if (B_val.kind == .Native) {
                        const native = B_val.kind.Native;
                        // TODO see note comment in native.zig
                        // if (native.arg_count != arg_count) {
                        //     // TODO improve this error message to tell the expected and given counts
                        //     return vm.reportErr("unexpected arg count");
                        // }

                        const args = vm.gc.stack.span()[vm.sp + C ..][0..arg_count];
                        for (args) |arg| {
                            if (arg == null)
                                return error.MalformedByteCode;
                        }

                        const ret_val = try native.func(vm, @bitCast([]*Value, args));
                        const ret_ref = try vm.gc.stackRef(vm.sp + A);
                        ret_ref.* = ret_val;
                        continue;
                    }

                    if (B_val.kind != .Fn) {
                        return vm.reportErr("attempt to call non function type");
                    }
                    const func = B_val.kind.Fn;

                    if (func.arg_count != arg_count) {
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
                        .ret_reg = A,
                        .module = mod,
                        .captures = func.captures,
                    });
                    vm.sp += C;
                    vm.ip = func.offset;
                    module = func.module;
                },
                .Return => {
                    const A_val = try vm.getVal(module);

                    if (vm.call_stack.len == start_len) {
                        if (start_len == 0) {
                            vm.gc.stackShrink(0);
                        }
                        // module result
                        return A_val;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    module = frame.module;
                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;

                    const ret_val = try vm.gc.stackAlloc(vm.sp + frame.ret_reg);
                    ret_val.* = A_val.*;
                },
                .ReturnNone => {
                    if (vm.call_stack.len == start_len) {
                        if (start_len == 0) {
                            vm.gc.stackShrink(0);
                        }
                        // module result
                        return &Value.None;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    module = frame.module;
                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;

                    const ret_val = try vm.gc.stackRef(vm.sp + frame.ret_reg);
                    ret_val.* = &Value.None;
                },
                .LoadCapture => {
                    const A_ref = try vm.getRef(module);
                    const n = vm.getArg(module, u8);

                    const frame = vm.call_stack.at(vm.call_stack.len - 1);
                    if (n >= frame.captures.len) return error.MalformedByteCode;

                    A_ref.* = frame.captures[n];
                },
                .StoreCapture => {
                    const A_val = try vm.getVal(module);
                    const B_val = try vm.getVal(module);
                    const n = vm.getArg(module, u8);

                    if (A_val.kind != .Fn) return error.MalformedByteCode;
                    const func = A_val.kind.Fn;
                    if (n >= func.captures.len) return error.MalformedByteCode;

                    func.captures[n] = B_val;
                },
                .LoadThis => {
                    const A_ref = try vm.getRef(module);

                    A_ref.* = vm.this orelse
                        return vm.reportErr("this has not been set");
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
        return &Value.None;
    }

    fn import(vm: *Vm, id: []const u8) !*Value {
        var mod = vm.imported_modules.getValue(id) orelse if (mem.endsWith(u8, id, bog.extension)) blk: {
            if (!vm.options.import_files) {
                return vm.reportErr("import failed");
            }
            const source = std.fs.cwd().readFileAlloc(vm.allocator, id, vm.options.max_import_size) catch |e| switch (e) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return vm.reportErr("import failed"),
            };
            defer vm.allocator.free(source);
            const mod = bog.compile(vm.allocator, source, &vm.errors) catch
                return vm.reportErr("import failed");
            mod.name = try mem.dupe(vm.allocator, u8, id);
            _ = try vm.imported_modules.put(id, mod);
            break :blk mod;
        } else if (mem.endsWith(u8, id, bog.bytecode_extension)) blk: {
            if (!vm.options.import_files) {
                return vm.reportErr("import failed");
            }
            const source = std.fs.cwd().readFileAlloc(vm.allocator, id, vm.options.max_import_size) catch |e| switch (e) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return vm.reportErr("import failed"),
            };
            defer vm.allocator.free(source);
            const read_module = Module.read(source) catch
                return vm.reportErr("import failed");

            const mod = try vm.allocator.create(Module);
            mod.* = .{
                .name = try mem.dupe(vm.allocator, u8, id),
                .code = try mem.dupe(vm.allocator, u8, read_module.code),
                .strings = try mem.dupe(vm.allocator, u8, read_module.strings),
                .entry = read_module.entry,
            };
            _ = try vm.imported_modules.put(id, mod);
            break :blk mod;
        } else {
            return vm.reportErr("no such package");
        };

        const saved_sp = vm.sp;
        const saved_ip = vm.ip;
        const saved_line_loc = vm.line_loc;
        const saved_stack_len = vm.gc.stack.items.len;
        vm.sp = vm.gc.stack.items.len;

        vm.ip = mod.entry;
        const res = try vm.exec(mod);

        vm.gc.stackShrink(saved_stack_len);
        vm.ip = saved_ip;
        vm.sp = saved_sp;
        vm.line_loc = saved_line_loc;
        return res orelse &Value.None;
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
        return try vm.gc.stackAlloc(vm.getArg(module, RegRef) + vm.sp);
    }

    fn getString(vm: *Vm, module: *Module) ![]const u8 {
        const offset = vm.getArg(module, u32);

        const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
        return module.strings[offset + @sizeOf(u32) ..][0..len];
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
        @setCold(true);
        try vm.errors.add(msg, vm.line_loc, .err);
        var i: u8 = 0;
        while (vm.call_stack.pop()) |some| {
            try vm.errors.add("called here", some.line_loc, .trace);
            i += 1;
            if (i > 32) {
                try vm.errors.add("too many calls, stopping now", some.line_loc, .note);
                break;
            }
        }
        return error.RuntimeError;
    }
};
