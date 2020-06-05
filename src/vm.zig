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

const max_params = @import("compiler.zig").max_params;

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

    options: Options,

    // TODO gc can't see this and it will be invalidated on collect
    last_get: ?*Value = null,

    const CallStack = std.SegmentedList(FunctionFrame, 16);
    const max_depth = 512;

    pub const Options = struct {
        /// can files be imported
        import_files: bool = false,

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
        // this points to the Fn values captures so the gc can see them
        captures: []*Value,

        // TODO gc can't see this and it will be invalidated on collect
        this: ?*Value = null,
    };

    pub const Error = error{
        RuntimeError,
        MalformedByteCode,
    } || Allocator.Error;

    pub fn init(allocator: *Allocator, options: Options) Vm {
        return .{
            .ip = 0,
            .sp = 0,
            .gc = Gc.init(allocator),
            .call_stack = CallStack.init(allocator),
            .errors = Errors.init(allocator),
            .options = options,
            .native_registry = bog.native.Registry.init(allocator),
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
            mod.value.deinit(vm.gc.gpa);
        }
        vm.imported_modules.deinit();
    }

    /// Compiles and executes `source`.
    pub fn run(vm: *Vm, source: []const u8) !*bog.Value {
        var module = try bog.compile(vm.gc.gpa, source, &vm.errors);
        defer module.deinit(vm.gc.gpa);

        vm.ip = module.entry;
        return try vm.exec(module);
    }

    /// Continues execution from current instruction pointer.
    pub fn exec(vm: *Vm, mod: *Module) Error!*Value {
        const start_len = vm.call_stack.len;
        var module = mod;
        while (vm.ip < module.code.len) {
            const inst = module.code[vm.ip];
            vm.ip += 1;

            switch (inst.op.op) {
                .const_int => {
                    const res = try vm.getNewVal(module, inst.int.res);

                    res.* = .{
                        .int = if (inst.int.long) try vm.getLong(module, i64) else inst.int.arg,
                    };
                },
                .const_num => {
                    const res = try vm.getNewVal(module, inst.single.arg);

                    res.* = .{
                        .num = try vm.getLong(module, f64),
                    };
                },
                .const_primitive => {
                    const res = try vm.getRef(module, inst.primitive.res);

                    res.* = switch (inst.primitive.kind) {
                        .none => &Value.None,
                        .True => &Value.True,
                        .False => &Value.False,
                        _ => return error.MalformedByteCode,
                    };
                },
                .const_string_off => {
                    const res = try vm.getNewVal(module, inst.off.res);
                    const str = try vm.getString(module, inst);

                    res.* = .{
                        .str = str,
                    };
                },
                .add_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    // TODO https://github.com/ziglang/zig/issues/3234 on all of these
                    const copy: Value = if (needNum(lhs, rhs))
                        .{ .num = asNum(lhs) + asNum(rhs) }
                    else
                        .{ .int = lhs.int + rhs.int };
                    res.* = copy;
                },
                .sub_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const copy: Value = if (needNum(lhs, rhs))
                        .{ .num = asNum(lhs) - asNum(rhs) }
                    else
                        .{ .int = lhs.int - rhs.int };
                    res.* = copy;
                },
                .mul_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const copy: Value = if (needNum(lhs, rhs))
                        .{ .num = asNum(lhs) * asNum(rhs) }
                    else
                        .{ .int = lhs.int * rhs.int };
                    res.* = copy;
                },
                .pow_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const copy: Value = if (needNum(lhs, rhs))
                        .{ .num = std.math.pow(f64, asNum(lhs), asNum(rhs)) }
                    else
                        .{ .int = std.math.powi(i64, lhs.int, rhs.int) catch @panic("TODO: overflow") };
                    res.* = copy;
                },
                .div_floor_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const copy: Value = if (needNum(lhs, rhs))
                        .{ .int = @floatToInt(i64, @divFloor(asNum(lhs), asNum(rhs))) }
                    else
                        .{ .int = @divFloor(lhs.int, rhs.int) };
                    res.* = copy;
                },
                .div_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const copy = Value{ .num = asNum(lhs) / asNum(rhs) };
                    res.* = copy;
                },
                .mod_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const copy: Value = if (needNum(lhs, rhs))
                        .{ .num = @rem(asNum(lhs), asNum(rhs)) }
                    else
                        .{ .int = std.math.rem(i64, lhs.int, rhs.int) catch @panic("TODO: overflow") };
                    res.* = copy;
                },
                .bool_and_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getBool(module, inst.triple.lhs);
                    const rhs = try vm.getBool(module, inst.triple.rhs);

                    res.* = if (lhs and rhs) &Value.True else &Value.False;
                },
                .bool_or_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getBool(module, inst.triple.lhs);
                    const rhs = try vm.getBool(module, inst.triple.rhs);

                    res.* = if (lhs or rhs) &Value.True else &Value.False;
                },
                .move_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    res.* = arg;
                },
                .copy_double => {
                    const res = try vm.getNewVal(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    res.* = arg.*;
                },
                .bool_not_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = try vm.getBool(module, inst.double.arg);

                    res.* = if (arg) &Value.False else &Value.True;
                },
                .bit_not_double => {
                    const res = try vm.getNewVal(module, inst.double.res);
                    const arg = try vm.getInt(module, inst.double.arg);

                    res.* = .{
                        .int = ~arg,
                    };
                },
                .bit_and_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getInt(module, inst.triple.lhs);
                    const rhs = try vm.getInt(module, inst.triple.rhs);

                    res.* = .{
                        .int = lhs & rhs,
                    };
                },
                .bit_or_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getInt(module, inst.triple.lhs);
                    const rhs = try vm.getInt(module, inst.triple.rhs);

                    res.* = .{
                        .int = lhs | rhs,
                    };
                },
                .bit_xor_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getInt(module, inst.triple.lhs);
                    const rhs = try vm.getInt(module, inst.triple.rhs);

                    res.* = .{
                        .int = lhs ^ rhs,
                    };
                },
                .equal_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getVal(module, inst.triple.lhs);
                    const rhs = try vm.getVal(module, inst.triple.rhs);

                    res.* = if (lhs.eql(rhs)) &Value.True else &Value.False;
                },
                .not_equal_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getVal(module, inst.triple.lhs);
                    const rhs = try vm.getVal(module, inst.triple.rhs);

                    res.* = if (lhs.eql(rhs)) &Value.False else &Value.True;
                },
                .less_than_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const bool_val = if (needNum(lhs, rhs))
                        asNum(lhs) < asNum(rhs)
                    else
                        lhs.int < rhs.int;

                    res.* = if (bool_val) &Value.True else &Value.False;
                },
                .less_than_equal_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const bool_val = if (needNum(lhs, rhs))
                        asNum(lhs) <= asNum(rhs)
                    else
                        lhs.int <= rhs.int;

                    res.* = if (bool_val) &Value.True else &Value.False;
                },
                .greater_than_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const bool_val = if (needNum(lhs, rhs))
                        asNum(lhs) > asNum(rhs)
                    else
                        lhs.int > rhs.int;

                    res.* = if (bool_val) &Value.True else &Value.False;
                },
                .greater_than_equal_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getNum(module, inst.triple.lhs);
                    const rhs = try vm.getNum(module, inst.triple.rhs);

                    const bool_val = if (needNum(lhs, rhs))
                        asNum(lhs) >= asNum(rhs)
                    else
                        lhs.int >= rhs.int;

                    res.* = if (bool_val) &Value.True else &Value.False;
                },
                .in_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const lhs = try vm.getVal(module, inst.triple.lhs);
                    const rhs = try vm.getVal(module, inst.triple.rhs);

                    switch (rhs.*) {
                        .str, .tuple, .list, .map, .range => {},
                        else => return vm.reportErr("invalid type for 'in'"),
                    }

                    res.* = if (lhs.in(rhs)) &Value.True else &Value.False;
                },
                .l_shift_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getInt(module, inst.triple.lhs);
                    const rhs = try vm.getInt(module, inst.triple.rhs);

                    if (rhs < 0)
                        return vm.reportErr("shift by negative amount");
                    const val = if (rhs > std.math.maxInt(u6)) 0 else lhs << @intCast(u6, rhs);
                    res.* = .{
                        .int = val,
                    };
                },
                .r_shift_triple => {
                    const res = try vm.getNewVal(module, inst.triple.res);
                    const lhs = try vm.getInt(module, inst.triple.lhs);
                    const rhs = try vm.getInt(module, inst.triple.rhs);

                    if (rhs < 0)
                        return vm.reportErr("shift by negative amount");
                    const val = if (rhs > std.math.maxInt(u6)) 0 else lhs >> @intCast(u6, rhs);
                    res.* = .{
                        .int = val,
                    };
                },
                .negate_double => {
                    const res = try vm.getNewVal(module, inst.double.res);
                    const arg = try vm.getNum(module, inst.double.arg);

                    const copy: Value = if (arg.* == .num)
                        .{ .num = -arg.num }
                    else
                        .{ .int = -arg.int };
                    res.* = copy;
                },
                .try_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    if (arg.* != .err) {
                        res.* = arg;
                        continue;
                    }

                    if (vm.call_stack.len == start_len) {
                        if (start_len == 0) {
                            vm.gc.stackShrink(0);
                        }
                        // module result
                        return arg;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    module = frame.module;

                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;

                    const ret_val = try vm.gc.stackAlloc(vm.sp + frame.ret_reg);
                    ret_val.* = arg.*;
                },
                .jump => {
                    const base = vm.ip;
                    const offset = @bitCast(i32, try vm.getSingle(module));
                    vm.ip = @intCast(usize, @intCast(isize, base) + offset);
                },
                .jump_false => {
                    const base = vm.ip;
                    const arg = try vm.getBool(module, inst.jump.arg);
                    const offset = try vm.getSingle(module);

                    if (arg == false) {
                        vm.ip = base + offset;
                    }
                },
                .jump_true => {
                    const base = vm.ip;
                    const arg = try vm.getBool(module, inst.jump.arg);
                    const offset = try vm.getSingle(module);

                    if (arg == true) {
                        vm.ip = base + offset;
                    }
                },
                .jump_not_error => {
                    const base = vm.ip;
                    const arg = try vm.getVal(module, inst.jump.arg);
                    const offset = try vm.getSingle(module);

                    if (arg.* != .err) {
                        vm.ip = base + offset;
                    }
                },
                .jump_none => {
                    const base = vm.ip;
                    const arg = try vm.getVal(module, inst.jump.arg);
                    const offset = try vm.getSingle(module);

                    if (arg.* == .none) {
                        vm.ip = base + offset;
                    }
                },
                .iter_init_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    res.* = try Value.iterator(arg, vm);
                },
                .iter_next_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    if (arg.* != .iterator)
                        return error.MalformedByteCode;

                    try arg.iterator.next(vm, res);
                },
                .unwrap_error_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    if (arg.* != .err)
                        return vm.reportErr("expected an error");
                    res.* = arg.err;
                },
                .import_off => {
                    const res = try vm.getRef(module, inst.off.res);
                    const str = try vm.getString(module, inst);

                    res.* = try vm.import(str);
                },
                .discard_single => {
                    const arg = try vm.getVal(module, inst.single.arg);

                    if (arg.* == .err) {
                        return vm.reportErr("error discarded");
                    }
                    if (vm.options.repl and vm.call_stack.len == 0) {
                        return arg;
                    }
                },
                .build_native_off => {
                    const res = try vm.getNewVal(module, inst.off.res);
                    const str = try vm.getString(module, inst);

                    res.* = .{
                        .native = vm.native_registry.map.getValue(str) orelse
                            return vm.reportErr("native function not registered"),
                    };
                },
                .build_tuple_off => {
                    const res = try vm.getNewVal(module, inst.off.res);
                    const size = if (inst.off.isArg())
                        try vm.getSingle(module)
                    else
                        inst.off.off;

                    res.* = .{
                        .tuple = .{
                            .values = try vm.gc.gpa.alloc(*Value, size),
                            .allocator = vm.gc.gpa,
                        },
                    };

                    // initialize to nones
                    for (res.tuple.values) |*item| {
                        item.* = try vm.gc.alloc();
                        item.*.* = .{ .none = {} };
                    }
                },
                .build_list_off => {
                    const res = try vm.getNewVal(module, inst.off.res);
                    const size = if (inst.off.isArg())
                        try vm.getSingle(module)
                    else
                        inst.off.off;

                    res.* = .{
                        .list = Value.List.init(vm.gc.gpa),
                    };
                    try res.list.resize(size);

                    // initialize to nones
                    for (res.list.items) |*item| {
                        item.* = try vm.gc.alloc();
                        item.*.* = .{ .none = {} };
                    }
                },
                .build_map_off => {
                    const res = try vm.getNewVal(module, inst.off.res);
                    const size = if (inst.off.isArg())
                        try vm.getSingle(module)
                    else
                        inst.off.off;

                    res.* = .{
                        .map = Value.Map.init(vm.gc.gpa),
                    };
                    try res.map.ensureCapacity(size);
                },
                .build_error_double => {
                    const res = try vm.getNewVal(module, inst.double.res);
                    const arg = try vm.getVal(module, inst.double.arg);

                    res.* = .{
                        .err = arg,
                    };
                },
                .build_func => {
                    const res = try vm.getNewVal(module, inst.func.res);
                    if (inst.func.arg_count > max_params)
                        return error.MalformedByteCode;

                    res.* = .{
                        .func = .{
                            .arg_count = inst.func.arg_count,
                            .offset = try vm.getSingle(module),
                            .module = module,
                            .allocator = vm.gc.gpa,
                            .captures = try vm.gc.gpa.alloc(*Value, inst.func.capture_count),
                        },
                    };
                },
                .get_triple => {
                    const res = try vm.getRef(module, inst.triple.res);
                    const container = try vm.getVal(module, inst.triple.lhs);
                    const index = try vm.getVal(module, inst.triple.rhs);

                    try container.get(vm, index, res);

                    // this will become the value of `this` for the next function call
                    vm.last_get = container;
                },
                .set_triple => {
                    const container = try vm.getVal(module, inst.triple.res);
                    const index = try vm.getVal(module, inst.triple.lhs);
                    const val = try vm.getVal(module, inst.triple.rhs);

                    try container.set(vm, index, val);
                },
                .as_type_id => {
                    const res = try vm.getRef(module, inst.type_id.res);
                    const arg = try vm.getVal(module, inst.type_id.arg);

                    // Value.as will hit unreachable on invalid type_id
                    switch (inst.type_id.type_id) {
                        .none, .int, .num, .bool, .str, .tuple, .map, .list => {},
                        .err, .range, .func, .native, .iterator => return error.MalformedByteCode,
                        _ => return error.MalformedByteCode,
                    }

                    res.* = try arg.as(vm, inst.type_id.type_id);
                },
                .is_type_id => {
                    const res = try vm.getRef(module, inst.type_id.res);
                    const arg = try vm.getVal(module, inst.type_id.arg);

                    switch (inst.type_id.type_id) {
                        .none, .int, .num, .bool, .str, .tuple, .map, .list, .err, .range, .func => {},
                        .iterator, .native => return error.MalformedByteCode,
                        _ => return error.MalformedByteCode,
                    }

                    res.* = if (arg.is(inst.type_id.type_id)) &Value.True else &Value.False;
                },
                .call => {
                    const res = inst.call.res;
                    const func = try vm.getVal(module, inst.call.func);
                    const first = inst.call.first;
                    const arg_count = try vm.getSingle(module);
                    if (arg_count > max_params)
                        return error.MalformedByteCode;

                    if (func.* == .native) {
                        // TODO see note comment in native.zig
                        // if (func.native.arg_count != arg_count) {
                        //     // TODO improve this error message to tell the expected and given counts
                        //     return vm.reportErr("unexpected arg count");
                        // }
                        const args = vm.gc.stack.items[vm.sp + first ..][0..arg_count];
                        for (args) |arg| {
                            if (arg == null)
                                return error.MalformedByteCode;
                        }

                        const ret_val = try func.native.func(vm, @bitCast([]*Value, args));
                        const ret_ref = try vm.gc.stackRef(vm.sp + res);
                        ret_ref.* = ret_val;
                        continue;
                    }

                    if (func.* != .func) {
                        return vm.reportErr("attempt to call non function type");
                    }

                    if (func.func.arg_count != arg_count) {
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
                        .ret_reg = res,
                        .module = mod,
                        .captures = func.func.captures,
                        .this = vm.last_get,
                    });
                    vm.sp += first;
                    vm.ip = func.func.offset;
                    module = func.func.module;
                },
                .return_single => {
                    const arg = try vm.getVal(module, inst.single.arg);

                    if (vm.call_stack.len == start_len) {
                        if (start_len == 0) {
                            vm.gc.stackShrink(0);
                        }
                        // module result
                        return arg;
                    }

                    const frame = vm.call_stack.pop() orelse unreachable;
                    module = frame.module;
                    vm.gc.stackShrink(vm.sp);
                    vm.ip = frame.ip;
                    vm.sp = frame.sp;
                    vm.line_loc = frame.line_loc;

                    const ret_val = try vm.gc.stackAlloc(vm.sp + frame.ret_reg);
                    ret_val.* = arg.*;
                },
                .return_none => {
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
                .load_capture_double => {
                    const res = try vm.getRef(module, inst.double.res);
                    const arg = inst.double.arg;

                    const frame = vm.call_stack.at(vm.call_stack.len - 1);
                    if (arg >= frame.captures.len) return error.MalformedByteCode;

                    res.* = frame.captures[arg];
                },
                .store_capture_triple => {
                    const res = try vm.getVal(module, inst.triple.res);
                    const lhs = try vm.getVal(module, inst.triple.lhs);
                    const rhs = inst.triple.rhs;

                    if (res.* != .func) return error.MalformedByteCode;
                    if (rhs >= res.func.captures.len) return error.MalformedByteCode;

                    res.func.captures[rhs] = lhs;
                },
                .load_this_single => {
                    const res = try vm.getRef(module, inst.single.arg);

                    const frame = vm.call_stack.at(vm.call_stack.len - 1);
                    res.* = frame.this orelse
                        return vm.reportErr("this has not been set");
                },
                .line_info => {
                    vm.line_loc = try vm.getSingle(module);
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
            const source = std.fs.cwd().readFileAlloc(vm.gc.gpa, id, vm.options.max_import_size) catch |e| switch (e) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return vm.reportErr("import failed"),
            };
            defer vm.gc.gpa.free(source);
            const mod = bog.compile(vm.gc.gpa, source, &vm.errors) catch
                return vm.reportErr("import failed");
            mod.name = try mem.dupe(vm.gc.gpa, u8, id);
            _ = try vm.imported_modules.put(id, mod);
            break :blk mod;
        } else if (mem.endsWith(u8, id, bog.bytecode_extension)) blk: {
            if (!vm.options.import_files) {
                return vm.reportErr("import failed");
            }
            const source = std.fs.cwd().readFileAlloc(vm.gc.gpa, id, vm.options.max_import_size) catch |e| switch (e) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return vm.reportErr("import failed"),
            };
            defer vm.gc.gpa.free(source);
            const read_module = Module.read(source) catch
                return vm.reportErr("import failed");

            const mod = try vm.gc.gpa.create(Module);
            mod.* = .{
                .name = try mem.dupe(vm.gc.gpa, u8, id),
                .code = try mem.dupe(vm.gc.gpa, bog.Instruction, read_module.code),
                .strings = try mem.dupe(vm.gc.gpa, u8, read_module.strings),
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
        return res;
    }

    fn getSingle(vm: *Vm, module: *Module) !u32 {
        if (vm.ip + 1 > module.code.len)
            return error.MalformedByteCode;

        vm.ip += 1;
        return module.code[vm.ip - 1].bare;
    }

    fn getLong(vm: *Vm, module: *Module, comptime T: type) !T {
        if (vm.ip + 2 > module.code.len)
            return error.MalformedByteCode;

        var arr: [2]bog.Instruction = undefined;
        arr[0] = module.code[vm.ip];
        arr[1] = module.code[vm.ip + 1];
        vm.ip += 2;

        return @bitCast(T, arr);
    }

    fn getVal(vm: *Vm, module: *Module, reg: RegRef) !*Value {
        return vm.gc.stackGet(vm.sp + reg) catch
            return error.MalformedByteCode;
    }

    fn getRef(vm: *Vm, module: *Module, reg: RegRef) !*?*Value {
        return try vm.gc.stackRef(vm.sp + reg);
    }

    fn getNewVal(vm: *Vm, module: *Module, reg: RegRef) !*Value {
        return try vm.gc.stackAlloc(vm.sp + reg);
    }

    fn getString(vm: *Vm, module: *Module, inst: bog.Instruction) ![]const u8 {
        const offset = if (inst.off.isArg())
            try vm.getSingle(module)
        else
            inst.off.off;

        const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
        return module.strings[offset + @sizeOf(u32) ..][0..len];
    }

    fn getBool(vm: *Vm, module: *Module, reg: RegRef) !bool {
        const val = try vm.getVal(module, reg);

        if (val.* != .bool) {
            return vm.reportErr("expected a boolean");
        }
        return val.bool;
    }

    fn getInt(vm: *Vm, module: *Module, reg: RegRef) !i64 {
        const val = try vm.getVal(module, reg);

        if (val.* != .int) {
            return vm.reportErr("expected an integer");
        }
        return val.int;
    }

    fn getIntRef(vm: *Vm, module: *Module, reg: RegRef) !*Value {
        const val = try vm.getVal(module, reg);

        if (val.* != .int) {
            return vm.reportErr("expected an integer");
        }
        return val;
    }

    fn getNum(vm: *Vm, module: *Module, reg: RegRef) !*Value {
        const val = try vm.getVal(module, reg);

        if (val.* != .int and val.* != .num) {
            return vm.reportErr("expected a number");
        }
        return val;
    }

    inline fn needNum(a: *Value, b: *Value) bool {
        return a.* == .num or b.* == .num;
    }

    inline fn asNum(val: *Value) f64 {
        return switch (val.*) {
            .int => |v| @intToFloat(f64, v),
            .num => |v| v,
            else => unreachable,
        };
    }

    pub fn reportErr(vm: *Vm, msg: []const u8) Error {
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

    /// Gets function `func_name` from map and calls it with `args`.
    pub fn call(vm: *Vm, val: *Value, func_name: []const u8, args: var) !*Value {
        std.debug.assert(vm.call_stack.len == 0); // vm must be in a callable state
        if (val.* != .map) return error.NotAMap;
        const index: Value = .{
            .str = func_name,
        };
        const member = val.map.getValue(&index) orelse
            return error.NoSuchMember;

        switch (member.*) {
            .func => |*func| {
                if (func.arg_count != args.len) {
                    // TODO improve this error message to tell the expected and given counts
                    return error.InvalidArgCount;
                }

                // prepare arguments
                inline for (args) |arg, i| {
                    const loc = try vm.gc.stackRef(i);
                    loc.* = try Value.zigToBog(vm, arg);
                }

                var frame: FunctionFrame = undefined;
                frame.this = val;
                try vm.call_stack.push(frame);

                vm.sp = 0;
                vm.ip = func.offset;
                return try vm.exec(func.module);
            },
            .native => return error.NativeFunctionsUnsupported, // TODO
            else => return error.NotAFunction,
        }
    }
};
