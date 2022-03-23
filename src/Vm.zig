const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Bytecode = bog.Bytecode;
const Value = bog.Value;
const Ref = Bytecode.Ref;
const Gc = bog.Gc;
const Errors = bog.Errors;

const Vm = @This();

const max_params = @import("Compiler.zig").max_params;

gc: Gc,

errors: Errors,

imports: std.StringHashMapUnmanaged(fn (*Vm) Vm.Error!*bog.Value) = .{},

/// all currently loaded packages and files
imported_modules: std.StringHashMapUnmanaged(*Bytecode) = .{},

options: Options = .{},

last_get: *Value = Value.Null,
call_depth: u32 = 0,

const max_depth = 512;

pub const Options = struct {
    /// can files be imported
    import_files: bool = false,

    /// run vm in repl mode
    repl: bool = false,

    /// maximum size of imported files
    max_import_size: u32 = 5 * 1024 * 1024,

    /// maximum amount of pages gc may allocate.
    /// 1 page == 1 MiB.
    /// default 2 GiB.
    page_limit: u32 = 2048,
};

pub const Frame = struct {
    body: []const Ref,
    ip: u32 = 0,
    mod: *Bytecode,
    this: ?*Value = null,
    caller_frame: ?*Frame,
    err_handlers: ErrHandlers = .{ .short = .{} },
    captures: []*Value,

    // Where to store a capture after a build_func instruction
    store_capture_index: u24 = 0,

    stack: std.AutoArrayHashMapUnmanaged(u32, *Value) = .{},

    pub fn deinit(f: *Frame, vm: *Vm) void {
        f.err_handlers.deinit(vm.gc.gpa);
        f.stack.deinit(vm.gc.gpa);
        f.* = undefined;
    }

    pub fn newVal(f: *Frame, vm: *Vm, ref: Ref) !*Value {
        const res = try f.newRef(vm, ref);
        if (res.*) |some| {
            // attempt to use old value for better performance in loops
            switch (some.*) {
                // simple values can be reused
                .int, .num, .range, .native => return some,
                // if string doesn't own it's contents it can be reused
                .str => |s| if (s.capacity == 0) return some,
                else => {},
            }
        }
        // allocate a new value if previous cannot be used or there isn't one
        res.* = try vm.gc.alloc();
        return res.*.?;
    }

    pub fn newRef(f: *Frame, vm: *Vm, ref: Ref) !*?*Value {
        const gop = try f.stack.getOrPut(vm.gc.gpa, @enumToInt(ref));
        const casted = @ptrCast(*?*Value, gop.value_ptr);
        if (!gop.found_existing) casted.* = null;
        return casted;
    }

    pub fn refAssert(f: *Frame, ref: Ref) **Value {
        return f.stack.getPtr(@enumToInt(ref)).?;
    }

    pub fn val(f: *Frame, ref: Ref) *Value {
        return f.stack.get(@enumToInt(ref)).?;
    }

    pub fn int(f: *Frame, vm: *Vm, ref: Ref) !i64 {
        const res = f.val(ref);
        if (res.* != .int) return vm.fatal("expected an integer");
        return res.int;
    }

    pub fn num(f: *Frame, vm: *Vm, ref: Ref) !*Value {
        const res = f.val(ref);
        switch (res.*) {
            .int, .num => return res,
            else => return vm.fatal("expected a number"),
        }
    }

    pub fn @"bool"(f: *Frame, vm: *Vm, ref: Ref) !bool {
        const res = f.val(ref);
        if (res.* != .bool) return vm.fatal("expected a bool");
        return res.bool;
    }
};

pub const Error = error{RuntimeError} || Allocator.Error;

pub fn init(allocator: Allocator, options: Options) Vm {
    return .{
        .gc = Gc.init(allocator, options.page_limit),
        .errors = Errors.init(allocator),
        .options = options,
    };
}

pub fn deinit(vm: *Vm) void {
    vm.errors.deinit();
    vm.gc.deinit();
    vm.imports.deinit(vm.gc.gpa);
    var it = vm.imported_modules.iterator();
    while (it.next()) |mod| {
        mod.value_ptr.*.deinit(vm.gc.gpa);
    }
    vm.imported_modules.deinit(vm.gc.gpa);
}

// TODO we might not want to require `importable` to be comptime
pub fn addPackage(vm: *Vm, name: []const u8, comptime importable: anytype) Allocator.Error!void {
    try vm.imports.putNoClobber(vm.gc.gpa, name, struct {
        fn func(_vm: *Vm) Vm.Error!*bog.Value {
            return bog.Value.zigToBog(_vm, importable);
        }
    }.func);
}

pub fn addStd(vm: *Vm) Allocator.Error!void {
    try vm.addStdNoIo();
    try vm.addPackage("std.io", bog.std.io);
    try vm.addPackage("std.os", bog.std.os);
}

pub fn addStdNoIo(vm: *Vm) Allocator.Error!void {
    try vm.addPackage("std.math", bog.std.math);
    try vm.addPackage("std.map", bog.std.map);
    try vm.addPackage("std.debug", bog.std.debug);
    try vm.addPackage("std.json", bog.std.json);
    try vm.addPackage("std.gc", bog.std.gc);
}

/// Compiles and executes the file given by `file_path`.
pub fn compileAndRun(vm: *Vm, file_path: []const u8) !*Value {
    const mod = try vm.importFile(file_path);

    var frame = Frame{
        .mod = mod,
        .body = mod.main,
        .caller_frame = null,
        .captures = &.{},
    };
    defer frame.deinit(vm);

    vm.gc.stack_protect_start = @frameAddress();

    var frame_val = try vm.gc.alloc();
    frame_val.* = .{ .frame = &frame };

    return vm.run(&frame);
}

/// Continues execution from current instruction pointer.
pub fn run(vm: *Vm, f: *Frame) Error!*Value {
    const mod = f.mod;
    const body = f.body;
    const ops = mod.code.items(.op);
    const data = mod.code.items(.data);

    var ip: u32 = f.ip;
    defer f.ip = ip;

    while (true) {
        const ref = body[ip];
        ip += 1;
        const inst = Bytecode.refToIndex(ref);
        switch (ops[inst]) {
            .primitive => {
                const res = try f.newRef(vm, ref);
                res.* = switch (data[inst].primitive) {
                    .@"null" => Value.Null,
                    .@"true" => Value.True,
                    .@"false" => Value.False,
                };
            },
            .int => {
                const res = try f.newVal(vm, ref);

                res.* = .{ .int = data[inst].int };
            },
            .num => {
                const res = try f.newVal(vm, ref);

                res.* = .{ .num = data[inst].num };
            },
            .str => {
                const res = try f.newVal(vm, ref);

                const str = mod.strings[data[inst].str.offset..][0..data[inst].str.len];
                res.* = Value.string(str);
            },
            .build_tuple => {
                const res = try f.newVal(vm, ref);
                const items = mod.extra[data[inst].extra.extra..][0..data[inst].extra.len];

                const tuple = try vm.gc.gpa.alloc(*Value, items.len);
                errdefer vm.gc.gpa.free(tuple);
                for (items) |item_ref, tuple_i| {
                    tuple[tuple_i] = f.val(item_ref);
                }
                res.* = .{ .tuple = tuple };
            },
            .build_list => {
                const res = try f.newVal(vm, ref);
                const items = mod.extra[data[inst].extra.extra..][0..data[inst].extra.len];

                var list = Value.List{};
                errdefer list.deinit(vm.gc.gpa);

                try list.ensureUnusedCapacity(vm.gc.gpa, items.len);
                list.items.len = items.len;

                for (items) |item_ref, list_I| {
                    list.items[list_I] = f.val(item_ref);
                }
                res.* = .{ .list = list };
            },
            .build_map => {
                const res = try f.newVal(vm, ref);
                const items = mod.extra[data[inst].extra.extra..][0..data[inst].extra.len];

                var map = Value.Map{};
                errdefer map.deinit(vm.gc.gpa);

                try map.ensureUnusedCapacity(vm.gc.gpa, items.len);

                var map_i: u32 = 0;
                while (map_i < items.len) : (map_i += 2) {
                    const key = f.val(items[map_i]);
                    const val = f.val(items[map_i + 1]);
                    map.putAssumeCapacity(key, val);
                }
                res.* = .{ .map = map };
            },
            .build_error => {
                const res = try f.newVal(vm, ref);
                const arg = f.val(data[inst].un);

                res.* = .{ .err = try vm.gc.dupe(arg) };
            },
            .build_error_null => {
                const res = try f.newVal(vm, ref);
                res.* = .{ .err = Value.Null };
            },
            .build_tagged => {
                const res = try f.newVal(vm, ref);
                const arg = f.val(data[inst].un);

                const name = mod.strings[data[inst].str.offset..][0..data[inst].str.len];
                res.* = .{ .tagged = .{
                    .name = name,
                    .value = try vm.gc.dupe(arg),
                } };
            },
            .build_tagged_null => {
                const res = try f.newVal(vm, ref);

                const name = mod.strings[data[inst].str.offset..][0..data[inst].str.len];
                res.* = .{ .tagged = .{ .name = name, .value = Value.Null } };
            },
            .build_func => {
                const res = try f.newVal(vm, ref);
                const extra = mod.extra[data[inst].extra.extra..][0..data[inst].extra.len];
                const fn_info = @bitCast(Bytecode.Inst.Data.FnInfo, extra[0]);
                const fn_body = extra[1..];

                const captures = try vm.gc.gpa.alloc(*Value, fn_info.captures);

                res.* = .{
                    .func = .{
                        .info = fn_info,
                        .body = fn_body.ptr,
                        .body_len = @intCast(u32, fn_body.len),
                        .module = mod,
                        .captures = captures.ptr,
                    },
                };
                f.store_capture_index = 0;
            },
            .build_range => {
                const res = try f.newVal(vm, ref);
                const start = try f.int(vm, data[inst].bin.lhs);
                const end = try f.int(vm, data[inst].bin.lhs);

                res.* = .{
                    .range = .{
                        .start = start,
                        .end = end,
                    },
                };
            },
            .build_range_step => {
                const res = try f.newVal(vm, ref);
                const start = try f.int(vm, data[inst].range.start);
                const end = try f.int(vm, mod.extra[data[inst].range.extra]);
                const step = try f.int(vm, mod.extra[data[inst].range.extra + 1]);

                res.* = .{
                    .range = .{
                        .start = start,
                        .end = end,
                        .step = step,
                    },
                };
            },
            .import => {
                const res = try f.newRef(vm, ref);
                const str = mod.strings[data[inst].str.offset..][0..data[inst].str.len];

                res.* = try vm.import(f, str);
            },
            .discard => {
                const arg = f.val(data[inst].un);

                if (arg.* == .err) {
                    return vm.fatal("error discarded");
                }
            },
            .copy_un => {
                const val = f.val(data[inst].un);

                const duped = try vm.gc.dupe(val);

                const res = try f.newRef(vm, ref);
                res.* = duped;
            },
            .copy => {
                const val = f.val(data[inst].bin.rhs);

                const duped = try vm.gc.dupe(val);

                const res = try f.newRef(vm, data[inst].bin.lhs);
                res.* = duped;
            },
            .move => {
                const val = f.val(data[inst].bin.rhs);
                const res = try f.newRef(vm, data[inst].bin.lhs);

                res.* = val;
            },
            .load_global => @panic("TODO"),
            .load_capture => {
                const index = @enumToInt(data[inst].un);
                const res = try f.newRef(vm, data[inst].bin.lhs);
                res.* = f.captures[index];
            },
            .store_capture => {
                const res = f.val(data[inst].bin.lhs);
                const val = f.val(data[inst].bin.rhs);

                res.func.captures[f.store_capture_index] = val;
                f.store_capture_index += 1;
            },
            .load_this => {
                const res = try f.newRef(vm, ref);
                res.* = f.this orelse Value.Null;
            },
            .div_floor => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                try vm.checkZero(rhs);

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .int = @floatToInt(i64, @divFloor(asNum(lhs), asNum(rhs))),
                } else .{
                    .int = std.math.divFloor(i64, lhs.int, rhs.int) catch
                        return vm.fatal("operation overflowed"),
                };
                res.* = copy;
            },
            .div => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                try vm.checkZero(rhs);

                const copy = Value{ .num = asNum(lhs) / asNum(rhs) };
                res.* = copy;
            },
            .rem => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                try vm.checkZero(rhs);
                if (isNegative(rhs))
                    return vm.fatal("remainder division by negative denominator");

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = @rem(asNum(lhs), asNum(rhs)),
                } else .{
                    .int = @rem(lhs.int, rhs.int),
                };
                res.* = copy;
            },
            .mul => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = asNum(lhs) * asNum(rhs),
                } else .{
                    .int = std.math.mul(i64, lhs.int, rhs.int) catch
                        return vm.fatal("operation overflowed"),
                };
                res.* = copy;
            },
            .pow => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = std.math.pow(f64, asNum(lhs), asNum(rhs)),
                } else .{
                    .int = std.math.powi(i64, lhs.int, rhs.int) catch
                        return vm.fatal("operation overflowed"),
                };
                res.* = copy;
            },
            .add => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = asNum(lhs) + asNum(rhs),
                } else .{
                    .int = std.math.add(i64, lhs.int, rhs.int) catch
                        return vm.fatal("operation overflowed"),
                };
                res.* = copy;
            },
            .sub => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = asNum(lhs) - asNum(rhs),
                } else .{
                    .int = std.math.sub(i64, lhs.int, rhs.int) catch
                        return vm.fatal("operation overflowed"),
                };
                res.* = copy;
            },
            .l_shift => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.int(vm, data[inst].bin.lhs);
                const rhs = try f.int(vm, data[inst].bin.rhs);
                if (rhs < 0) return vm.fatal("shift by negative amount");

                const val = if (rhs > std.math.maxInt(u6))
                    0
                else
                    lhs << @intCast(u6, rhs);
                res.* = .{
                    .int = val,
                };
            },
            .r_shift => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.int(vm, data[inst].bin.lhs);
                const rhs = try f.int(vm, data[inst].bin.rhs);
                if (rhs < 0) return vm.fatal("shift by negative amount");

                const val = if (rhs > std.math.maxInt(u6))
                    if (lhs < 0) std.math.maxInt(i64) else @as(i64, 0)
                else
                    lhs << @intCast(u6, rhs);
                res.* = .{
                    .int = val,
                };
            },
            .bit_and => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.int(vm, data[inst].bin.lhs);
                const rhs = try f.int(vm, data[inst].bin.rhs);

                res.* = .{ .int = lhs & rhs };
            },
            .bit_or => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.int(vm, data[inst].bin.lhs);
                const rhs = try f.int(vm, data[inst].bin.rhs);

                res.* = .{ .int = lhs | rhs };
            },
            .bit_xor => {
                const res = try f.newVal(vm, ref);
                const lhs = try f.int(vm, data[inst].bin.lhs);
                const rhs = try f.int(vm, data[inst].bin.rhs);

                res.* = .{ .int = lhs ^ rhs };
            },
            .equal => {
                const res = try f.newRef(vm, ref);
                const lhs = f.val(data[inst].bin.lhs);
                const rhs = f.val(data[inst].bin.rhs);

                res.* = if (lhs.eql(rhs)) Value.True else Value.False;
            },
            .not_equal => {
                const res = try f.newRef(vm, ref);
                const lhs = f.val(data[inst].bin.lhs);
                const rhs = f.val(data[inst].bin.rhs);

                res.* = if (!lhs.eql(rhs)) Value.True else Value.False;
            },
            .less_than => {
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) < asNum(rhs)
                else
                    lhs.int < rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .less_than_equal => {
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) <= asNum(rhs)
                else
                    lhs.int <= rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .greater_than => {
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) > asNum(rhs)
                else
                    lhs.int > rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .greater_than_equal => {
                const lhs = try f.num(vm, data[inst].bin.lhs);
                const rhs = try f.num(vm, data[inst].bin.rhs);
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) >= asNum(rhs)
                else
                    lhs.int >= rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .in => {
                const lhs = f.val(data[inst].bin.lhs);
                const rhs = f.val(data[inst].bin.rhs);

                switch (rhs.*) {
                    .str, .tuple, .list, .map, .range => {},
                    else => return vm.fatal("invalid type for 'in'"),
                }

                const res = try f.newRef(vm, ref);
                res.* = if (lhs.in(rhs)) Value.True else Value.False;
            },
            .append => {
                const container = f.val(data[inst].bin.lhs);
                const operand = f.val(data[inst].bin.rhs);

                try container.list.append(vm.gc.gpa, try vm.gc.dupe(operand));
            },
            .as => {
                const arg = f.val(data[inst].bin_ty.operand);

                // type is validated by the compiler
                const casted = try arg.as(vm, data[inst].bin_ty.ty);
                if (casted.* == .err) {
                    if (f.err_handlers.get()) |handler| {
                        const handler_operand = f.refAssert(handler.operand);
                        handler_operand.* = casted;
                        ip = handler.offset;
                    }
                }
                const res = try f.newRef(vm, ref);
                res.* = casted;
            },
            .is => {
                const res = try f.newRef(vm, ref);
                const arg = f.val(data[inst].bin_ty.operand);

                // type is validated by the compiler
                res.* = if (arg.is(data[inst].bin_ty.ty)) Value.True else Value.False;
            },
            .negate => {
                const res = try f.newVal(vm, ref);
                const operand = try f.num(vm, data[inst].un);

                const copy: Value = if (operand.* == .num) .{
                    .num = -operand.num,
                } else .{
                    .int = -operand.int,
                };
                res.* = copy;
            },
            .bool_not => {
                const operand = try f.bool(vm, data[inst].un);
                const res = try f.newRef(vm, ref);

                res.* = if (operand) Value.False else Value.True;
            },
            .bit_not => {
                const res = try f.newVal(vm, ref);
                const operand = try f.int(vm, data[inst].un);

                res.* = .{ .int = ~operand };
            },
            .unwrap_error => {
                const res = try f.newRef(vm, ref);
                const arg = f.val(data[inst].jump_condition.operand);

                if (arg.* != .err) return vm.fatal("expected an error");
                res.* = try vm.gc.dupe(arg.err);
            },
            .unwrap_tagged,
            .unwrap_tagged_or_null,
            => @panic("TODO"),
            .check_len => {
                const container = f.val(data[inst].bin.lhs);
                const len = @enumToInt(data[inst].bin.rhs);

                const ok = switch (container.*) {
                    .list => |list| list.items.len == len,
                    .tuple => |tuple| tuple.len == len,
                    else => false,
                };
                const res = try f.newRef(vm, ref);
                if (ok) {
                    res.* = Value.True;
                } else {
                    res.* = Value.False;
                }
            },
            .assert_len => {
                const container = f.val(data[inst].bin.lhs);
                const len = @enumToInt(data[inst].bin.rhs);

                const actual_len = switch (container.*) {
                    .list => |list| list.items.len,
                    .tuple => |tuple| tuple.len,
                    else => return vm.fatal("cannot destructure non list/tuple value"),
                };
                if (len < actual_len) {
                    const str = try Value.String.init(
                        vm.gc.gpa,
                        "not enough values to destructure (expected {d} args, got {d})",
                        .{ len, actual_len },
                    );
                    return vm.fatalExtra(str);
                } else if (len > actual_len) {
                    const str = try Value.String.init(
                        vm.gc.gpa,
                        "too many values to destructure (expected {d} args, got {d})",
                        .{ len, actual_len },
                    );
                    return vm.fatalExtra(str);
                }
            },
            .get => {
                const res = try f.newRef(vm, ref);
                const container = f.val(data[inst].bin.lhs);
                const index = f.val(data[inst].bin.rhs);

                try container.get(vm, index, res);
                f.this = container;
            },
            .get_or_null => {
                const res = try f.newRef(vm, ref);
                const container = f.val(data[inst].bin.lhs);
                const index = f.val(data[inst].bin.rhs);

                if (container.* != .map) {
                    res.* = Value.Null;
                } else {
                    res.* = container.map.get(index) orelse Value.Null;
                }
            },
            .set => {
                const container = f.val(data[inst].range.start);
                const index = f.val(mod.extra[data[inst].range.extra]);
                const val = f.val(mod.extra[data[inst].range.extra + 1]);

                try container.set(vm, index, val);
            },
            .push_err_handler => {
                const err_val_ref = data[inst].jump_condition.operand;
                const offset = data[inst].jump_condition.offset;

                try f.err_handlers.push(vm.gc.gpa, .{
                    .operand = err_val_ref,
                    .offset = offset,
                });
            },
            .pop_err_handler => {
                f.err_handlers.pop();
            },
            .jump => {
                ip = data[inst].jump;
            },
            .jump_if_true => {
                const arg = try f.bool(vm, data[inst].jump_condition.operand);

                if (arg) {
                    ip = data[inst].jump_condition.offset;
                }
            },
            .jump_if_false => {
                const arg = try f.bool(vm, data[inst].jump_condition.operand);

                if (!arg) {
                    ip = data[inst].jump_condition.offset;
                }
            },
            .jump_if_null => {
                const arg = f.val(data[inst].jump_condition.operand);

                if (arg.* == .@"null") {
                    ip = data[inst].jump_condition.offset;
                }
                continue;
            },
            .unwrap_error_or_jump => {
                const res = try f.newRef(vm, ref);
                const arg = f.val(data[inst].jump_condition.operand);

                if (arg.* == .err) {
                    res.* = arg.err;
                } else {
                    ip = data[inst].jump_condition.offset;
                }
            },
            .iter_init => {
                const arg = f.val(data[inst].un);

                const it = try Value.iterator(arg, vm);
                if (it.* == .err) {
                    if (f.err_handlers.get()) |handler| {
                        const handler_operand = f.refAssert(handler.operand);
                        handler_operand.* = it;
                        ip = handler.offset;
                    }
                }
                const res = try f.newRef(vm, ref);
                res.* = it;
            },
            .iter_next => {
                const res = try f.newRef(vm, ref);
                const arg = f.val(data[inst].jump_condition.operand);

                try arg.iterator.next(vm, res);

                if (res.*.?.* == .@"null") {
                    ip = data[inst].jump_condition.offset;
                }
            },
            .call,
            .call_one,
            .call_zero,
            => {
                var buf: [1]Ref = undefined;
                var args: []const Ref = &.{};
                var callee: *Value = undefined;
                switch (ops[inst]) {
                    .call => {
                        const extra = mod.extra[data[inst].extra.extra..][0..data[inst].extra.len];
                        callee = f.val(extra[0]);
                        args = extra[1..];
                    },
                    .call_one => {
                        callee = f.val(data[inst].bin.lhs);
                        buf[0] = data[inst].bin.rhs;
                        args = &buf;
                    },
                    .call_zero => callee = f.val(data[inst].un),
                    else => unreachable,
                }
                if (callee.* == .native) {
                    if (true) @panic("TODO native calls");
                } else if (callee.* == .func) {
                    if (callee.func.info.args != args.len) {
                        const str = try Value.String.init(
                            vm.gc.gpa,
                            "expected {} args, got {}",
                            .{ callee.func.info.args, args.len },
                        );
                        return vm.fatalExtra(str);
                    }

                    if (vm.call_depth > max_depth) {
                        return vm.fatal("maximum recursion depth exceeded");
                    }
                    vm.call_depth += 1;
                    defer vm.call_depth -= 1;

                    var new_frame = Frame{
                        .mod = mod,
                        .body = callee.func.body[0..callee.func.body_len],
                        .caller_frame = f,
                        .this = f.this,
                        .captures = &.{},
                    };
                    defer new_frame.deinit(vm);

                    try new_frame.stack.ensureUnusedCapacity(vm.gc.gpa, args.len);

                    for (args) |arg_ref, i| {
                        new_frame.stack.putAssumeCapacity(@intCast(u32, i), f.val(arg_ref));
                    }

                    var frame_val = try vm.gc.alloc();
                    frame_val.* = .{ .frame = &new_frame };

                    const returned = try f.newRef(vm, ref);
                    returned.* = try vm.run(&new_frame);
                } else {
                    const returned = try f.newRef(vm, ref);
                    returned.* = try vm.typeError(.func, callee.*);
                }
                f.this = null;
                const returned = f.val(ref);
                if (returned.* == .err) {
                    if (f.err_handlers.get()) |handler| {
                        const handler_operand = f.refAssert(handler.operand);
                        handler_operand.* = returned;
                        ip = handler.offset;
                    }
                }
            },
            .ret => return f.val(data[inst].un),
            .ret_null => return Value.Null,
            .throw => {
                const val = f.val(data[inst].un);
                if (val.* == .err) {
                    if (f.err_handlers.get()) |handler| {
                        const handler_operand = f.refAssert(handler.operand);
                        handler_operand.* = val;
                        ip = handler.offset;
                        continue;
                    }
                }
                return val;
            },
        }
    }
}

const ErrHandlers = extern union {
    const Handler = extern struct {
        operand: Ref,
        offset: u32,
    };

    short: extern struct {
        len: u32 = 0,
        capacity: u32 = 4,
        arr: [4]Handler = undefined,
    },
    long: extern struct {
        len: u32,
        capacity: u32,
        ptr: [*]Handler,
    },

    fn deinit(e: *ErrHandlers, gpa: Allocator) void {
        if (e.short.capacity != 4) {
            gpa.free(e.long.ptr[0..e.long.capacity]);
        }
    }

    fn push(e: *ErrHandlers, gpa: Allocator, new: Handler) !void {
        if (e.short.capacity != 4) {
            var arr_list = std.ArrayList(Handler){
                .items = e.long.ptr[0..e.long.len],
                .capacity = e.long.capacity,
                .allocator = gpa,
            };
            defer {
                e.long.capacity = @intCast(u32, arr_list.capacity);
                e.long.len = @intCast(u32, arr_list.items.len);
                e.long.ptr = arr_list.items.ptr;
            }
            try arr_list.append(new);
        } else if (e.short.len == 4) {
            var arr_list = std.ArrayList(Handler).init(gpa);
            {
                errdefer arr_list.deinit();
                try arr_list.appendSlice(&e.short.arr);
                try arr_list.append(new);
            }
            e.long.capacity = @intCast(u32, arr_list.capacity);
            e.long.len = @intCast(u32, arr_list.items.len);
            e.long.ptr = arr_list.items.ptr;
        } else {
            e.short.arr[e.short.len] = new;
            e.short.len += 1;
        }
    }

    fn pop(e: *ErrHandlers) void {
        e.short.len -= 1;
    }

    fn get(e: ErrHandlers) ?Handler {
        return if (e.short.len == 0)
            null
        else if (e.short.capacity == 4)
            e.short.arr[e.short.len - 1]
        else
            e.long.ptr[e.long.len - 1];
    }
};

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

fn checkZero(vm: *Vm, val: *Value) !void {
    switch (val.*) {
        .int => |v| if (v != 0) return,
        .num => |v| if (v != 0) return,
        else => unreachable,
    }
    return vm.fatal("division by zero");
}

fn isNegative(val: *Value) bool {
    switch (val.*) {
        .int => |v| return v < 0,
        .num => |v| return v < 0,
        else => unreachable,
    }
}

fn importFile(vm: *Vm, id: []const u8) !*Bytecode {
    if (!vm.options.import_files) {
        const str = try Value.String.init(
            vm.gc.gpa,
            "cannot import '{s}': importing disabled by host",
            .{id},
        );
        return vm.fatalExtra(str);
    }

    const source = std.fs.cwd().readFileAlloc(vm.gc.gpa, id, vm.options.max_import_size) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => |err| {
            const str = try Value.String.init(
                vm.gc.gpa,
                "cannot import '{s}': {s}",
                .{ id, @errorName(err) },
            );
            return vm.fatalExtra(str);
        },
    };
    errdefer vm.gc.gpa.free(source);

    var mod = bog.compile(vm.gc.gpa, source, &vm.errors) catch |err| {
        const str = try Value.String.init(
            vm.gc.gpa,
            "cannot import '{s}': {s}",
            .{ id, @errorName(err) },
        );
        return vm.fatalExtra(str);
    };
    errdefer mod.deinit(vm.gc.gpa);

    // mod takes ownership
    mod.debug_info.file_path = try vm.gc.gpa.dupe(u8, id);

    const duped = try vm.gc.gpa.create(Bytecode);
    errdefer vm.gc.gpa.destroy(duped);
    duped.* = mod;

    _ = try vm.imported_modules.put(vm.gc.gpa, id, duped);
    return duped;
}

fn import(vm: *Vm, caller_frame: *Frame, id: []const u8) !*Value {
    const mod = vm.imported_modules.get(id) orelse if (mem.endsWith(u8, id, bog.extension))
        try vm.importFile(id)
    else {
        if (vm.imports.get(id)) |some| {
            return some(vm);
        }
        return vm.fatal("no such package");
    };

    var frame = Frame{
        .mod = mod,
        .body = mod.main,
        .caller_frame = caller_frame,
        .this = caller_frame.this,
        .captures = &.{},
    };
    defer frame.deinit(vm);

    var frame_val = try vm.gc.alloc();
    frame_val.* = .{ .frame = &frame };

    return vm.run(&frame);
}

pub fn errorFmt(vm: *Vm, comptime fmt: []const u8, args: anytype) Vm.Error!*Value {
    const str = try vm.gc.alloc();
    str.* = .{ .str = try Value.String.init(vm.gc.gpa, fmt, args) };

    const err = try vm.gc.alloc();
    err.* = .{ .err = str };
    return err;
}

pub fn typeError(vm: *Vm, expected: bog.Type, got: bog.Type) Vm.Error!*Value {
    return vm.errorFmt("expected {s}, got {s}", .{ @tagName(expected), @tagName(got) });
}

pub fn errorVal(vm: *Vm, msg: []const u8) !*Value {
    const str = try vm.gc.alloc();
    str.* = Value.string(msg);

    const err = try vm.gc.alloc();
    err.* = .{ .err = str };
    return err;
}

pub fn fatal(vm: *Vm, msg: []const u8) Error {
    @setCold(true);
    return vm.fatalExtra(.{ .data = msg });
}

pub fn fatalExtra(vm: *Vm, str: Value.String) Error {
    @setCold(true);
    _ = vm;
    _ = str;
    return error.RuntimeError;
}

/// Gets function `func_name` from map and calls it with `args`.
pub fn call(vm: *Vm, val: *Value, func_name: []const u8, args: anytype) !*Value {
    std.debug.assert(vm.call_stack.items.len == 0); // vm must be in a callable state
    if (val.* != .map) return error.NotAMap;
    const index = Value.string(func_name);
    const member = val.map.get(&index) orelse
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

            var frame: Frame = undefined;
            frame.this = val;
            try vm.call_stack.append(vm.gc.gpa, frame);

            vm.sp = 0;
            vm.ip = func.offset;
            return try vm.exec(func.module);
        },
        .native => return error.NativeFunctionsUnsupported, // TODO
        else => return error.NotAFunction,
    }
}
