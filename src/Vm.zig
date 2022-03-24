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

/// Functions that act like modules, called by `import`ing a package by the name.
imports: std.StringHashMapUnmanaged(fn (Context) Value.NativeError!*Value) = .{},

/// All currently loaded packages and files.
imported_modules: std.StringHashMapUnmanaged(*Bytecode) = .{},

options: Options = .{},
/// Current call stack depth, used to prevent stack overflow.
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
    /// List of instructions part of this function.
    body: []const Ref,
    /// Index into `body`.
    ip: u32 = 0,
    /// The module in which this function lives in.
    mod: *Bytecode,
    /// Values this function captures.
    captures: []*Value,

    /// Value of `this` as set by the caller.
    this: *Value = Value.Null,
    /// Frame of the function which called this, forms a call stack.
    caller_frame: ?*Frame,
    /// Frame of `mod.main`.
    module_frame: *Frame,
    /// Where to store a capture after a build_func instruction
    store_capture_index: u24 = 0,
    /// This function frames stack.
    stack: std.AutoArrayHashMapUnmanaged(u32, *Value) = .{},
    /// Stack of error handlers that have been set up.
    err_handlers: ErrHandlers = .{ .short = .{} },

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

    pub fn int(f: *Frame, vm: *Vm, ref: Ref) !?i64 {
        const res = f.val(ref);
        if (res.* != .int) {
            try f.throw(vm, "expected an integer");
            return null;
        }
        return res.int;
    }

    pub fn num(f: *Frame, vm: *Vm, ref: Ref) !?*Value {
        const res = f.val(ref);
        switch (res.*) {
            .int, .num => return res,
            else => {
                try f.throw(vm, "expected a number");
                return null;
            },
        }
    }

    pub fn @"bool"(f: *Frame, vm: *Vm, ref: Ref) !?bool {
        const res = f.val(ref);
        if (res.* != .bool) {
            try f.throw(vm, "expected a bool");
            return null;
        }
        return res.bool;
    }

    pub fn ctx(f: *Frame, vm: *Vm) Context {
        return .{ .vm = vm, .frame = f };
    }

    pub fn ctxThis(f: *Frame, this: *Value, vm: *Vm) Context {
        return .{ .this = this, .vm = vm, .frame = f };
    }

    pub fn fatal(f: *Frame, vm: *Vm, msg: []const u8) Error {
        @setCold(true);
        return f.fatalExtra(vm, .{ .data = msg }, .err);
    }

    pub fn fatalExtra(f: *Frame, vm: *Vm, msg: Value.String, kind: bog.Errors.Kind) Error {
        @setCold(true);

        const line_num = f.mod.debug_info.getLineForIndex(f.ip);
        var i: u32 = 0;
        var lines_seen: u32 = 1;
        const source = f.mod.debug_info.source;
        while (lines_seen < line_num and i < source.len) : (i += 1) {
            if (source[i] == '\n') {
                lines_seen += 1;
            }
        }
        const start = i;
        // find the end of the line
        while (i < source.len) : (i += 1) {
            if (source[i] == '\n') break;
        }
        try vm.errors.list.append(vm.errors.arena.child_allocator, .{
            .msg = msg,
            .line = try vm.errors.arena.allocator().dupe(u8, source[start..i]),
            .path = try vm.errors.arena.allocator().dupe(u8, f.mod.debug_info.path),
            .line_num = line_num,
            .col_num = 1, // TODO
            .kind = kind,
        });
        if (f.caller_frame) |some| return some.fatalExtra(vm, .{ .data = "called here" }, .trace);
        return error.FatalError;
    }

    pub fn throw(f: *Frame, vm: *Vm, err: []const u8) !void {
        if (f.err_handlers.get()) |handler| {
            const handler_operand = try f.newRef(vm, handler.operand);
            handler_operand.* = try vm.errorVal(err);
            f.ip = handler.offset;
        } else {
            return f.fatal(vm, err);
        }
    }

    pub fn throwFmt(f: *Frame, vm: *Vm, comptime err: []const u8, args: anytype) !void {
        if (f.err_handlers.get()) |handler| {
            const handler_operand = try f.newRef(vm, handler.operand);
            handler_operand.* = try vm.errorFmt(err, args);
            f.ip = handler.offset;
        } else {
            const str = try Value.String.init(vm.gc.gpa, err, args);
            return f.fatalExtra(vm, str, .err);
        }
    }
};

pub const Context = struct {
    this: *Value = Value.Null,
    vm: *Vm,
    frame: *Vm.Frame,

    pub fn throw(ctx: Context, err: []const u8) Value.NativeError {
        try ctx.frame.throw(ctx.vm, err);
        return error.Throw;
    }

    pub fn throwFmt(ctx: Context, comptime err: []const u8, args: anytype) Value.NativeError {
        try ctx.frame.throwFmt(ctx.vm, err, args);
        return error.Throw;
    }
};

pub const Error = error{FatalError} || Allocator.Error;

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
        fn func(ctx: Context) Vm.Error!*bog.Value {
            return bog.Value.zigToBog(ctx.vm, importable);
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
    const mod = vm.importFile(file_path) catch |err| switch (err) {
        error.ImportingDisabled => unreachable,
        else => |e| return e,
    };

    var frame = Frame{
        .mod = mod,
        .body = mod.main,
        .caller_frame = null,
        .module_frame = undefined,
        .captures = &.{},
    };
    defer frame.deinit(vm);
    frame.module_frame = &frame;

    vm.gc.stack_protect_start = @frameAddress();

    var frame_val = try vm.gc.alloc();
    frame_val.* = .{ .frame = &frame };
    defer frame_val.* = .{ .int = 0 }; // clear frame

    return vm.run(&frame);
}

/// Continues execution from current instruction pointer.
pub fn run(vm: *Vm, f: *Frame) Error!*Value {
    const mod = f.mod;
    const body = f.body;
    const ops = mod.code.items(.op);
    const data = mod.code.items(.data);

    while (true) {
        const ref = body[f.ip];
        f.ip += 1;
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
                const arg = f.val(mod.extra[data[inst].extra.extra]);
                const str_offset = @enumToInt(mod.extra[data[inst].extra.extra + 1]);

                const name = mod.strings[str_offset..][0..data[inst].extra.len];
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
                const start = (try f.int(vm, data[inst].bin.lhs)) orelse continue;
                const end = (try f.int(vm, data[inst].bin.rhs)) orelse continue;

                const res = try f.newVal(vm, ref);
                res.* = .{
                    .range = .{
                        .start = start,
                        .end = end,
                    },
                };
            },
            .build_range_step => {
                const start = (try f.int(vm, data[inst].range.start)) orelse continue;
                const end = (try f.int(vm, mod.extra[data[inst].range.extra])) orelse continue;
                const step = (try f.int(vm, mod.extra[data[inst].range.extra + 1])) orelse continue;

                const res = try f.newVal(vm, ref);
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
                    return f.fatal(vm, "error discarded");
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
            .load_global => {
                const res = try f.newRef(vm, ref);
                res.* = f.module_frame.stack.get(@enumToInt(data[inst].un)) orelse
                    return f.fatal(vm, "use of undefined variable");
            },
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
                res.* = f.this;
            },
            .div_floor => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
                if (checkZero(rhs)) {
                    try f.throw(vm, "division by zero");
                    continue;
                }

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .int = std.math.lossyCast(i64, @divFloor(asNum(lhs), asNum(rhs))),
                } else .{
                    .int = std.math.divFloor(i64, lhs.int, rhs.int) catch {
                        try f.throw(vm, "operation overflowed");
                        continue;
                    },
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .div => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
                if (checkZero(rhs)) {
                    try f.throw(vm, "division by zero");
                    continue;
                }

                const copy = Value{ .num = asNum(lhs) / asNum(rhs) };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .rem => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
                if (checkZero(rhs)) {
                    try f.throw(vm, "division by zero");
                    continue;
                }
                if (isNegative(rhs)) {
                    try f.throw(vm, "remainder division by negative denominator");
                    continue;
                }

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = @rem(asNum(lhs), asNum(rhs)),
                } else .{
                    .int = @rem(lhs.int, rhs.int),
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .mul => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = asNum(lhs) * asNum(rhs),
                } else .{
                    .int = std.math.mul(i64, lhs.int, rhs.int) catch {
                        try f.throw(vm, "operation overflowed");
                        continue;
                    },
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .pow => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = std.math.pow(f64, asNum(lhs), asNum(rhs)),
                } else .{
                    .int = std.math.powi(i64, lhs.int, rhs.int) catch {
                        try f.throw(vm, "operation overflowed");
                        continue;
                    },
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .add => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = asNum(lhs) + asNum(rhs),
                } else .{
                    .int = std.math.add(i64, lhs.int, rhs.int) catch {
                        try f.throw(vm, "operation overflowed");
                        continue;
                    },
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .sub => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;

                const copy: Value = if (needNum(lhs, rhs)) .{
                    .num = asNum(lhs) - asNum(rhs),
                } else .{
                    .int = std.math.sub(i64, lhs.int, rhs.int) catch {
                        try f.throw(vm, "operation overflowed");
                        continue;
                    },
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .l_shift => {
                const lhs = (try f.int(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.int(vm, data[inst].bin.rhs)) orelse continue;
                if (rhs < 0) {
                    try f.throw(vm, "shift by negative amount");
                    continue;
                }

                const val = if (rhs > std.math.maxInt(u6))
                    0
                else
                    lhs << @intCast(u6, rhs);

                const res = try f.newVal(vm, ref);
                res.* = .{ .int = val };
            },
            .r_shift => {
                const lhs = (try f.int(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.int(vm, data[inst].bin.rhs)) orelse continue;
                if (rhs < 0) {
                    try f.throw(vm, "shift by negative amount");
                    continue;
                }

                const val = if (rhs > std.math.maxInt(u6))
                    if (lhs < 0) std.math.maxInt(i64) else @as(i64, 0)
                else
                    lhs << @intCast(u6, rhs);

                const res = try f.newVal(vm, ref);
                res.* = .{ .int = val };
            },
            .bit_and => {
                const lhs = (try f.int(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.int(vm, data[inst].bin.rhs)) orelse continue;

                const res = try f.newVal(vm, ref);
                res.* = .{ .int = lhs & rhs };
            },
            .bit_or => {
                const lhs = (try f.int(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.int(vm, data[inst].bin.rhs)) orelse continue;

                const res = try f.newVal(vm, ref);
                res.* = .{ .int = lhs | rhs };
            },
            .bit_xor => {
                const lhs = (try f.int(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.int(vm, data[inst].bin.rhs)) orelse continue;

                const res = try f.newVal(vm, ref);
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
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) < asNum(rhs)
                else
                    lhs.int < rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .less_than_equal => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) <= asNum(rhs)
                else
                    lhs.int <= rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .greater_than => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
                const res = try f.newRef(vm, ref);

                const bool_val = if (needNum(lhs, rhs))
                    asNum(lhs) > asNum(rhs)
                else
                    lhs.int > rhs.int;

                res.* = if (bool_val) Value.True else Value.False;
            },
            .greater_than_equal => {
                const lhs = (try f.num(vm, data[inst].bin.lhs)) orelse continue;
                const rhs = (try f.num(vm, data[inst].bin.rhs)) orelse continue;
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
                    else => {
                        try f.throwFmt(vm, "'in' not allowed with type {s}", .{@tagName(rhs.*)});
                        continue;
                    },
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
                const casted = arg.as(vm, data[inst].bin_ty.ty) catch |err| switch (err) {
                    error.Throw => continue,
                    else => |e| return e,
                };
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
                const operand = (try f.num(vm, data[inst].un)) orelse continue;

                const copy: Value = if (operand.* == .num) .{
                    .num = -operand.num,
                } else .{
                    .int = -operand.int,
                };
                const res = try f.newVal(vm, ref);
                res.* = copy;
            },
            .bool_not => {
                const operand = (try f.bool(vm, data[inst].un)) orelse continue;
                const res = try f.newRef(vm, ref);

                res.* = if (operand) Value.False else Value.True;
            },
            .bit_not => {
                const operand = (try f.int(vm, data[inst].un)) orelse continue;

                const res = try f.newVal(vm, ref);
                res.* = .{ .int = ~operand };
            },
            .unwrap_error => {
                const arg = f.val(data[inst].un);

                if (arg.* != .err) {
                    try f.throw(vm, "expected an error");
                    continue;
                }

                const res = try f.newRef(vm, ref);
                res.* = try vm.gc.dupe(arg.err);
            },
            .unwrap_tagged => {
                const operand = f.val(mod.extra[data[inst].extra.extra]);
                const str_offset = @enumToInt(mod.extra[data[inst].extra.extra + 1]);
                const name = mod.strings[str_offset..][0..data[inst].extra.len];

                if (operand.* != .tagged) {
                    try f.throw(vm, "expected a tagged value");
                    continue;
                }
                if (!mem.eql(u8, operand.tagged.name, name)) {
                    try f.throw(vm, "invalid tag");
                    continue;
                }

                const res = try f.newRef(vm, ref);
                res.* = operand.tagged.value;
            },
            .unwrap_tagged_or_null => {
                const res = try f.newRef(vm, ref);
                const operand = f.val(mod.extra[data[inst].extra.extra]);
                const str_offset = @enumToInt(mod.extra[data[inst].extra.extra + 1]);
                const name = mod.strings[str_offset..][0..data[inst].extra.len];

                if (operand.* == .tagged and mem.eql(u8, operand.tagged.name, name)) {
                    res.* = operand.tagged.value;
                } else {
                    res.* = Value.Null;
                }
            },
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
                    else => {
                        try f.throw(vm, "cannot destructure non list/tuple value");
                        continue;
                    },
                };
                if (len < actual_len) {
                    try f.throwFmt(
                        vm,
                        "not enough values to destructure (expected {d} args, got {d})",
                        .{ len, actual_len },
                    );
                } else if (len > actual_len) {
                    try f.throwFmt(
                        vm,
                        "too many values to destructure (expected {d} args, got {d})",
                        .{ len, actual_len },
                    );
                }
            },
            .get => {
                const res = try f.newRef(vm, ref);
                const container = f.val(data[inst].bin.lhs);
                const index = f.val(data[inst].bin.rhs);

                container.get(f.ctx(vm), index, res) catch |err| switch (err) {
                    error.Throw => continue,
                    else => |e| return e,
                };
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

                container.set(f.ctx(vm), index, val) catch |err| switch (err) {
                    error.Throw => continue,
                    else => |e| return e,
                };
            },
            .push_err_handler => {
                const err_val_ref = data[inst].jump_condition.operand;
                const offset = data[inst].jump_condition.offset;

                // Clear the error value ref in case we're in a loop
                _ = f.stack.swapRemove(@enumToInt(err_val_ref));

                try f.err_handlers.push(vm.gc.gpa, .{
                    .operand = err_val_ref,
                    .offset = offset,
                });
            },
            .pop_err_handler => {
                const handler = f.err_handlers.pop();

                // Jump past error handlers in case nothing was thrown
                if (f.stack.get(@enumToInt(handler.operand)) == null) {
                    f.ip = data[inst].jump;
                }
            },
            .jump => {
                f.ip = data[inst].jump;
            },
            .jump_if_true => {
                const arg = (try f.bool(vm, data[inst].jump_condition.operand)) orelse continue;

                if (arg) {
                    f.ip = data[inst].jump_condition.offset;
                }
            },
            .jump_if_false => {
                const arg = (try f.bool(vm, data[inst].jump_condition.operand)) orelse continue;

                if (!arg) {
                    f.ip = data[inst].jump_condition.offset;
                }
            },
            .jump_if_null => {
                const arg = f.val(data[inst].jump_condition.operand);

                if (arg.* == .@"null") {
                    f.ip = data[inst].jump_condition.offset;
                }
                continue;
            },
            .unwrap_error_or_jump => {
                const res = try f.newRef(vm, ref);
                const arg = f.val(data[inst].jump_condition.operand);

                if (arg.* == .err) {
                    res.* = arg.err;
                } else {
                    f.ip = data[inst].jump_condition.offset;
                }
            },
            .iter_init => {
                const arg = f.val(data[inst].un);

                const it = Value.iterator(arg, f.ctx(vm)) catch |err| switch (err) {
                    error.Throw => continue,
                    else => |e| return e,
                };
                const res = try f.newRef(vm, ref);
                res.* = it;
            },
            .iter_next => {
                const res = try f.newRef(vm, ref);
                const arg = f.val(data[inst].jump_condition.operand);

                arg.iterator.next(f.ctx(vm), res) catch |err| switch (err) {
                    error.Throw => continue,
                    else => |e| return e,
                };

                if (res.*.?.* == .@"null") {
                    f.ip = data[inst].jump_condition.offset;
                }
            },
            .call,
            .call_one,
            .call_zero,
            .this_call,
            .this_call_zero,
            => {
                var buf: [1]Ref = undefined;
                var args: []const Ref = &.{};
                var callee: *Value = undefined;
                var this: *Value = Value.Null;
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
                    .this_call => {
                        const extra = mod.extra[data[inst].extra.extra..][0..data[inst].extra.len];
                        callee = f.val(extra[0]);
                        this = f.val(extra[1]);
                        args = extra[2..];
                    },
                    .this_call_zero => {
                        callee = f.val(data[inst].bin.lhs);
                        this = f.val(data[inst].bin.rhs);
                    },
                    else => unreachable,
                }
                if (callee.* == .native) {
                    if (callee.native.arg_count != args.len) {
                        try f.throwFmt(
                            vm,
                            "expected {} args, got {}",
                            .{ callee.native.arg_count, args.len },
                        );
                        continue;
                    }

                    const res = callee.native.func(f.ctxThis(this, vm), args) catch |err| switch (err) {
                        error.Throw => continue,
                        else => |e| return e,
                    };

                    // function may mutate the stack
                    const returned = try f.newRef(vm, ref);
                    returned.* = res;
                } else if (callee.* == .func) {
                    if (callee.func.info.args != args.len) {
                        try f.throwFmt(
                            vm,
                            "expected {} args, got {}",
                            .{ callee.func.info.args, args.len },
                        );
                        continue;
                    }

                    if (vm.call_depth > max_depth) {
                        return f.fatal(vm, "maximum recursion depth exceeded");
                    }
                    vm.call_depth += 1;
                    defer vm.call_depth -= 1;

                    var new_frame = Frame{
                        .mod = mod,
                        .body = callee.func.body[0..callee.func.body_len],
                        .caller_frame = f,
                        .module_frame = f.module_frame,
                        .this = this,
                        .captures = &.{},
                    };
                    defer new_frame.deinit(vm);

                    try new_frame.stack.ensureUnusedCapacity(vm.gc.gpa, args.len);

                    for (args) |arg_ref, i| {
                        new_frame.stack.putAssumeCapacity(@intCast(u32, i), f.val(arg_ref));
                    }

                    var frame_val = try vm.gc.alloc();
                    frame_val.* = .{ .frame = &new_frame };
                    defer frame_val.* = .{ .int = 0 }; // clear frame

                    const returned = try vm.run(&new_frame);
                    if (returned.* == .err) {
                        if (f.err_handlers.get()) |handler| {
                            const handler_operand = try f.newRef(vm, handler.operand);
                            handler_operand.* = returned.err;
                            f.ip = handler.offset;
                        }
                    }
                    const res = try f.newRef(vm, ref);
                    res.* = returned;
                } else {
                    try f.throwFmt(vm, "cannot call '{s}'", .{@tagName(callee.*)});
                }
            },
            .ret => return f.val(data[inst].un),
            .ret_null => return Value.Null,
            .throw => {
                const val = f.val(data[inst].un);
                if (f.err_handlers.get()) |handler| {
                    const handler_operand = try f.newRef(vm, handler.operand);
                    handler_operand.* = val;
                    f.ip = handler.offset;
                    continue;
                }
                const res = try vm.gc.alloc();
                res.* = .{ .err = val };
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

    fn pop(e: *ErrHandlers) Handler {
        const handler = e.get().?;
        e.short.len -= 1;
        return handler;
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

fn checkZero(val: *Value) bool {
    switch (val.*) {
        .int => |v| return v == 0,
        .num => |v| return v == 0,
        else => unreachable,
    }
}

fn isNegative(val: *Value) bool {
    switch (val.*) {
        .int => |v| return v < 0,
        .num => |v| return v < 0,
        else => unreachable,
    }
}

fn importFile(vm: *Vm, path: []const u8) !*Bytecode {
    if (!vm.options.import_files) return error.ImportingDisabled;

    var mod = mod: {
        const source = try std.fs.cwd().readFileAlloc(vm.gc.gpa, path, vm.options.max_import_size);
        errdefer vm.gc.gpa.free(source);

        break :mod try bog.compile(vm.gc.gpa, source, path, &vm.errors);
    };
    errdefer mod.deinit(vm.gc.gpa);

    const duped = try vm.gc.gpa.create(Bytecode);
    errdefer vm.gc.gpa.destroy(duped);
    duped.* = mod;

    _ = try vm.imported_modules.put(vm.gc.gpa, duped.debug_info.path, duped);
    return duped;
}

fn import(vm: *Vm, caller_frame: *Frame, id: []const u8) !*Value {
    const mod = vm.imported_modules.get(id) orelse if (mem.endsWith(u8, id, bog.extension))
        vm.importFile(id) catch |err| switch (err) {
            error.ImportingDisabled => {
                try caller_frame.throwFmt(
                    vm,
                    "cannot import '{s}': importing disabled by host",
                    .{id},
                );
                return Value.Null;
            },
            else => |e| {
                try caller_frame.throwFmt(
                    vm,
                    "cannot import '{s}': {s}",
                    .{ id, @errorName(e) },
                );
                return Value.Null;
            },
        }
    else {
        if (vm.imports.get(id)) |some| {
            return some(caller_frame.ctx(vm)) catch |err| switch (err) {
                error.Throw => return Value.Null,
                else => |e| return e,
            };
        }
        try caller_frame.throw(vm, "no such package");
        return Value.Null;
    };

    var frame = Frame{
        .mod = mod,
        .body = mod.main,
        .caller_frame = caller_frame,
        .module_frame = undefined,
        .this = Value.Null,
        .captures = &.{},
    };
    defer frame.deinit(vm);
    frame.module_frame = &frame;

    var frame_val = try vm.gc.alloc();
    frame_val.* = .{ .frame = &frame };
    defer frame_val.* = .{ .int = 0 }; // clear frame

    return vm.run(&frame);
}

pub fn errorFmt(vm: *Vm, comptime fmt: []const u8, args: anytype) Vm.Error!*Value {
    const str = try vm.gc.alloc();
    str.* = .{ .str = try Value.String.init(vm.gc.gpa, fmt, args) };

    const err = try vm.gc.alloc();
    err.* = .{ .err = str };
    return err;
}

pub fn errorVal(vm: *Vm, msg: []const u8) !*Value {
    const str = try vm.gc.alloc();
    str.* = Value.string(msg);

    const err = try vm.gc.alloc();
    err.* = .{ .err = str };
    return err;
}
