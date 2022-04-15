const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;

pub const Type = enum(u8) {
    @"null",
    int,
    num,
    bool,
    str,
    tuple,
    map,
    list,
    err,
    range,
    func,
    tagged,

    /// A called function during execution.
    frame,

    /// pseudo type user should not have access to via valid bytecode
    iterator,

    /// native being separate from .func is an implementation detail
    native,

    native_val,

    /// Result of ... operand, needs special handling in contexts where allowed.
    spread,
};

pub const Value = union(Type) {
    tuple: []*Value,
    map: Map,
    list: List,
    err: *Value,
    int: i64,
    num: f64,
    range: Range,
    str: String,
    func: Func,
    frame: *Vm.Frame,
    native: Native,
    native_val: NativeVal,
    tagged: struct {
        name: []const u8,
        value: *Value,
    },
    iterator: Iterator,
    spread: Spread,

    /// always memoized
    bool: bool,
    @"null",

    pub const Iterator = struct {
        value: *Value,
        i: packed union {
            u: usize,
            i: i64,
        } = .{ .u = 0 },

        pub fn next(iter: *Iterator, ctx: Vm.Context, res: *?*Value) !bool {
            switch (iter.value.*) {
                .tuple => |tuple| {
                    if (iter.i.u == tuple.len) {
                        return false;
                    }

                    res.* = tuple[iter.i.u];
                    iter.i.u += 1;
                },
                .list => |list| {
                    if (iter.i.u == list.inner.items.len) {
                        return false;
                    }

                    res.* = list.inner.items[iter.i.u];
                    iter.i.u += 1;
                },
                .str => |str| {
                    if (iter.i.u == str.data.len) {
                        return false;
                    }
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc(.str);
                    }

                    const cp_len = std.unicode.utf8ByteSequenceLength(str.data[iter.i.u]) catch
                        return ctx.throw("invalid utf-8 sequence");
                    iter.i.u += cp_len;

                    res.*.?.* = .{
                        .str = .{
                            .data = str.data[iter.i.u - cp_len .. iter.i.u],
                        },
                    };
                },
                .map => |map| {
                    if (iter.i.u == map.count()) {
                        return false;
                    }

                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc(.tuple);
                    }
                    if (iter.i.u == 0) {
                        res.*.?.* = .{ .tuple = try ctx.vm.gc.gpa.alloc(*Value, 2) };
                    }
                    // const e = &map.entries.items[iter.i.u];
                    const e_key = &map.keys()[iter.i.u].*;
                    const e_value = &map.values()[iter.i.u].*;
                    const t = res.*.?.tuple;
                    // removing `const` on `Map` causes dependency loop??
                    t[0] = @intToPtr(*Value, @ptrToInt(e_key));
                    t[1] = e_value;
                    iter.i.u += 1;
                },
                .range => |r| {
                    if (iter.i.i >= r.end) {
                        return false;
                    }
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc(.int);
                    }

                    res.*.?.* = .{
                        .int = iter.i.i,
                    };
                    iter.i.i += r.step;
                },
                else => unreachable,
            }
            return true;
        }
    };

    pub const Spread = struct {
        iterable: *Value,

        pub fn len(s: @This()) usize {
            return switch (s.iterable.*) {
                .range => unreachable, // Handled in Vm
                .str => unreachable, // Handled in Vm
                .tuple => |tuple| tuple.len,
                .list => |list| list.inner.items.len,
                else => unreachable,
            };
        }

        pub fn items(s: @This()) []*Value {
            return switch (s.iterable.*) {
                .range => unreachable, // Handled in Vm
                .str => unreachable, // Handled in Vm
                .tuple => |tuple| tuple,
                .list => |list| list.inner.items,
                else => unreachable,
            };
        }
    };

    pub const Func = struct {
        /// module in which this function exists
        module: *bog.Bytecode,
        captures_ptr: [*]*Value,
        body_len: u32,
        extra_index: u32,

        pub fn args(f: Func) u32 {
            return @enumToInt(f.module.extra[f.extra_index]) & std.math.maxInt(u31);
        }

        pub fn variadic(f: Func) bool {
            return (@enumToInt(f.module.extra[f.extra_index]) >> 31) != 0;
        }

        pub fn captures(f: Func) []*Value {
            return f.captures_ptr[0..@enumToInt(f.module.extra[f.extra_index + 1])];
        }

        pub fn body(f: Func) []const u32 {
            return @bitCast([]const u32, f.module.extra[f.extra_index + 2 + @enumToInt(f.module.extra[f.extra_index + 1]) ..][0..f.body_len]);
        }
    };

    pub const Range = struct {
        start: i64 = 0,
        end: i64 = std.math.maxInt(i64),
        step: i64 = 1,

        pub fn count(r: Range) u64 {
            return @intCast(u64, @divFloor(r.end - r.start - 1, r.step));
        }

        pub fn iterator(r: Range) Range.Iterator {
            return .{ .r = r, .i = r.start };
        }

        pub const Iterator = struct {
            r: Range,
            i: i64,

            pub fn next(iter: *Range.Iterator) ?i64 {
                if (iter.i >= iter.r.end) {
                    return null;
                }

                defer iter.i += iter.r.step;
                return iter.i;
            }
        };
    };

    pub const String = @import("String.zig");
    pub const Map = @import("Map.zig");
    pub const List = @import("List.zig");
    pub const Native = struct {
        arg_count: u8,
        variadic: bool,
        func: fn (Vm.Context, []*Value) NativeError!*Value,
    };
    pub const NativeError = Vm.Error || error{Throw};
    pub const NativeVal = struct {
        vtable: *const VTable,
        ptr: *anyopaque,

        pub const VTable = struct {
            typeName: fn (*anyopaque) []const u8,
            deinit: fn (*anyopaque, Allocator) void,
            eql: fn (*anyopaque, *const Value) bool,

            markVal: ?fn (*anyopaque, *bog.Gc) void,
            hash: ?fn (*anyopaque) u32 = null,
            get: ?fn (*anyopaque, Vm.Context, index: *const Value, res: *?*Value) NativeError!void = null,
            set: ?fn (*anyopaque, Vm.Context, index: *const Value, new_val: *Value) NativeError!void = null,
            contains: ?fn (*anyopaque, *const Value) bool = null,
        };

        pub inline fn unwrap(ptr: *anyopaque, comptime T: type) *T {
            return @ptrCast(*T, @alignCast(@alignOf(T), ptr));
        }
    };

    /// Makes a distinct type which can be used as the parameter of a native function
    /// to easily get the value of `this`.
    pub fn This(comptime T: type) type {
        return struct {
            t: T,

            const __bog_This_T = T;
        };
    }

    /// Makes a distinct type which can be used as the parameter of a native function
    /// to make it variadic.
    pub fn Variadic(comptime T: type) type {
        return struct {
            t: []T,

            const __bog_Variadic_T = T;
        };
    }

    var null_base = Value{ .@"null" = {} };
    var true_base = Value{ .bool = true };
    var false_base = Value{ .bool = false };

    pub const Null = &null_base;
    pub const True = &true_base;
    pub const False = &false_base;

    pub fn string(data: anytype) Value {
        return switch (@TypeOf(data)) {
            []u8 => .{
                .str = .{
                    .data = data,
                    .capacity = data.len,
                },
            },
            else => .{
                .str = .{
                    .data = data,
                },
            },
        };
    }

    pub fn typeName(val: Value) []const u8 {
        return switch (val) {
            .@"null",
            .int,
            .num,
            .bool,
            .str,
            .tuple,
            .map,
            .list,
            .err,
            .range,
            .func,
            .tagged,
            => @tagName(val),
            .frame => "frame",
            .iterator => unreachable,
            .spread => unreachable,
            .native => "func",
            .native_val => |n| n.vtable.typeName(n.ptr),
        };
    }

    /// Frees any extra memory allocated by value.
    /// Does not free values recursively.
    pub fn deinit(value: *Value, allocator: Allocator) void {
        switch (value.*) {
            .bool, .@"null" => return,
            .frame => |f| {
                f.stack.deinit(allocator);
                f.err_handlers.deinit(allocator);
                allocator.destroy(f);
            },
            .int, .num, .native, .tagged, .range, .iterator, .err, .spread => {},
            .tuple => |t| allocator.free(t),
            .map => |*m| m.deinit(allocator),
            .list => |*l| l.deinit(allocator),
            .str => |*s| s.deinit(allocator),
            .func => |f| allocator.free(f.captures()),
            .native_val => |n| n.vtable.deinit(n.ptr, allocator),
        }
        value.* = undefined;
    }

    pub fn hash(key: *const Value) u32 {
        const autoHash = std.hash.autoHash;

        var hasher = std.hash.Wyhash.init(0);
        autoHash(&hasher, @as(Type, key.*));
        switch (key.*) {
            .iterator, .spread => unreachable,
            .frame => autoHash(&hasher, key.frame),
            .@"null" => {},
            .int => |int| autoHash(&hasher, int),
            .num => {},
            .bool => |b| autoHash(&hasher, b),
            .str => |str| hasher.update(str.data),
            .tuple => |tuple| {
                autoHash(&hasher, tuple.len);
                // TODO more hashing?
            },
            .map => |map| {
                autoHash(&hasher, map.count());
                // TODO more hashing?
            },
            .list => |list| {
                autoHash(&hasher, list.inner.items.len);
                // TODO more hashing?
            },
            .err => |err| autoHash(&hasher, @as(Type, err.*)),
            .range => |range| {
                autoHash(&hasher, range.start);
                autoHash(&hasher, range.end);
                autoHash(&hasher, range.step);
            },
            .func => |func| {
                autoHash(&hasher, func.extra_index);
                autoHash(&hasher, func.module);
            },
            .native => |func| {
                autoHash(&hasher, func.arg_count);
                autoHash(&hasher, func.func);
            },
            .tagged => |tagged| {
                hasher.update(tagged.name);
                autoHash(&hasher, tagged.value);
            },
            .native_val => |n| {
                if (n.vtable.hash) |some| {
                    return some(n.ptr);
                } else {
                    // TODO value not hashable?
                }
            },
        }
        return @truncate(u32, hasher.final());
    }

    pub fn eql(a: *const Value, b: *const Value) bool {
        switch (a.*) {
            .int => |i| return switch (b.*) {
                .int => |b_val| i == b_val,
                .num => |b_val| @intToFloat(f64, i) == b_val,
                else => false,
            },
            .num => |n| return switch (b.*) {
                .int => |b_val| n == @intToFloat(f64, b_val),
                .num => |b_val| n == b_val,
                else => false,
            },
            .native_val => |n| return n.vtable.eql(n.ptr, b),
            else => if (a.* != @as(std.meta.Tag(@TypeOf(b.*)), b.*)) return false,
        }
        return switch (a.*) {
            .iterator, .spread, .int, .num, .native_val => unreachable,
            .frame => a.frame == b.frame,
            .@"null" => true,
            .bool => |bool_val| bool_val == b.bool,
            .str => |s| s.eql(b.str),
            .tuple => |t| {
                const b_val = b.tuple;
                if (t.len != b_val.len) return false;
                for (t) |v, i| {
                    if (!v.eql(b_val[i])) return false;
                }
                return true;
            },
            .map => @panic("TODO eql for maps"),
            .list => |l| l.eql(b.list),
            .err => |e| e.eql(b.err),
            .range => |r| {
                return r.start == b.range.start and
                    r.end == b.range.end and
                    r.step == b.range.step;
            },
            .func => |f| {
                const b_f = b.func;
                return f.module == b_f.module and f.extra_index == b_f.extra_index;
            },
            .native => |n| n.func == b.native.func,
            .tagged => |t| {
                if (!mem.eql(u8, t.name, b.tagged.name)) return false;
                return t.value.eql(b.tagged.value);
            },
        };
    }

    /// Prints string representation of value to writer
    pub fn dump(value: *const Value, writer: anytype, level: u32) @TypeOf(writer).Error!void {
        switch (value.*) {
            .iterator, .spread => unreachable,
            .int => |i| try writer.print("{}", .{i}),
            .num => |n| try writer.print("{d}", .{n}),
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .@"null" => try writer.writeAll("null"),
            .range => |r| {
                try writer.print("{}:{}:{}", .{ r.start, r.end, r.step });
            },
            .tuple => |t| {
                if (level == 0) {
                    try writer.writeAll("(...)");
                } else {
                    try writer.writeByte('(');
                    for (t) |v, i| {
                        if (i != 0) try writer.writeAll(", ");
                        try v.dump(writer, level - 1);
                    }
                    try writer.writeByte(')');
                }
            },
            .map => |m| {
                if (level == 0) {
                    try writer.writeAll("{...}");
                } else {
                    try writer.writeByte('{');
                    var i: usize = 0;
                    var iter = m.iterator();
                    while (iter.next()) |entry| : (i += 1) {
                        if (i != 0)
                            try writer.writeAll(", ");
                        try entry.key_ptr.*.dump(writer, level - 1);
                        try writer.writeAll(" = ");
                        try entry.value_ptr.*.dump(writer, level - 1);
                    }
                    try writer.writeByte('}');
                }
            },
            .list => |l| {
                if (level == 0) {
                    try writer.writeAll("[...]");
                } else {
                    try writer.writeByte('[');
                    for (l.inner.items) |v, i| {
                        if (i != 0) try writer.writeAll(", ");
                        try v.dump(writer, level - 1);
                    }
                    try writer.writeByte(']');
                }
            },
            .err => |e| {
                if (level == 0) {
                    try writer.writeAll("error(...)");
                } else {
                    try writer.writeAll("error(");
                    try e.dump(writer, level - 1);
                    try writer.writeByte(')');
                }
            },
            .str => |s| try s.dump(writer),
            .func => |f| {
                try writer.print("fn({})@0x{X}[{}]", .{ f.args(), f.body()[0], f.captures().len });
            },
            .frame => |f| {
                try writer.print("frame@x{X}", .{f.body[0]});
            },
            .native => |n| {
                try writer.print("native({})@0x{}", .{ n.arg_count, @ptrToInt(n.func) });
            },
            .tagged => |t| {
                try writer.print("@{s}", .{t.name});
                if (level == 0) {
                    try writer.writeAll("(...)");
                } else if (t.value != Value.Null) {
                    try t.value.dump(writer, level - 1);
                }
            },
            .native_val => |n| {
                // TODO expose dump function
                try writer.writeAll(n.vtable.typeName(n.ptr));
            },
        }
    }

    /// Returns value in `container` at `index`.
    pub fn get(container: *const Value, ctx: Vm.Context, index: *const Value, res: *?*Value) NativeError!void {
        switch (container.*) {
            .tuple => |tuple| switch (index.*) {
                .int => {
                    var i = index.int;
                    if (i < 0)
                        i += @intCast(i64, tuple.len);
                    if (i < 0 or i >= tuple.len)
                        return ctx.throw("index out of bounds");

                    res.* = tuple[@intCast(u32, i)];
                },
                .range => |r| {
                    if (r.start < 0 or r.end > tuple.len)
                        return ctx.throw("index out of bounds");

                    res.* = try ctx.vm.gc.alloc(.list);
                    res.*.?.* = .{ .list = .{} };
                    const res_list = &res.*.?.*.list;
                    try res_list.inner.ensureUnusedCapacity(ctx.vm.gc.gpa, r.count());

                    var it = r.iterator();
                    while (it.next()) |some| {
                        res_list.inner.appendAssumeCapacity(tuple[@intCast(u32, some)]);
                    }
                },
                .str => |s| {
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc(.int);
                    }

                    if (mem.eql(u8, s.data, "len")) {
                        res.*.?.* = .{ .int = @intCast(i64, tuple.len) };
                    } else {
                        return ctx.throw("no such property");
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            .list => |list| return list.get(ctx, index, res),
            .map => |map| {
                res.* = map.get(index) orelse
                    return ctx.throwFmt("no value associated with key: {}", .{index.*});
            },
            .str => |str| return str.get(ctx, index, res),
            .iterator => unreachable,
            .range => |r| switch (index.*) {
                .str => |s| {
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc(.int);
                    }

                    if (mem.eql(u8, s.data, "start")) {
                        res.*.?.* = .{ .int = r.start };
                    } else if (mem.eql(u8, s.data, "end")) {
                        res.*.?.* = .{ .int = r.end };
                    } else if (mem.eql(u8, s.data, "step")) {
                        res.*.?.* = .{ .int = r.step };
                    } else {
                        return ctx.throw("no such property");
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            .frame => |f| switch (index.*) {
                .str => |s| {
                    if (mem.eql(u8, s.data, "finished")) {
                        res.* = if (f.result_val != null) Value.True else Value.False;
                    } else {
                        return ctx.throw("no such property");
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            .native_val => |n| if (n.vtable.get) |some| {
                return some(n.ptr, ctx, index, res);
            } else {
                return ctx.throw("invalid subscript type");
            },
            else => return ctx.throw("invalid subscript type"),
        }
    }

    /// Sets index of container to value. Does a shallow copy if value stored.
    pub fn set(container: *Value, ctx: Vm.Context, index: *const Value, new_val: *Value) NativeError!void {
        switch (container.*) {
            .tuple => |tuple| switch (index.*) {
                .int => {
                    var i = index.int;
                    if (i < 0)
                        i += @intCast(i64, tuple.len);
                    if (i < 0 or i >= tuple.len)
                        return ctx.throw("index out of bounds");

                    tuple[@intCast(u32, i)] = new_val;
                },
                .range => |r| {
                    if (r.start < 0 or r.end > tuple.len)
                        return ctx.throw("index out of bounds");

                    var it = r.iterator();
                    while (it.next()) |some| {
                        tuple[@intCast(u32, some)] = new_val;
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            .map => |*map| {
                _ = try map.put(ctx.vm.gc.gpa, try ctx.vm.gc.dupe(index), new_val);
            },
            .list => |*list| try list.set(ctx, index, new_val),
            .str => |*str| try str.set(ctx, index, new_val),
            .native_val => |n| if (n.vtable.set) |some| {
                return some(n.ptr, ctx, index, new_val);
            } else {
                return ctx.throw("invalid subscript type");
            },
            .iterator => unreachable,
            else => return ctx.throw("invalid subscript type"),
        }
    }

    /// `type_id` must be valid and cannot be .err, .range, .func or .native
    pub fn as(val: *Value, ctx: Vm.Context, type_id: Type) NativeError!*Value {
        if (type_id == .@"null") {
            return Value.Null;
        }
        if (val.* == type_id) {
            return val;
        }

        if (val.* == .str) {
            return val.str.as(ctx, type_id);
        } else if (type_id == .str) {
            return String.from(val, ctx.vm);
        }

        if (val.* == .list) {
            return val.list.as(ctx, type_id);
        } else if (type_id == .list) {
            return List.from(val, ctx);
        }

        if (type_id == .bool) {
            const bool_res = switch (val.*) {
                .int => |int| int != 0,
                .num => |num| num != 0,
                .bool => unreachable,
                .str => unreachable,
                else => return ctx.throwFmt("cannot cast {s} to bool", .{val.typeName()}),
            };

            return if (bool_res) Value.True else Value.False;
        }

        const new_val = try ctx.vm.gc.alloc(type_id);
        new_val.* = switch (type_id) {
            .int => .{
                .int = switch (val.*) {
                    .int => unreachable,
                    .num => |num| std.math.lossyCast(i64, num),
                    .bool => |b| @boolToInt(b),
                    .str => unreachable,
                    else => return ctx.throwFmt("cannot cast {s} to int", .{val.typeName()}),
                },
            },
            .num => .{
                .num = switch (val.*) {
                    .num => unreachable,
                    .int => |int| @intToFloat(f64, int),
                    .bool => |b| @intToFloat(f64, @boolToInt(b)),
                    .str => unreachable,
                    else => return ctx.throwFmt("cannot cast {s} to num", .{val.typeName()}),
                },
            },
            .str, .list, .bool, .@"null" => unreachable,
            .tuple, .map => return ctx.frame.fatal(ctx.vm, "TODO cast to tuple/map"),
            else => unreachable,
        };
        return new_val;
    }

    pub fn is(val: *const Value, type_id: Type) bool {
        if (val.* == type_id) return true;
        if (type_id == .func and val.* == .native) return true;
        return false;
    }

    /// Returns whether `container` has `val` in it.
    pub fn in(val: *const Value, container: *const Value) bool {
        switch (container.*) {
            .str => |str| return str.in(val),
            .tuple => |tuple| {
                for (tuple) |v| {
                    if (v.eql(val)) return true;
                }
                return false;
            },
            .list => |list| return list.in(val),
            .map => |map| return map.contains(val),
            .range => |r| {
                if (val.* != .int) return false;
                const int = val.int;
                if (int < r.start or int > r.end) return false;
                if (@rem(int - r.start, r.step) != 0) return false;
                return true;
            },
            .native_val => |n| if (n.vtable.contains) |some| {
                return some(n.ptr, val);
            } else {
                return false; // TODO throw
            },
            .iterator => unreachable,
            else => unreachable,
        }
    }

    pub fn iterator(val: *const Value, ctx: Vm.Context) NativeError!*Value {
        var start: ?i64 = null;
        switch (val.*) {
            .range => |r| start = r.start,
            .str, .tuple, .list, .map => {},
            .iterator => unreachable,
            else => return ctx.throwFmt("cannot iterate {s}", .{val.typeName()}),
        }
        const iter = try ctx.vm.gc.alloc(.iterator);
        iter.* = .{
            .iterator = .{
                .value = try ctx.vm.gc.dupe(val),
            },
        };
        if (start) |some| iter.iterator.i.i = some;
        return iter;
    }

    /// Converts Zig value to Bog value. Allocates copy in the gc.
    pub fn zigToBog(vm: *Vm, val: anytype) error{OutOfMemory}!*Value {
        if (comptime std.meta.trait.hasFn("intoBog")(@TypeOf(val))) {
            return try val.intoBog(vm);
        }

        switch (@TypeOf(val)) {
            void => return Value.Null,
            bool => return if (val) Value.True else Value.False,
            *Value => return val,
            Value => {
                const ret = try vm.gc.alloc(val);
                ret.* = val;
                return ret;
            },
            []const u8, []u8 => {
                // assume val was allocated with vm.gc
                const str = try vm.gc.alloc(.str);
                str.* = Value.string(val);
                return str;
            },
            String => {
                const str = try vm.gc.alloc(.str);
                str.* = Value{ .str = val };
                return str;
            },
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    comptime var pub_decls = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) pub_decls += 1;
                    }

                    const res = try vm.gc.alloc(.map);
                    res.* = .{ .map = .{} };
                    try res.map.ensureTotalCapacity(vm.gc.gpa, pub_decls);

                    inline for (info.decls) |decl| {
                        if (!decl.is_pub) continue;
                        // skip common interfaces
                        if (comptime std.mem.eql(u8, decl.name, "intoBog")) continue;
                        if (comptime std.mem.eql(u8, decl.name, "fromBog")) continue;
                        if (comptime std.mem.eql(u8, decl.name, "format")) continue;

                        const key = try vm.gc.alloc(.str);
                        key.* = Value.string(decl.name);

                        const value = if (@typeInfo(@TypeOf(@field(val, decl.name))) == .Fn)
                            try zigFnToBog(vm, @field(val, decl.name))
                        else
                            try zigToBog(vm, @field(val, decl.name));
                        res.map.putAssumeCapacityNoClobber(key, value);
                    }

                    return res;
                },
                else => @compileError("unsupported type: " ++ @typeName(val)),
            },
            else => switch (@typeInfo(@TypeOf(val))) {
                .Pointer => |info| {
                    if (info.size == .Slice) @compileError("unsupported type: " ++ @typeName(val));
                    const int = try vm.gc.alloc(.int);
                    int.* = .{
                        .int = @bitCast(isize, @ptrToInt(val)),
                    };
                    return int;
                },
                .Fn => @compileError("use zigFnToBog"),
                .ComptimeInt, .Int => {
                    const int = try vm.gc.alloc(.int);
                    int.* = .{
                        // try to implicit cast the value
                        .int = val,
                    };
                    return int;
                },
                .ComptimeFloat, .Float => {
                    const num = try vm.gc.alloc(.num);
                    num.* = .{
                        // try to implicit cast the value
                        .num = val,
                    };
                    return num;
                },
                .ErrorUnion => if (val) |some| {
                    return zigToBog(vm, some);
                } else |e| {
                    // wrap error string
                    const str = try vm.gc.alloc(.str);
                    str.* = Value.string(@errorName(e));
                    const err = try vm.gc.alloc(.err);
                    err.* = .{ .err = str };
                    return err;
                },
                .Enum => {
                    const tag = try vm.gc.alloc(.tagged);
                    tag.* = .{
                        .tagged = .{
                            .name = @tagName(val),
                            .value = Value.Null,
                        },
                    };
                    return tag;
                },
                .Optional => if (val) |some| {
                    return zigToBog(vm, some);
                } else {
                    return Value.Null;
                },
                .Struct => {
                    const T = @TypeOf(val);
                    const S = struct {
                        const vtable = NativeVal.VTable{
                            .typeName = T.typeName,
                            .deinit = T.deinit,
                            .eql = T.eql,

                            .markVal = if (@hasDecl(T, "markVal")) T.markVal else null,
                            .hash = if (@hasDecl(T, "hash")) T.hash else null,
                            .get = if (@hasDecl(T, "get")) T.get else null,
                            .set = if (@hasDecl(T, "set")) T.set else null,
                            .contains = if (@hasDecl(T, "contains")) T.contains else null,
                        };
                    };
                    const ptr = try vm.gc.gpa.create(T);
                    errdefer vm.gc.gpa.destroy(ptr);
                    ptr.* = val;

                    const native = try vm.gc.alloc(.native_val);
                    native.* = .{ .native_val = .{
                        .vtable = &S.vtable,
                        .ptr = ptr,
                    } };
                    return native;
                },
                else => @compileError("unsupported type: " ++ @typeName(@TypeOf(val))),
            },
        }
    }

    pub fn zigFnToBog(vm: *Vm, comptime func: anytype) error{OutOfMemory}!*Value {
        const Fn = @typeInfo(@TypeOf(func)).Fn;
        if (Fn.is_generic) @compileError("cannot wrap a generic function");

        @setEvalBranchQuota(Fn.args.len * 1000);

        const S = struct {
            fn native(ctx: Vm.Context, bog_args: []*Value) Value.NativeError!*Value {
                var args: std.meta.ArgsTuple(@TypeOf(func)) = undefined;

                comptime var bog_arg_i: u8 = 0;
                comptime var vm_passed = false;
                comptime var this_passed = false;
                comptime var variadic = false;
                inline for (Fn.args) |arg, i| {
                    const ArgT = arg.arg_type.?;
                    if (variadic) @compileError("Value.Variadic must be the last parameter");
                    if (ArgT == Vm.Context) {
                        if (vm_passed) @compileError("function takes more than one Vm.Context");
                        // if (i == 1 and !this_passed)
                        //     @compileError("Vm.Context must come after Value.This")
                        // else if (i != 0)
                        //     @compileError("Vm.Context must be first or second parameter");

                        args[i] = ctx;
                        vm_passed = true;
                    } else if (@typeInfo(ArgT) == .Struct and @hasDecl(ArgT, "__bog_This_T")) {
                        if (i != 0) @compileError("Value.This must be the first parameter");
                        args[i] = ArgT{ .t = try ctx.this.bogToZig(ArgT.__bog_This_T, ctx) };
                        this_passed = true;
                    } else if (@typeInfo(ArgT) == .Struct and @hasDecl(ArgT, "__bog_Variadic_T")) {
                        variadic = true;
                        const args_tuple_size = bog_args.len - bog_arg_i;
                        const args_tuple = try ctx.vm.gc.gpa.alloc(ArgT.__bog_Variadic_T, args_tuple_size);
                        errdefer ctx.vm.gc.gpa.free(args_tuple);
                        args[i] = ArgT{ .t = args_tuple };

                        for (bog_args[bog_arg_i..]) |val, tuple_i| {
                            args_tuple[tuple_i] = try val.bogToZig(ArgT.__bog_Variadic_T, ctx);
                        }
                    } else {
                        args[i] = try bog_args[bog_arg_i].bogToZig(ArgT, ctx);
                        bog_arg_i += 1;
                    }
                }
                const res = @call(.{}, func, args);
                if (variadic) ctx.vm.gc.gpa.free(args[args.len - 1].t);
                switch (@typeInfo(@TypeOf(res))) {
                    .ErrorSet => switch (@as(anyerror, res)) {
                        error.FatalError, error.Throw => |e| return e,
                        else => |e| return ctx.throw(@errorName(e)),
                    },
                    .ErrorUnion => if (res) |val| {
                        return Value.zigToBog(ctx.vm, val);
                    } else |err| switch (@as(anyerror, err)) {
                        error.FatalError, error.Throw => |e| return e,
                        else => |e| return ctx.throw(@errorName(e)),
                    },
                    else => return Value.zigToBog(ctx.vm, res),
                }
            }
        };

        // TODO can't use bog_arg_i due to a stage1 bug.
        comptime var bog_arg_count = 0;
        comptime var variadic = false;
        comptime for (Fn.args) |arg| {
            const ArgT = arg.arg_type.?;
            if (@typeInfo(ArgT) == .Struct and @hasDecl(ArgT, "__bog_Variadic_T")) {
                variadic = true;
            } else if (ArgT != Vm.Context and !(@typeInfo(ArgT) == .Struct and @hasDecl(ArgT, "__bog_This_T"))) {
                bog_arg_count += 1;
            }
        };

        const native = try vm.gc.alloc(.native);
        native.* = .{
            .native = .{
                .arg_count = bog_arg_count,
                .variadic = variadic,
                .func = S.native,
            },
        };
        return native;
    }

    /// Converts Bog value to Zig value. Returned string is invalidated
    /// on next garbage collection.
    pub fn bogToZig(val: *Value, comptime T: type, ctx: Vm.Context) NativeError!T {
        if (comptime std.meta.trait.hasFn("fromBog")(T)) {
            return try T.fromBog(val, ctx);
        }

        return switch (T) {
            void => {
                if (val.* != .@"null")
                    return ctx.throw("expected a null");
            },
            bool => {
                if (val.* != .bool)
                    return ctx.throw("expected a bool");
                return val.bool;
            },
            []const u8 => {
                if (val.* != .str)
                    return ctx.throw("expected a string");
                return val.str.data;
            },
            *Map, *const Map => {
                if (val.* != .map)
                    return ctx.throw("expected a map");
                return &val.map;
            },
            *List, *const List => {
                if (val.* != .list)
                    return ctx.throw("expected a list");
                return &val.list;
            },
            *Value, *const Value => val,
            *String, *const String => {
                if (val.* != .str)
                    return ctx.throw("expected a string");
                return &val.str;
            },
            []*Value, []const *Value, []*const Value, []const *const Value => {
                switch (val.*) {
                    .tuple => |t| return t,
                    .list => |*l| return l.items,
                    else => return ctx.throw("expected a list or a tuple"),
                }
            },
            else => switch (@typeInfo(T)) {
                .Int => switch (val.*) {
                    .int => |int| {
                        if (int < std.math.minInt(T) or int > std.math.maxInt(T))
                            return ctx.throw("cannot fit int in desired type");
                        return @intCast(T, int);
                    },
                    .num => |num| std.math.lossyCast(T, num),
                    else => return ctx.throw("expected int"),
                },
                .Float => |info| switch (info.bits) {
                    32 => switch (val.*) {
                        .num => |num| @floatCast(f32, num),
                        .int => |int| @intToFloat(f32, int),
                        else => return ctx.throw("expected num"),
                    },
                    64 => switch (val.*) {
                        .num => |num| num,
                        .int => |int| @intToFloat(f64, int),
                        else => return ctx.throw("expected num"),
                    },
                    else => @compileError("unsupported float"),
                },
                .Enum => {
                    if (val.* != .tagged)
                        return ctx.throw("expected tag");
                    const e = std.meta.stringToEnum(T, val.tagged.name) orelse
                        return ctx.throw("no value by such name");
                    if (val.tagged.value.* != .@"null")
                        return ctx.throw("expected no value");
                    return e;
                },
                else => @compileError("unsupported type: " ++ @typeName(T)),
            },
        };
    }

    pub fn jsonStringify(val: *const Value, options: std.json.StringifyOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (val.*) {
            .@"null" => try writer.writeAll("null"),
            .tuple => |t| {
                try writer.writeByte('[');
                for (t) |e, i| {
                    if (i != 0) try writer.writeByte(',');
                    try e.jsonStringify(options, writer);
                }
                try writer.writeByte(']');
            },
            .list => |*l| {
                try writer.writeByte('[');
                for (l.inner.items) |e, i| {
                    if (i != 0) try writer.writeByte(',');
                    try e.jsonStringify(options, writer);
                }
                try writer.writeByte(']');
            },
            .map => |*m| {
                try writer.writeByte('{');
                var i: usize = 0;
                var iter = m.iterator();
                while (iter.next()) |entry| : (i += 1) {
                    if (i != 0)
                        try writer.writeAll(", ");

                    try entry.key_ptr.*.jsonStringify(options, writer);
                    try writer.writeAll(":");
                    try entry.value_ptr.*.jsonStringify(options, writer);
                }
                try writer.writeByte('}');
            },

            .int,
            .num,
            .bool,
            => try val.dump(writer, 0),
            .str => |s| {
                try writer.print("\"{}\"", .{std.zig.fmtEscapes(s.data)});
            },
            .native,
            .func,
            .frame,
            .range,
            .err,
            .tagged,
            .native_val,
            => {
                try writer.writeByte('\"');
                try val.dump(writer, 0);
                try writer.writeByte('\"');
            },
            .iterator, .spread => unreachable,
        }
    }
};

var buffer: [1024]u8 = undefined;

fn testDump(val: Value, expected: []const u8) !void {
    var fbs = std.io.fixedBufferStream(&buffer);

    val.dump(fbs.writer(), 4) catch @panic("test failed");
    try std.testing.expectEqualStrings(expected, fbs.getWritten());
}

test "dump int/num" {
    var int = Value{
        .int = 2,
    };
    try testDump(int, "2");
    var num = Value{
        .num = 2.5,
    };
    try testDump(num, "2.5");
}

test "dump error" {
    var int = Value{
        .int = 2,
    };
    var err = Value{
        .err = &int,
    };
    try testDump(err, "error(2)");
}
