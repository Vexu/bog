const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Module = bog.Module;
const util = @import("util.zig");

pub const Type = enum(u8) {
    none = 0,
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

    /// pseudo type user should not have access to via valid bytecode
    iterator,

    /// native being separate from .func is an implementation detail
    native,
    _,
};

pub const Value = union(Type) {
    tuple: []*Value,
    map: Map,
    list: List,
    err: *Value,
    int: i64,
    num: f64,
    range: struct {
        start: i64 = 0,
        end: i64 = std.math.maxInt(i64),
        step: i64 = 1,
    },
    str: String,
    func: struct {
        /// offset to the functions first instruction
        offset: u32,
        arg_count: u8,

        /// module in which this function exists
        module: *Module,

        captures: []*Value,
    },
    native: Native,
    tagged: struct {
        name: []const u8,
        value: *Value,
    },
    iterator: struct {
        value: *Value,
        i: packed union {
            u: usize,
            i: i64,
        } = .{ .u = 0 },

        pub fn next(iter: *@This(), vm: *Vm, res: *?*Value) !void {
            switch (iter.value.*) {
                .tuple => |tuple| {
                    if (iter.i.u == tuple.len) {
                        res.* = &Value.None;
                        return;
                    }

                    res.* = tuple[iter.i.u];
                    iter.i.u += 1;
                },
                .list => |*list| {
                    if (iter.i.u == list.items.len) {
                        res.* = &Value.None;
                        return;
                    }

                    res.* = list.items[iter.i.u];
                    iter.i.u += 1;
                },
                .str => |*str| {
                    if (iter.i.u == str.data.len) {
                        res.* = &Value.None;
                        return;
                    }
                    if (res.* == null)
                        res.* = try vm.gc.alloc();

                    const cp_len = std.unicode.utf8ByteSequenceLength(str.data[iter.i.u]) catch
                        return vm.fatal("invalid utf-8 sequence");
                    iter.i.u += cp_len;

                    res.*.?.* = .{
                        .str = .{
                            .data = str.data[iter.i.u - cp_len .. iter.i.u],
                        },
                    };
                },
                .map => |*map| {
                    if (iter.i.u == map.count()) {
                        res.* = &Value.None;
                        return;
                    }

                    if (res.* == null)
                        res.* = try vm.gc.alloc();
                    if (iter.i.u == 0) {
                        res.*.?.* = .{ .tuple = try vm.gc.gpa.alloc(*Value, 2) };
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
                .range => {
                    if (iter.i.i >= iter.value.range.end) {
                        res.* = &Value.None;
                        return;
                    }
                    if (res.* == null)
                        res.* = try vm.gc.alloc();

                    res.*.?.* = .{
                        .int = iter.i.i,
                    };
                    iter.i.i += iter.value.range.step;
                },
                else => unreachable,
            }
        }
    },

    /// always memoized
    bool: bool,
    none,

    pub const String = @import("String.zig");

    const ValueMapContext = struct {
        pub fn hash(self: @This(), v: *const Value) u32 {
            _ = self;
            return Value.hash(v);
        }

        pub fn eql(self: @This(), a: *const Value, b: *const Value) bool {
            _ = self;
            return Value.eql(a, b);
        }
    };

    pub const Map = std.array_hash_map.ArrayHashMapUnmanaged(*const Value, *Value, Value.ValueMapContext, true);
    pub const List = std.ArrayListUnmanaged(*Value);
    pub const Native = struct {
        arg_count: u8,
        func: fn (*Vm, []*Value) Vm.Error!*Value,
    };

    pub var None = Value{ .none = {} };
    pub var True = Value{ .bool = true };
    pub var False = Value{ .bool = false };

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

    /// Frees any extra memory allocated by value.
    /// Does not free values recursively.
    pub fn deinit(value: *Value, allocator: *Allocator) void {
        switch (value.*) {
            .bool, .none => return,
            .int, .num, .native, .tagged, .range, .iterator, .err => {},
            .tuple => |t| allocator.free(t),
            .map => |*m| m.deinit(allocator),
            .list => |*l| l.deinit(allocator),
            .str => |*s| s.deinit(allocator),
            .func => |*f| allocator.free(f.captures),
        }
        value.* = undefined;
    }

    pub fn hash(key: *const Value) u32 {
        const autoHash = std.hash.autoHash;
        const autoHashStrat = std.hash.autoHashStrat;

        var hasher = std.hash.Wyhash.init(0);
        autoHash(&hasher, @as(Type, key.*));
        switch (key.*) {
            .iterator => unreachable,
            .none => {},
            .int => |int| autoHash(&hasher, int),
            .num => {},
            .bool => |b| autoHash(&hasher, b),
            .str => |*str| hasher.update(str.data),
            .tuple => |tuple| {
                autoHash(&hasher, tuple.len);
                autoHash(&hasher, tuple.ptr);
            },
            .map => |*map| {
                autoHash(&hasher, map.count());
                autoHashStrat(&hasher, map.keys(), .Shallow);
                autoHashStrat(&hasher, map.values(), .Shallow);
                autoHash(&hasher, map.index_header);
            },
            .list => |*list| {
                autoHash(&hasher, list.items.len);
                autoHash(&hasher, list.items.ptr);
            },
            .err => |err| autoHash(&hasher, @as(Type, err.*)),
            .range => |*range| {
                autoHash(&hasher, range.start);
                autoHash(&hasher, range.end);
                autoHash(&hasher, range.step);
            },
            .func => |*func| {
                autoHash(&hasher, func.offset);
                autoHash(&hasher, func.arg_count);
                autoHash(&hasher, func.module);
            },
            .native => |*func| {
                autoHash(&hasher, func.arg_count);
                autoHash(&hasher, func.func);
            },
            .tagged => |*tagged| {
                hasher.update(tagged.name);
                autoHash(&hasher, tagged.value);
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
            else => if (a.* != @as(std.meta.TagType(@TypeOf(b.*)), b.*)) return false,
        }
        return switch (a.*) {
            .iterator, .int, .num => unreachable,
            .none => true,
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
            .map => |_| @panic("TODO eql for maps"),
            .list => |*l| {
                if (l.items.len != b.list.items.len) return false;
                for (l.items) |v, i| {
                    if (!v.eql(b.list.items[i])) return false;
                }
                return true;
            },
            .err => |e| e.eql(b.err),
            .range => |*r| {
                return r.start == b.range.start and
                    r.end == b.range.end and
                    r.step == b.range.step;
            },
            .func => |*f| {
                return f.offset == b.func.offset and
                    f.arg_count == b.func.arg_count and
                    f.module == b.func.module;
            },
            .native => |*n| n.func == b.native.func,
            .tagged => |*t| {
                if (!mem.eql(u8, t.name, b.tagged.name)) return false;
                return t.value.eql(b.tagged.value);
            },
        };
    }

    /// Prints string representation of value to writer
    pub fn dump(value: *const Value, writer: anytype, level: u32) @TypeOf(writer).Error!void {
        switch (value.*) {
            .iterator => unreachable,
            .int => |i| try writer.print("{}", .{i}),
            .num => |n| try writer.print("{d}", .{n}),
            .bool => |b| try writer.writeAll(if (b) "true" else "false"),
            .none => try writer.writeAll("()"),
            .range => |*r| {
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
            .map => |*m| {
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
                        try writer.writeAll(": ");
                        try entry.value_ptr.*.dump(writer, level - 1);
                    }
                    try writer.writeByte('}');
                }
            },
            .list => |*l| {
                if (level == 0) {
                    try writer.writeAll("[...]");
                } else {
                    try writer.writeByte('[');
                    for (l.items) |v, i| {
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
            .func => |*f| {
                try writer.print("fn({})@0x{X}[{}]", .{ f.arg_count, f.offset, f.captures.len });
            },
            .native => |*n| {
                try writer.print("native({})@0x{}", .{ n.arg_count, @ptrToInt(n.func) });
            },
            .tagged => |*t| {
                try writer.print("@{s}", .{t.name});
                if (level == 0) {
                    try writer.writeAll("(...)");
                } else {
                    try t.value.dump(writer, level - 1);
                }
            },
        }
    }

    /// Returns value in `container` at `index`.
    pub fn get(container: *const Value, vm: *Vm, index: *const Value, res: *?*Value) Vm.Error!void {
        switch (container.*) {
            .tuple => |tuple| switch (index.*) {
                .int => {
                    var i = index.int;
                    if (i < 0)
                        i += @intCast(i64, tuple.len);
                    if (i < 0 or i >= tuple.len)
                        return vm.fatal("index out of bounds");

                    res.* = tuple[@intCast(u32, i)];
                },
                .range => return vm.fatal("TODO get with ranges"),
                .str => |*s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s.data, "len")) {
                        res.*.?.* = .{ .int = @intCast(i64, tuple.len) };
                    } else {
                        return vm.fatal("no such property");
                    }
                },
                else => return vm.fatal("invalid index type"),
            },
            .list => |*list| switch (index.*) {
                .int => {
                    var i = index.int;
                    if (i < 0)
                        i += @intCast(i64, list.items.len);
                    if (i < 0 or i >= list.items.len)
                        return vm.fatal("index out of bounds");

                    res.* = list.items[@intCast(u32, i)];
                },
                .range => return vm.fatal("TODO get with ranges"),
                .str => |*s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s.data, "len")) {
                        res.*.?.* = .{ .int = @intCast(i64, list.items.len) };
                    } else if (mem.eql(u8, s.data, "append")) {
                        res.* = try zigToBog(vm, struct {
                            fn append(_vm: *Vm, val: *Value) !void {
                                if (_vm.last_get.* != .list)
                                    return _vm.fatal("expected list");
                                try _vm.last_get.list.append(_vm.gc.gpa, try _vm.gc.dupe(val));
                            }
                        }.append);
                    } else {
                        return vm.fatal("no such property");
                    }
                },
                else => return vm.fatal("invalid index type"),
            },
            .map => |*map| {
                res.* = map.get(index) orelse
                    return vm.fatal("TODO better handling undefined key");
            },
            .str => |*str| return str.get(vm, index, res),
            .iterator => unreachable,
            .range => |*r| switch (index.*) {
                .str => |*s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s.data, "start")) {
                        res.*.?.* = .{ .int = r.start };
                    } else if (mem.eql(u8, s.data, "end")) {
                        res.*.?.* = .{ .int = r.end };
                    } else if (mem.eql(u8, s.data, "step")) {
                        res.*.?.* = .{ .int = r.step };
                    } else {
                        return vm.fatal("no such property");
                    }
                },
                else => return vm.fatal("invalid index type"),
            },
            else => return vm.fatal("invalid subscript type"),
        }
    }

    /// Sets index of container to value. Does a shallow copy if value stored.
    pub fn set(container: *Value, vm: *Vm, index: *const Value, new_val: *const Value) Vm.Error!void {
        switch (container.*) {
            .tuple => |tuple| if (index.* == .int) {
                var i = index.int;
                if (i < 0)
                    i += @intCast(i64, tuple.len);
                if (i < 0 or i >= tuple.len)
                    return vm.fatal("index out of bounds");

                tuple[@intCast(u32, i)] = try vm.gc.dupe(new_val);
            } else {
                return vm.fatal("TODO set with ranges");
            },
            .map => |*map| {
                _ = try map.put(vm.gc.gpa, try vm.gc.dupe(index), try vm.gc.dupe(new_val));
            },
            .list => |*list| if (index.* == .int) {
                var i = index.int;
                if (i < 0)
                    i += @intCast(i64, list.items.len);
                if (i < 0 or i >= list.items.len)
                    return vm.fatal("index out of bounds");

                list.items[@intCast(u32, i)] = try vm.gc.dupe(new_val);
            } else {
                return vm.fatal("TODO set with ranges");
            },
            .str => |*str| try str.set(vm, index, new_val),
            .iterator => unreachable,
            else => return vm.fatal("invalid subscript type"),
        }
    }

    /// `type_id` must be valid and cannot be .err, .range, .func or .native
    pub fn as(val: *Value, vm: *Vm, type_id: Type) Vm.Error!*Value {
        if (type_id == .none) {
            return &Value.None;
        }
        if (val.* == type_id) {
            return val;
        }

        if (val.* == .str) {
            return val.str.as(vm, type_id);
        }

        if (type_id == .bool) {
            const bool_res = switch (val.*) {
                .int => |int| int != 0,
                .num => |num| num != 0,
                .bool => unreachable,
                .str => unreachable,
                else => return vm.errorFmt("cannot cast {s} to bool", .{@tagName(val.*)}),
            };

            return if (bool_res) &Value.True else &Value.False;
        } else if (type_id == .str) {
            return String.from(val, vm);
        }

        const new_val = try vm.gc.alloc();
        new_val.* = switch (type_id) {
            .int => .{
                .int = switch (val.*) {
                    .int => unreachable,
                    .num => |num| @floatToInt(i64, num),
                    .bool => |b| @boolToInt(b),
                    .str => unreachable,
                    else => return vm.errorFmt("cannot cast {s} to int", .{@tagName(val.*)}),
                },
            },
            .num => .{
                .num = switch (val.*) {
                    .num => unreachable,
                    .int => |int| @intToFloat(f64, int),
                    .bool => |b| @intToFloat(f64, @boolToInt(b)),
                    .str => unreachable,
                    else => return vm.errorFmt("cannot cast {s} to num", .{@tagName(val.*)}),
                },
            },
            .str, .bool, .none => unreachable,
            .tuple,
            .map,
            .list,
            => return vm.fatal("TODO more casts"),
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
            .str => |*str| return str.in(val),
            .tuple => |tuple| {
                for (tuple) |v| {
                    if (v.eql(val)) return true;
                }
                return false;
            },
            .list => |*list| {
                for (list.items) |v| {
                    if (v.eql(val)) return true;
                }
                return false;
            },
            .map => |*map| return map.contains(val),
            .range => |*r| {
                if (val.* != .int) return false;
                const int = val.int;
                if (int < r.start or int > r.end) return false;
                if (@rem(int - r.start, r.step) != 0) return false;
                return true;
            },
            .iterator => unreachable,
            else => unreachable,
        }
    }

    pub fn iterator(val: *const Value, vm: *Vm) Vm.Error!*Value {
        var start: ?i64 = null;
        switch (val.*) {
            .range => |*r| start = r.start,
            .str, .tuple, .list, .map => {},
            .iterator => unreachable,
            else => return vm.errorFmt("cannot iterate {s}", .{@tagName(val.*)}),
        }
        const iter = try vm.gc.alloc();
        iter.* = .{
            .iterator = .{
                .value = try vm.gc.dupe(val),
            },
        };
        if (start) |some| iter.iterator.i.i = some;
        return iter;
    }

    /// Converts Zig value to Bog value. Allocates copy in the gc.
    pub fn zigToBog(vm: *Vm, val: anytype) Vm.Error!*Value {
        if (comptime std.meta.trait.hasFn("intoBog")(@TypeOf(val))) {
            return try val.intoBog(vm);
        }

        switch (@TypeOf(val)) {
            void => return &Value.None,
            bool => return if (val) &Value.True else &Value.False,
            *Value => return val,
            Value => {
                const ret = try vm.gc.alloc();
                ret.* = val;
                return ret;
            },
            []const u8, []u8 => {
                // assume val was allocated with vm.gc
                const str = try vm.gc.alloc();
                str.* = Value.string(val);
                return str;
            },
            String => {
                const str = try vm.gc.alloc();
                str.* = Value{ .str = val };
                return str;
            },
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    var map = Value.Map{};
                    errdefer map.deinit(vm.gc.gpa);

                    comptime var pub_decls = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) pub_decls += 1;
                    }

                    try map.ensureCapacity(vm.gc.gpa, pub_decls);

                    inline for (info.decls) |decl| {
                        if (!decl.is_pub) continue;
                        // skip common interfaces
                        if (comptime std.mem.eql(u8, decl.name, "intoBog")) continue;
                        if (comptime std.mem.eql(u8, decl.name, "fromBog")) continue;
                        if (comptime std.mem.eql(u8, decl.name, "format")) continue;

                        const key = try vm.gc.alloc();
                        key.* = Value.string(decl.name);
                        const value = try zigToBog(vm, @field(val, decl.name));
                        map.putAssumeCapacityNoClobber(key, value);
                    }

                    const ret = try vm.gc.alloc();
                    ret.* = .{
                        .map = map,
                    };
                    return ret;
                },
                else => @compileError("unsupported type: " ++ @typeName(val)),
            },
            else => switch (@typeInfo(@TypeOf(val))) {
                .Pointer => |info| {
                    if (info.size == .Slice) @compileError("unsupported type: " ++ @typeName(val));
                    const int = try vm.gc.alloc();
                    int.* = .{
                        .int = @bitCast(isize, @ptrToInt(val)),
                    };
                    return int;
                },
                .Fn => {
                    const native = try vm.gc.alloc();
                    native.* = .{
                        .native = wrapZigFunc(val),
                    };
                    return native;
                },
                .ComptimeInt, .Int => {
                    const int = try vm.gc.alloc();
                    int.* = .{
                        // try to implicit cast the value
                        .int = val,
                    };
                    return int;
                },
                .ComptimeFloat, .Float => {
                    const num = try vm.gc.alloc();
                    num.* = .{
                        // try to implicit cast the value
                        .num = val,
                    };
                    return num;
                },
                .ErrorUnion => if (val) |some| {
                    return zigToBog(vm, some);
                } else |e| {
                    // capture runtime errors
                    if (@as(anyerror, e) == error.RuntimeError)
                        return error.RuntimeError;

                    // wrap error string
                    const str = try vm.gc.alloc();
                    str.* = Value.string(@errorName(e));
                    const err = try vm.gc.alloc();
                    err.* = .{ .err = str };
                    return err;
                },
                .Enum => {
                    const tag = try vm.gc.alloc();
                    tag.* = .{
                        .tagged = .{
                            .name = @tagName(val),
                            .value = &None,
                        },
                    };
                    return tag;
                },
                .Optional => if (val) |some| {
                    return zigToBog(vm, some);
                } else {
                    return &Value.None;
                },
                else => @compileError("unsupported type: " ++ @typeName(@TypeOf(val))),
            },
        }
    }

    /// Converts Bog value to Zig value. Returned string is invalidated
    /// on next garbage collection.
    pub fn bogToZig(val: *Value, comptime T: type, vm: *Vm) Vm.Error!T {
        if (comptime std.meta.trait.hasFn("fromBog")(T)) {
            return try T.fromBog(val, vm);
        }

        return switch (T) {
            void => {
                if (val.* != .none)
                    return vm.fatal("expected none");
            },
            bool => {
                if (val.* != .bool)
                    return vm.fatal("expected bool");
                return val.bool;
            },
            []const u8 => {
                if (val.* != .str)
                    return vm.fatal("expected string");
                return val.str.data;
            },
            *Map, *const Map => {
                if (val.* != .map)
                    return vm.fatal("expected map");
                return &val.map;
            },
            *Vm => vm,
            *Value, *const Value => val,
            Value => return val.*,
            String => {
                if (val.* != .str)
                    return vm.fatal("expected string");
                return val.str;
            },
            []*Value, []const *Value, []*const Value, []const *const Value => {
                switch (val.*) {
                    .tuple => |t| return t,
                    .list => |*l| return l.items,
                    else => return vm.fatal("expected a list or a tuple"),
                }
            },
            else => switch (@typeInfo(T)) {
                .Int => if (val.* == .int) blk: {
                    if (val.int < std.math.minInt(T) or val.int > std.math.maxInt(T))
                        return vm.fatal("cannot fit int in desired type");
                    break :blk @intCast(T, val.int);
                } else if (val.* == .num)
                    @floatToInt(T, val.num)
                else
                    return vm.fatal("expected int"),
                .Float => |info| switch (info.bits) {
                    32 => if (val.* == .num)
                        @floatCast(f32, val.num)
                    else if (val.* == .int)
                        @intToFloat(f32, val.int)
                    else
                        return vm.fatal("expected num"),
                    64 => if (val.* == .num)
                        val.num
                    else if (val.* == .int)
                        @intToFloat(f64, val.int)
                    else
                        return vm.fatal("expected num"),
                    else => @compileError("unsupported float"),
                },
                .Enum => {
                    if (val.* != .tagged)
                        return vm.fatal("expected tag");
                    const e = std.meta.stringToEnum(T, val.tagged.name) orelse
                        return vm.fatal("no value by such name");
                    if (val.tagged.value.* != .none)
                        return vm.fatal("expected no value");
                    return e;
                },
                else => @compileError("unsupported type: " ++ @typeName(T)),
            },
        };
    }

    pub fn jsonStringify(val: Value, options: std.json.StringifyOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (val) {
            .none => try writer.writeAll("null"),
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
                for (l.items) |e, i| {
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
            .range,
            .err,
            .tagged,
            => {
                try writer.writeByte('\"');
                try val.dump(writer, 0);
                try writer.writeByte('\"');
            },
            .iterator => unreachable,
        }
    }
};

fn wrapZigFunc(func: anytype) Value.Native {
    const Fn = @typeInfo(@TypeOf(func)).Fn;
    if (Fn.is_generic or Fn.is_var_args)
        @compileError("unsupported function");

    @setEvalBranchQuota(Fn.args.len * 1000);

    const S = struct {
        // cannot directly use `func` so declare a pointer to it
        var _func: @TypeOf(func) = undefined;

        fn native(vm: *Vm, bog_args: []*Value) Vm.Error!*Value {
            var args: std.meta.ArgsTuple(@TypeOf(_func)) = undefined;

            comptime var bog_arg_i: u8 = 0;
            inline for (Fn.args) |arg, i| {
                if (arg.arg_type.? == *Vm) {
                    args[i] = vm;
                } else {
                    args[i] = try bog_args[bog_arg_i].bogToZig(arg.arg_type.?, vm);
                    bog_arg_i += 1;
                }
            }
            return Value.zigToBog(vm, @call(.{}, _func, args));
        }
    };
    S._func = func;

    // TODO can't use bog_arg_i due to a stage1 bug.
    comptime var bog_arg_count = 0;
    comptime {
        for (Fn.args) |arg| {
            if (arg.arg_type != *Vm) bog_arg_count += 1;
        }
    }

    return .{
        .arg_count = bog_arg_count,
        .func = S.native,
    };
}

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
