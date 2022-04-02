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

    /// Result of ... operand, needs special handling in contexts where allowed.
    spread,
};

pub const Value = struct {
    v: union {
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
        tagged: struct {
            name: []const u8,
            value: *Value,
        },
        iterator: Iterator,
        spread: Spread,

        /// always memoized
        bool: bool,
        @"null": void,
    },
    ty: Type,
    state: enum {
        empty,
        white,
        gray,
        black,
    } = .white,

    pub const Iterator = struct {
        value: *Value,
        i: packed union {
            u: usize,
            i: i64,
        } = .{ .u = 0 },

        pub fn next(iter: *Iterator, ctx: Vm.Context, res: *?*Value) !bool {
            switch (iter.value.ty) {
                .tuple => {
                    const t = iter.value.v.tuple;
                    if (iter.i.u == t.len) {
                        return false;
                    }

                    res.* = t[iter.i.u];
                    iter.i.u += 1;
                },
                .list => {
                    const l = iter.value.v.list.inner.items;
                    if (iter.i.u == l.len) {
                        return false;
                    }

                    res.* = l[iter.i.u];
                    iter.i.u += 1;
                },
                .str => {
                    const str = iter.value.v.str.data;
                    if (iter.i.u == str.len) {
                        return false;
                    }
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc();
                    }

                    const cp_len = std.unicode.utf8ByteSequenceLength(str[iter.i.u]) catch
                        return ctx.throw("invalid utf-8 sequence");
                    iter.i.u += cp_len;

                    res.*.?.* = Value.string(str[iter.i.u - cp_len .. iter.i.u]);
                },
                .map => {
                    const m = iter.value.v.map;
                    if (iter.i.u == m.count()) {
                        return false;
                    }

                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc();
                    }
                    if (iter.i.u == 0) {
                        res.*.?.* = Value.tuple(try ctx.vm.gc.gpa.alloc(*Value, 2));
                    }
                    // const e = &map.entries.items[iter.i.u];
                    const e_key = &m.keys()[iter.i.u].*;
                    const e_value = &m.values()[iter.i.u].*;
                    const t = res.*.?.v.tuple;
                    // removing `const` on `Map` causes dependency loop??
                    t[0] = @intToPtr(*Value, @ptrToInt(e_key));
                    t[1] = e_value;
                    iter.i.u += 1;
                },
                .range => {
                    const r = iter.value.v.range;
                    if (iter.i.i >= r.end) {
                        return false;
                    }
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc();
                    }

                    res.*.?.* = Value.int(iter.i.i);
                    iter.i.i += r.step;
                },
                else => unreachable,
            }
            return true;
        }
    };

    pub const Spread = struct {
        iterable: *Value,

        pub fn len(s: Spread) usize {
            return switch (s.iterable.ty) {
                .range => unreachable, // Handled in Vm
                .str => unreachable, // Handled in Vm
                .tuple => s.iterable.v.tuple.len,
                .list => s.iterable.v.list.inner.items.len,
                else => unreachable,
            };
        }

        pub fn items(s: Spread) []*Value {
            return switch (s.iterable.ty) {
                .range => unreachable, // Handled in Vm
                .str => unreachable, // Handled in Vm
                .tuple => s.iterable.v.tuple,
                .list => s.iterable.v.list.inner.items,
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

    var null_base = Value{ .v = .{ .@"null" = {} }, .ty = .@"null" };
    var true_base = Value{ .v = .{ .bool = true }, .ty = .bool };
    var false_base = Value{ .v = .{ .bool = false }, .ty = .bool };

    pub const Null = &null_base;
    pub const True = &true_base;
    pub const False = &false_base;

    pub fn string(s: anytype) Value {
        return .{ .ty = .str, .v = .{ .str = .{
            .data = s,
            .capacity = if (std.meta.trait.isConstPtr(@TypeOf(s))) 0 else s.len,
        } } };
    }

    pub fn map() Value {
        return .{ .ty = .map, .v = .{ .map = .{} } };
    }

    pub fn list() Value {
        return .{ .ty = .list, .v = .{ .list = .{} } };
    }

    pub fn tuple(t: []*Value) Value {
        return .{ .ty = .tuple, .v = .{ .tuple = t } };
    }

    pub fn int(i: i64) Value {
        return .{ .ty = .int, .v = .{ .int = i } };
    }

    pub fn num(n: f64) Value {
        return .{ .ty = .num, .v = .{ .num = n } };
    }

    pub fn frame(f: *Vm.Frame) Value {
        return .{ .ty = .frame, .v = .{ .frame = f } };
    }

    pub fn err(e: *Value) Value {
        return .{ .ty = .err, .v = .{ .err = e } };
    }

    pub fn tagged(name: []const u8, v: *Value) Value {
        return .{
            .ty = .tagged,
            .v = .{ .tagged = .{ .name = name, .value = v } },
        };
    }

    pub inline fn typeName(val: Value) []const u8 {
        return switch (val.ty) {
            // zig fmt: off
            .@"null", .int, .num, .bool, .str, .tuple,
            .map, .list, .err, .range, .func, .tagged,
            // zig fmt: on
            => @tagName(val.ty),
            .frame => unreachable,
            .iterator => unreachable,
            .spread => unreachable,
            .native => "func",
        };
    }

    /// Frees any extra memory allocated by value.
    /// Does not free values recursively.
    pub fn deinit(val: *Value, allocator: Allocator) void {
        switch (val.ty) {
            .bool, .@"null" => return,
            .frame => {}, // frames are managed by the VM
            .int, .num, .native, .tagged, .range, .iterator, .err, .spread => {},
            .tuple => allocator.free(val.v.tuple),
            .map => val.v.map.deinit(allocator),
            .list => val.v.list.deinit(allocator),
            .str => val.v.str.deinit(allocator),
            .func => allocator.free(val.v.func.captures()),
        }
        val.* = undefined;
    }

    pub fn hash(key: *const Value) u32 {
        const autoHash = std.hash.autoHash;

        var hasher = std.hash.Wyhash.init(0);
        autoHash(&hasher, key.ty);
        switch (key.ty) {
            .iterator, .spread => unreachable,
            .frame => autoHash(&hasher, key.v.frame),
            .@"null" => {},
            .int => autoHash(&hasher, key.v.int),
            .num => {},
            .bool => autoHash(&hasher, key.v.bool),
            .str => hasher.update(key.v.str.data),
            .tuple => {
                autoHash(&hasher, key.v.tuple.len);
                // TODO more hashing?
            },
            .map => {
                autoHash(&hasher, key.v.map.count());
                // TODO more hashing?
            },
            .list => {
                autoHash(&hasher, key.v.list.inner.items.len);
                // TODO more hashing?
            },
            .err => autoHash(&hasher, key.v.err.ty),
            .range => {
                const range = key.v.range;
                autoHash(&hasher, range.start);
                autoHash(&hasher, range.end);
                autoHash(&hasher, range.step);
            },
            .func => {
                const f = key.v.func;
                autoHash(&hasher, f.extra_index);
                autoHash(&hasher, f.module);
            },
            .native => {
                const n = key.v.native;
                autoHash(&hasher, n.arg_count);
                autoHash(&hasher, n.func);
            },
            .tagged => {
                const t = key.v.tagged;
                hasher.update(t.name);
                autoHash(&hasher, t.value);
            },
        }
        return @truncate(u32, hasher.final());
    }

    pub fn eql(a: *const Value, b: *const Value) bool {
        switch (a.ty) {
            .int => return switch (b.ty) {
                .int => a.v.int == b.v.int,
                .num => @intToFloat(f64, a.v.int) == b.v.num,
                else => false,
            },
            .num => return switch (b.ty) {
                .int => a.v.num == @intToFloat(f64, b.v.int),
                .num => a.v.num == b.v.num,
                else => false,
            },
            else => if (a.ty != b.ty) return false,
        }
        return switch (a.ty) {
            .iterator, .spread, .int, .num => unreachable,
            .frame => a.v.frame == b.v.frame,
            .@"null" => true,
            .bool => a.v.bool == b.v.bool,
            .str => a.v.str.eql(b.v.str),
            .tuple => {
                const a_t = a.v.tuple;
                const b_t = b.v.tuple;
                if (a_t.len != b_t.len) return false;
                for (a_t) |v, i| {
                    if (!v.eql(b_t[i])) return false;
                }
                return true;
            },
            .map => @panic("TODO eql for maps"),
            .list => a.v.list.eql(b.v.list),
            .err => a.v.err.eql(b.v.err),
            .range => {
                const a_r = a.v.range;
                const b_r = a.v.range;
                return a_r.start == b_r.start and
                    a_r.end == b_r.end and
                    a_r.step == b_r.step;
            },
            .func => {
                const a_f = a.v.func;
                const b_f = b.v.func;
                return a_f.module == b_f.module and a_f.extra_index == b_f.extra_index;
            },
            .native => a.v.native.func == b.v.native.func,
            .tagged => {
                if (!mem.eql(u8, a.v.tagged.name, b.v.tagged.name)) return false;
                return a.v.tagged.value.eql(b.v.tagged.value);
            },
        };
    }

    /// Prints string representation of value to writer
    pub fn dump(val: *const Value, writer: anytype, level: u32) @TypeOf(writer).Error!void {
        switch (val.ty) {
            .iterator, .spread => unreachable,
            .int => try writer.print("{}", .{val.v.int}),
            .num => try writer.print("{d}", .{val.v.num}),
            .bool => try writer.writeAll(if (val.v.bool) "true" else "false"),
            .@"null" => try writer.writeAll("null"),
            .range => {
                const r = val.v.range;
                try writer.print("{}:{}:{}", .{ r.start, r.end, r.step });
            },
            .tuple => {
                if (level == 0) {
                    try writer.writeAll("(...)");
                } else {
                    try writer.writeByte('(');
                    for (val.v.tuple) |v, i| {
                        if (i != 0) try writer.writeAll(", ");
                        try v.dump(writer, level - 1);
                    }
                    try writer.writeByte(')');
                }
            },
            .map => {
                if (level == 0) {
                    try writer.writeAll("{...}");
                } else {
                    try writer.writeByte('{');
                    var i: usize = 0;
                    var iter = val.v.map.iterator();
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
            .list => {
                if (level == 0) {
                    try writer.writeAll("[...]");
                } else {
                    try writer.writeByte('[');
                    for (val.v.list.inner.items) |v, i| {
                        if (i != 0) try writer.writeAll(", ");
                        try v.dump(writer, level - 1);
                    }
                    try writer.writeByte(']');
                }
            },
            .err => {
                if (level == 0) {
                    try writer.writeAll("error(...)");
                } else {
                    try writer.writeAll("error(");
                    try val.v.err.dump(writer, level - 1);
                    try writer.writeByte(')');
                }
            },
            .str => try val.v.str.dump(writer),
            .func => {
                const f = val.v.func;
                try writer.print("fn({})@0x{X}[{}]", .{ f.args(), f.body()[0], f.captures().len });
            },
            .frame => {
                try writer.print("frame@x{X}", .{val.v.frame.body[0]});
            },
            .native => {
                const n = val.v.native;
                try writer.print("native({})@0x{}", .{ n.arg_count, @ptrToInt(n.func) });
            },
            .tagged => {
                try writer.print("@{s}", .{val.v.tagged.name});
                if (level == 0) {
                    try writer.writeAll("(...)");
                } else {
                    try val.v.tagged.value.dump(writer, level - 1);
                }
            },
        }
    }

    /// Returns value in `container` at `index`.
    pub fn get(container: *const Value, ctx: Vm.Context, index: *const Value, res: *?*Value) NativeError!void {
        switch (container.ty) {
            .tuple => switch (index.ty) {
                .int => {
                    var i = index.v.int;
                    if (i < 0)
                        i += @intCast(i64, container.v.tuple.len);
                    if (i < 0 or i >= container.v.tuple.len)
                        return ctx.throw("index out of bounds");

                    res.* = container.v.tuple[@intCast(u32, i)];
                },
                .range => {
                    const r = container.v.range;
                    if (r.start < 0 or r.end > container.v.tuple.len)
                        return ctx.throw("index out of bounds");

                    res.* = try ctx.vm.gc.alloc();
                    res.*.?.* = Value.list();
                    const res_list = &res.*.?.*.v.list;
                    try res_list.inner.ensureUnusedCapacity(ctx.vm.gc.gpa, r.count());

                    var it = r.iterator();
                    while (it.next()) |some| {
                        res_list.inner.appendAssumeCapacity(container.v.tuple[@intCast(u32, some)]);
                    }
                },
                .str => {
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc();
                    }

                    if (mem.eql(u8, index.v.str.data, "len")) {
                        res.*.?.* = Value.int(@intCast(i64, container.v.tuple.len));
                    } else {
                        return ctx.throw("no such property");
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            .list => return container.v.list.get(ctx, index, res),
            .map => {
                res.* = container.v.map.get(index) orelse
                    return ctx.throwFmt("no value associated with key: {}", .{index.*});
            },
            .str => return container.v.str.get(ctx, index, res),
            .iterator => unreachable,
            .range => switch (index.ty) {
                .str => {
                    const r = container.v.range;
                    const s = index.v.str.data;
                    if (res.* == null) {
                        res.* = try ctx.vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "start")) {
                        res.*.?.* = Value.int(r.start);
                    } else if (mem.eql(u8, s, "end")) {
                        res.*.?.* = Value.int(r.end);
                    } else if (mem.eql(u8, s, "step")) {
                        res.*.?.* = Value.int(r.step);
                    } else {
                        return ctx.throw("no such property");
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            else => return ctx.throw("invalid subscript type"),
        }
    }

    /// Sets index of container to value. Does a shallow copy if value stored.
    pub fn set(container: *Value, ctx: Vm.Context, index: *const Value, new_val: *Value) NativeError!void {
        switch (container.ty) {
            .tuple => switch (index.ty) {
                .int => {
                    var i = index.v.int;
                    if (i < 0)
                        i += @intCast(i64, container.v.tuple.len);
                    if (i < 0 or i >= container.v.tuple.len)
                        return ctx.throw("index out of bounds");

                    container.v.tuple[@intCast(u32, i)] = new_val;
                },
                .range => {
                    const r = index.v.range;
                    if (r.start < 0 or r.end > container.v.tuple.len)
                        return ctx.throw("index out of bounds");

                    var it = r.iterator();
                    while (it.next()) |some| {
                        container.v.tuple[@intCast(u32, some)] = new_val;
                    }
                },
                else => return ctx.throw("invalid index type"),
            },
            .map => {
                _ = try container.v.map.put(ctx.vm.gc.gpa, try ctx.vm.gc.dupe(index), new_val);
            },
            .list => try container.v.list.set(ctx, index, new_val),
            .str => try container.v.str.set(ctx, index, new_val),
            .iterator => unreachable,
            else => return ctx.throw("invalid subscript type"),
        }
    }

    /// `type_id` must be valid and cannot be .err, .range, .func or .native
    pub fn as(val: *Value, ctx: Vm.Context, type_id: Type) NativeError!*Value {
        if (type_id == .@"null") {
            return Value.Null;
        }
        if (val.ty == type_id) {
            return val;
        }

        if (val.ty == .str) {
            return val.v.str.as(ctx, type_id);
        } else if (type_id == .str) {
            return String.from(val, ctx.vm);
        }

        if (val.ty == .list) {
            return val.v.list.as(ctx, type_id);
        } else if (type_id == .list) {
            return List.from(val, ctx);
        }

        if (type_id == .bool) {
            const bool_res = switch (val.ty) {
                .int => val.v.int != 0,
                .num => val.v.num != 0,
                .bool => unreachable,
                .str => unreachable,
                else => return ctx.throwFmt("cannot cast {s} to bool", .{val.typeName()}),
            };

            return if (bool_res) Value.True else Value.False;
        }

        const new_val = try ctx.vm.gc.alloc();
        new_val.* = switch (type_id) {
            .int => Value.int(switch (val.ty) {
                .int => unreachable,
                .num => std.math.lossyCast(i64, val.v.num),
                .bool => @boolToInt(val.v.bool),
                .str => unreachable,
                else => return ctx.throwFmt("cannot cast {s} to int", .{val.typeName()}),
            }),
            .num => Value.num(switch (val.ty) {
                .num => unreachable,
                .int => @intToFloat(f64, val.v.int),
                .bool => @intToFloat(f64, @boolToInt(val.v.bool)),
                .str => unreachable,
                else => return ctx.throwFmt("cannot cast {s} to num", .{val.typeName()}),
            }),
            .str, .list, .bool, .@"null" => unreachable,
            .tuple, .map => return ctx.frame.fatal(ctx.vm, "TODO cast to tuple/map"),
            else => unreachable,
        };
        return new_val;
    }

    pub fn is(val: *const Value, type_id: Type) bool {
        if (val.ty == type_id) return true;
        if (type_id == .func and val.ty == .native) return true;
        return false;
    }

    /// Returns whether `container` has `val` in it.
    pub fn in(val: *const Value, container: *const Value) bool {
        switch (container.ty) {
            .str => return val.v.str.in(val),
            .tuple => {
                for (val.v.tuple) |v| {
                    if (v.eql(val)) return true;
                }
                return false;
            },
            .list => return val.v.list.in(val),
            .map => return val.v.map.contains(val),
            .range => {
                const r = val.v.range;
                if (val.ty != .int) return false;
                const i = val.v.int;
                if (i < r.start or i > r.end) return false;
                if (@rem(i - r.start, r.step) != 0) return false;
                return true;
            },
            .iterator => unreachable,
            else => unreachable,
        }
    }

    pub fn iterator(val: *const Value, ctx: Vm.Context) NativeError!*Value {
        var start: ?i64 = null;
        switch (val.ty) {
            .range => start = val.v.range.start,
            .str, .tuple, .list, .map => {},
            .iterator => unreachable,
            else => return ctx.throwFmt("cannot iterate {s}", .{val.typeName()}),
        }
        const iter = try ctx.vm.gc.alloc();
        iter.* = .{ .ty = .iterator, .v = .{
            .iterator = .{
                .value = try ctx.vm.gc.dupe(val),
            },
        } };
        if (start) |some| iter.v.iterator.i.i = some;
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
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    comptime var pub_decls = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) pub_decls += 1;
                    }

                    const res = try vm.gc.alloc();
                    res.* = Value.map();
                    try res.v.map.ensureTotalCapacity(vm.gc.gpa, pub_decls);

                    inline for (info.decls) |decl| {
                        if (!decl.is_pub) continue;
                        // skip common interfaces
                        if (comptime std.mem.eql(u8, decl.name, "intoBog")) continue;
                        if (comptime std.mem.eql(u8, decl.name, "fromBog")) continue;
                        if (comptime std.mem.eql(u8, decl.name, "format")) continue;

                        const key = try vm.gc.alloc();
                        key.* = Value.string(decl.name);

                        const value = if (@typeInfo(@TypeOf(@field(val, decl.name))) == .Fn)
                            try zigFnToBog(vm, @field(val, decl.name))
                        else
                            try zigToBog(vm, @field(val, decl.name));
                        res.v.map.putAssumeCapacityNoClobber(key, value);
                    }

                    return res;
                },
                else => @compileError("unsupported type: " ++ @typeName(val)),
            },
            else => switch (@typeInfo(@TypeOf(val))) {
                .Fn => @compileError("use zigFnToBog"),
                .ComptimeInt, .Int => {
                    const res = try vm.gc.alloc();
                    // try to implicit cast the value
                    res.* = Value.int(val);
                    return res;
                },
                .ComptimeFloat, .Float => {
                    const res = try vm.gc.alloc();
                    // try to implicit cast the value
                    res.* = Value.num(val);
                    return res;
                },
                .ErrorUnion => if (val) |some| {
                    return zigToBog(vm, some);
                } else |e| {
                    // wrap error string
                    const str = try vm.gc.alloc();
                    str.* = Value.string(@errorName(e));
                    const res = try vm.gc.alloc();
                    res.* = Value.err(str);
                    return res;
                },
                .Enum => {
                    const tag = try vm.gc.alloc();
                    tag.* = Value.tagged(@tagName(val), Value.Null);
                    return tag;
                },
                .Optional => if (val) |some| {
                    return zigToBog(vm, some);
                } else {
                    return Value.Null;
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
                        else => return Value.zigToBog(ctx.vm, res),
                    },
                    .ErrorUnion => if (res) |val| {
                        return Value.zigToBog(ctx.vm, val);
                    } else |er| switch (@as(anyerror, er)) {
                        error.FatalError, error.Throw => |e| return e,
                        else => return Value.zigToBog(ctx.vm, res),
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

        const native = try vm.gc.alloc();
        native.* = .{ .ty = .native, .v = .{
            .native = .{
                .arg_count = bog_arg_count,
                .variadic = variadic,
                .func = S.native,
            },
        } };
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
                if (val.ty != .@"null")
                    return ctx.throw("expected a null");
            },
            bool => {
                if (val.ty != .bool)
                    return ctx.throw("expected a bool");
                return val.v.bool;
            },
            []const u8 => {
                if (val.ty != .str)
                    return ctx.throw("expected a string");
                return val.v.str.data;
            },
            *Map, *const Map => {
                if (val.ty != .map)
                    return ctx.throw("expected a map");
                return &val.v.map;
            },
            *List, *const List => {
                if (val.ty != .list)
                    return ctx.throw("expected a list");
                return &val.v.list;
            },
            *Value, *const Value => val,
            *String, *const String => {
                if (val.ty != .str)
                    return ctx.throw("expected a string");
                return &val.v.str;
            },
            []*Value, []const *Value, []*const Value, []const *const Value => {
                switch (val.ty) {
                    .tuple => return val.v.tuple,
                    .list => return val.v.list.inner.items,
                    else => return ctx.throw("expected a list or a tuple"),
                }
            },
            else => switch (@typeInfo(T)) {
                .Int => switch (val.ty) {
                    .int => {
                        const i = val.v.int;
                        if (i < std.math.minInt(T) or i > std.math.maxInt(T))
                            return ctx.throw("cannot fit int in desired type");
                        return @intCast(T, i);
                    },
                    .num => std.math.lossyCast(T, val.v.num),
                    else => return ctx.throw("expected int"),
                },
                .Float => |info| switch (info.bits) {
                    32 => switch (val.ty) {
                        .num => @floatCast(f32, val.v.num),
                        .int => @intToFloat(f32, val.v.int),
                        else => return ctx.throw("expected num"),
                    },
                    64 => switch (val.ty) {
                        .num => val.v.num,
                        .int => @intToFloat(f64, val.v.int),
                        else => return ctx.throw("expected num"),
                    },
                    else => @compileError("unsupported float"),
                },
                .Enum => {
                    if (val.ty != .tagged)
                        return ctx.throw("expected tag");
                    const e = std.meta.stringToEnum(T, val.v.tagged.name) orelse
                        return ctx.throw("no value by such name");
                    if (val.v.tagged.value.ty != .@"null")
                        return ctx.throw("expected no value");
                    return e;
                },
                else => @compileError("unsupported type: " ++ @typeName(T)),
            },
        };
    }

    pub fn jsonStringify(val: *const Value, options: std.json.StringifyOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (val.ty) {
            .@"null" => try writer.writeAll("null"),
            .tuple => {
                try writer.writeByte('[');
                for (val.v.tuple) |e, i| {
                    if (i != 0) try writer.writeByte(',');
                    try e.jsonStringify(options, writer);
                }
                try writer.writeByte(']');
            },
            .list => {
                try writer.writeByte('[');
                for (val.v.list.inner.items) |e, i| {
                    if (i != 0) try writer.writeByte(',');
                    try e.jsonStringify(options, writer);
                }
                try writer.writeByte(']');
            },
            .map => {
                try writer.writeByte('{');
                var i: usize = 0;
                var iter = val.v.map.iterator();
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
            .str => {
                try writer.print("\"{}\"", .{std.zig.fmtEscapes(val.v.str.data)});
            },
            .native,
            .func,
            .frame,
            .range,
            .err,
            .tagged,
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
