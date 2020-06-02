const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Module = bog.Module;
const NativeFn = bog.NativeFn;
const util = @import("util.zig");

pub const Type = enum(u8) {
    none,
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

    /// pseudo type user should not have access to via valid bytecode
    iterator,

    /// native being separate from .func is an implementation detail
    native,
    _,
};

pub const Value = union(Type) {
    tuple: struct {
        values: []*Value,
        allocator: *Allocator,
    },
    map: Map,
    list: List,
    err: *Value,
    int: i64,
    num: f64,
    range: struct {
        begin: *Value,
        end: *Value,
    },
    str: []const u8,
    func: struct {
        /// offset to the functions first instruction
        offset: u32,
        arg_count: u8,

        /// module in which this function exists
        module: *Module,

        captures: []*Value,
        allocator: *Allocator,
    },
    native: bog.native.Native,
    iterator: struct {
        value: *Value,
        index: usize,

        // TODO protect against concurrent modification
        pub fn next(iter: *@This(), vm: *Vm, res: *?*Value) !void {
            switch (iter.value.*) {
                .tuple => |tuple| {
                    if (iter.index == tuple.values.len) {
                        res.* = &Value.None;
                        return;
                    }

                    res.* = tuple.values[iter.index];
                    iter.index += 1;
                },
                .list => |list| {
                    if (iter.index == list.items.len) {
                        res.* = &Value.None;
                        return;
                    }

                    res.* = list.items[iter.index];
                    iter.index += 1;
                },
                .str => |str| {
                    if (iter.index == str.len) {
                        res.* = &Value.None;
                        return;
                    }
                    if (res.* == null)
                        res.* = try vm.gc.alloc();

                    const cp_len = std.unicode.utf8ByteSequenceLength(str[iter.index]) catch
                        return vm.reportErr("invalid utf-8 sequence");
                    iter.index += cp_len;

                    res.*.?.* = .{
                        .str = str[iter.index - cp_len .. iter.index],
                    };
                },
                .map => @panic("TODO: map iterator"),
                .range => @panic("TODO: range iterator"),
                else => unreachable,
            }
        }
    },

    /// always memoized
    bool: bool,
    none,

    pub const Map = std.HashMap(*Value, *Value, hash, eql);
    pub const List = std.ArrayList(*Value);

    pub var None = Value{ .none = {} };
    pub var True = Value{ .bool = true };
    pub var False = Value{ .bool = false };

    /// Frees any extra memory allocated by value.
    /// Does not free values recursively.
    pub fn deinit(value: *Value) void {
        switch (value.*) {
            .int, .num, .none, .bool, .native => {},
            .tuple => |*t| t.allocator.free(t.values),
            .map => |*m| m.deinit(),
            .list => |*l| l.deinit(),
            .err => |e| e.deinit(),
            .range => |*r| {
                r.begin.deinit();
                r.end.deinit();
            },
            .str => {
                // TODO string memory management
            },
            .func => |*f| f.allocator.free(f.captures),
            .iterator => |*i| i.value.deinit(),
            _ => unreachable,
        }
    }

    pub fn hash(key: *Value) u32 {
        const autoHash = std.hash.autoHash;

        var hasher = std.hash.Wyhash.init(0);
        autoHash(&hasher, @as(Type, key.*));
        switch (key.*) {
            .iterator => unreachable,
            .none => {},
            .int => |int| autoHash(&hasher, int),
            .num => |num| autoHash(&hasher, num),
            .bool => |b| autoHash(&hasher, b),
            .str => |str| hasher.update(str),
            .tuple => |*tuple| {
                autoHash(&hasher, tuple.values.len);
                autoHash(&hasher, tuple.values.ptr);
            },
            .map => |*map| {
                autoHash(&hasher, map.size);
                autoHash(&hasher, map.entries.len);
                autoHash(&hasher, map.entries.ptr);
                autoHash(&hasher, map.max_distance_from_start_index);
            },
            .list => |*list| {
                autoHash(&hasher, list.items.len);
                autoHash(&hasher, list.items.ptr);
            },
            .err => |err| autoHash(&hasher, @as(Type, err.*)),
            .range => |*range| {
                autoHash(&hasher, @as(Type, range.begin.*));
                autoHash(&hasher, @as(Type, range.end.*));
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
            _ => unreachable,
        }
        return @truncate(u32, hasher.final());
    }

    pub fn eql(a: *Value, b: *Value) bool {
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
            else => if (a.* != @as(@TagType(@TypeOf(b.*)), b.*)) return false,
        }
        return switch (a.*) {
            .iterator, .int, .num => unreachable,
            .none => true,
            .bool => |bool_val| bool_val == b.bool,
            .str => |s| {
                return std.mem.eql(u8, s, b.str);
            },
            .tuple => |t| {
                const b_val = b.tuple.values;
                if (t.values.len != b_val.len) return false;
                for (t.values) |v, i| {
                    if (!v.eql(b_val[i])) return false;
                }
                return true;
            },
            .map => |m| @panic("TODO eql for maps"),
            .list => |l| {
                if (l.items.len != b.list.items.len) return false;
                for (l.items) |v, i| {
                    if (!v.eql(b.list.items[i])) return false;
                }
                return true;
            },
            .err => |e| e.eql(b.err),
            .range => |r| @panic("TODO eql for ranges"),
            .func => |f| {
                return f.offset == b.func.offset and
                    f.arg_count == b.func.arg_count and
                    f.module == b.func.module;
            },
            .native => |n| n.func == b.native.func,
            _ => unreachable,
        };
    }

    pub fn dump(value: Value, stream: var, level: u32) @TypeOf(stream).Error!void {
        switch (value) {
            .iterator => unreachable,
            .int => |i| try stream.print("{}", .{i}),
            .num => |n| try stream.print("{d}", .{n}),
            .bool => |b| try stream.writeAll(if (b) "true" else "false"),
            .none => try stream.writeAll("()"),
            .range => |r| {
                if (level == 0) {
                    try stream.writeAll("(range)");
                } else {
                    try r.begin.dump(stream, level - 1);
                    try stream.writeAll("...");
                    try r.end.dump(stream, level - 1);
                }
            },
            .tuple => |t| {
                if (level == 0) {
                    try stream.writeAll("(...)");
                } else {
                    try stream.writeByte('(');
                    for (t.values) |v, i| {
                        if (i != 0) try stream.writeAll(", ");
                        try v.dump(stream, level - 1);
                    }
                    try stream.writeByte(')');
                }
            },
            .map => |m| {
                if (level == 0) {
                    try stream.writeAll("{...}");
                } else {
                    try stream.writeByte('{');
                    var it = m.iterator();
                    while (it.next()) |kv| {
                        if (it.count != 1)
                            try stream.writeAll(", ");
                        try kv.key.dump(stream, level - 1);
                        try stream.writeAll(": ");
                        try kv.value.dump(stream, level - 1);
                    }
                    try stream.writeByte('}');
                }
            },
            .list => |l| {
                if (level == 0) {
                    try stream.writeAll("[...]");
                } else {
                    try stream.writeByte('[');
                    for (l.items) |v, i| {
                        if (i != 0) try stream.writeAll(", ");
                        try v.dump(stream, level - 1);
                    }
                    try stream.writeByte(']');
                }
            },
            .err => |e| {
                if (level == 0) {
                    try stream.writeAll("error(...)");
                } else {
                    try stream.writeAll("error(");
                    try e.dump(stream, level - 1);
                    try stream.writeByte(')');
                }
            },
            .str => |s| {
                try stream.writeByte('"');
                for (s) |c| {
                    switch (c) {
                        '\n' => try stream.writeAll("\\n"),
                        '\t' => try stream.writeAll("\\t"),
                        '\r' => try stream.writeAll("\\r"),
                        '\'' => try stream.writeAll("\\'"),
                        '"' => try stream.writeAll("\\\""),
                        else => if (std.ascii.isCntrl(c))
                            try stream.print("\\x{x:0<2}", .{c})
                        else
                            try stream.print("{c}", .{c}),
                    }
                }
                try stream.writeByte('"');
            },
            .func => |f| {
                try stream.print("fn({})@0x{X}[{}]", .{ f.arg_count, f.offset, f.captures.len });
            },
            .native => |n| {
                try stream.print("native({})@0x{}", .{ n.arg_count, @ptrToInt(n.func) });
            },
            _ => unreachable,
        }
    }

    pub fn get(val: *Value, vm: *Vm, index: *Value, res: *?*Value) !void {
        switch (val.*) {
            .tuple => |tuple| switch (index.*) {
                .int => {
                    var i = index.int;
                    if (i < 0)
                        i += @intCast(i64, tuple.values.len);
                    if (i < 0 or i >= tuple.values.len)
                        return vm.reportErr("index out of bounds");

                    res.* = tuple.values[@intCast(u32, i)];
                },
                .range => return vm.reportErr("TODO get with ranges"),
                .str => |s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "len")) {
                        res.*.?.* = .{ .int = @intCast(i64, tuple.values.len) };
                    } else {
                        return vm.reportErr("no such property");
                    }
                },
                else => return vm.reportErr("invalid index type"),
            },
            .list => |list| switch (index.*) {
                .int => {
                    var i = index.int;
                    if (i < 0)
                        i += @intCast(i64, list.items.len);
                    if (i < 0 or i >= list.items.len)
                        return vm.reportErr("index out of bounds");

                    res.* = list.items[@intCast(u32, i)];
                },
                .range => return vm.reportErr("TODO get with ranges"),
                .str => |s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "len")) {
                        res.*.?.* = .{ .int = @intCast(i64, list.items.len) };
                    } else {
                        return vm.reportErr("no such property");
                    }
                },
                else => return vm.reportErr("invalid index type"),
            },
            .map => |map| {
                res.* = map.getValue(index) orelse
                    return vm.reportErr("TODO better handling undefined key");
            },
            .str => |str| switch (index.*) {
                .int => return vm.reportErr("TODO get str"),
                .range => return vm.reportErr("TODO get with ranges"),
                .str => |s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "len")) {
                        res.*.?.* = .{ .int = @intCast(i64, str.len) };
                    } else {
                        return vm.reportErr("no such property");
                    }
                },
                else => return vm.reportErr("invalid index type"),
            },
            .iterator => unreachable,
            else => return vm.reportErr("invalid subscript type"),
        }
    }

    pub fn set(val: *Value, vm: *Vm, index: *Value, new_val: *Value) !void {
        switch (val.*) {
            .tuple => |tuple| if (index.* == .int) {
                var i = index.int;
                if (i < 0)
                    i += @intCast(i64, tuple.values.len);
                if (i < 0 or i >= tuple.values.len)
                    return vm.reportErr("index out of bounds");

                tuple.values[@intCast(u32, i)].* = new_val.*;
            } else {
                return vm.reportErr("TODO set with ranges");
            },
            .map => |*map| {
                _ = try map.put(index, new_val);
            },
            .list => |list| if (index.* == .int) {
                var i = index.int;
                if (i < 0)
                    i += @intCast(i64, list.items.len);
                if (i < 0 or i >= list.items.len)
                    return vm.reportErr("index out of bounds");

                list.items[@intCast(u32, i)].* = new_val.*;
            } else {
                return vm.reportErr("TODO set with ranges");
            },
            .str => |str| {
                return vm.reportErr("TODO set string");
            },
            .iterator => unreachable,
            else => return vm.reportErr("invalid subscript type"),
        }
    }

    /// `type_id` must be valid and cannot be .err, .range, .func or .native
    pub fn as(val: *Value, vm: *Vm, type_id: Type) !*Value {
        if (type_id == .none) {
            return &Value.None;
        }
        if (val.* == type_id) {
            return val;
        }

        if (type_id == .bool) {
            const bool_res = switch (val.*) {
                .int => |int| int != 0,
                .num => |num| num != 0,
                .bool => unreachable,
                .str => |str| if (mem.eql(u8, str, "true"))
                    true
                else if (mem.eql(u8, str, "false"))
                    false
                else
                    return vm.reportErr("cannot cast string to bool"),
                else => return vm.reportErr("invalid cast to bool"),
            };

            return if (bool_res) &Value.True else &Value.False;
        }

        const new_val = try vm.gc.alloc();
        new_val.* = switch (type_id) {
            .bool, .none, .err, .range, .func, .native, .iterator => unreachable,
            .int => .{
                .int = switch (val.*) {
                    .int => unreachable,
                    .num => |num| @floatToInt(i64, num),
                    .bool => |b| @boolToInt(b),
                    .str => |str| util.parseInt(str) catch
                        return vm.reportErr("invalid cast to int"),
                    else => return vm.reportErr("invalid cast to int"),
                },
            },
            .num => .{
                .num = switch (val.*) {
                    .num => unreachable,
                    .int => |int| @intToFloat(f64, int),
                    .bool => |b| @intToFloat(f64, @boolToInt(b)),
                    .str => |str| util.parseNum(str) catch
                        return vm.reportErr("invalid cast to num"),
                    else => return vm.reportErr("invalid cast to num"),
                },
            },
            .str,
            .tuple,
            .map,
            .list,
            => return vm.reportErr("TODO more casts"),
            _ => unreachable,
        };
        return new_val;
    }

    pub fn is(val: *Value, type_id: Type) bool {
        if (val.* == type_id) return true;
        if (type_id == .func and val.* == .native) return true;
        return false;
    }

    pub fn in(val: *Value, container: *Value) bool {
        switch (container.*) {
            .str => |str| {
                if (val.* != .str) return false;
                return mem.indexOf(u8, str, val.str) != null;
            },
            .tuple => |*tuple| {
                for (tuple.values) |v| {
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
            .map => @panic("TODO in map"),
            .range => @panic("TODO in range"),
            .iterator => unreachable,
            else => unreachable,
        }
    }

    pub fn iterator(val: *Value, vm: *Vm) !*Value {
        switch (val.*) {
            .map => return vm.reportErr("TODO: map iterator"),
            .range => return vm.reportErr("TODO: range iterator"),
            .str, .tuple, .list => {},
            .iterator => unreachable,
            else => return vm.reportErr("invalid type for iteration"),
        }
        const iter = try vm.gc.alloc();
        iter.* = .{
            .iterator = .{
                .value = val,
                .index = 0,
            },
        };
        return iter;
    }
};

var buffer: [1024]u8 = undefined;

fn testDump(val: Value, expected: []const u8) !void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var out_buf = try std.Buffer.initSize(alloc, 0);
    var out_stream = std.io.BufferOutStream.init(&out_buf);

    try val.dump(&out_stream.stream, 4);
    const result = out_buf.items;

    if (!std.mem.eql(u8, result, expected)) {
        std.debug.warn("\n---expected----\n{}\n-----found-----\n{}\n---------------\n", .{ expected, result });
        return error.TestFailed;
    }
}

// TODO these cause a false dependency loop
// https://github.com/ziglang/zig/issues/4562
// test "dump int/num" {
//     var int = Value{
//         .int = 2,
//     };
//     try testDump(int, "2");
//     var num = Value{
//         .num = 2.5,
//     };
//     try testDump(num, "2.5");
// }

// test "dump error" {
//     var int = Value{
//         .int = 2,
//     };
//     var err = Value{
//         .err = &int,
//     };
//     try testDump(err, "error(2)");
// }
