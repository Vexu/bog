const std = @import("std");
const mem = std.mem;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Module = bog.Module;
const NativeFn = bog.NativeFn;
const util = @import("util.zig");

pub const Value = struct {
    pub const TypeId = enum(u8) {
        None,
        Int,
        Num,
        Bool,
        Str,
        Tuple,
        Map,
        List,
        Error,
        Range,
        Fn,

        /// pseudo type user should not have access to via valid bytecode
        Iterator,

        /// Native being separate from .Fn is an implementation detail
        Native,
        _,
    };

    pub const Map = std.HashMap(*Value, *Value, hash, eql);
    pub const List = std.ArrayList(*Value);

    marked: bool = false,

    /// TODO https://github.com/ziglang/zig/issues/4295
    __padding: u32 = 0,

    kind: union(TypeId) {
        Tuple: []*Value,
        Map: Map,
        List: List,
        Error: *Value,
        Int: i64,
        Num: f64,
        Range: struct {
            begin: *Value,
            end: *Value,
        },
        Str: []const u8,
        Fn: struct {
            /// offset to the functions first instruction
            offset: u32,
            arg_count: u8,

            /// module in which this function exists
            module: *Module,
        },
        Native: bog.native.Native,
        Iterator: struct {
            value: *Value,
            index: usize,

            // TODO protect against concurrent modification
            pub fn next(iter: *@This(), vm: *Vm, res: *?*Value) !void {
                switch (iter.value.kind) {
                    .Tuple => |tuple| {
                        if (iter.index == tuple.len) {
                            res.* = &Value.None;
                            return;
                        }

                        res.* = tuple[iter.index];
                        iter.index += 1;
                    },
                    .List => |list| {
                        if (iter.index == list.len) {
                            res.* = &Value.None;
                            return;
                        }

                        res.* = list.at(iter.index);
                        iter.index += 1;
                    },
                    .Str => |str| {
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
                            .kind = .{
                                .Str = str[iter.index - cp_len .. iter.index],
                            },
                        };
                    },
                    .Map => @panic("TODO: map iterator"),
                    .Range => @panic("TODO: range iterator"),
                    else => unreachable,
                }
            }
        },

        /// always memoized
        Bool: bool,
        None,
    },

    pub var None = Value{
        .kind = .None,
    };
    pub var True = Value{
        .kind = .{ .Bool = true },
    };
    pub var False = Value{
        .kind = .{ .Bool = false },
    };

    pub fn hash(key: *Value) u32 {
        const autoHash = std.hash.autoHash;

        var hasher = std.hash.Wyhash.init(0);
        autoHash(&hasher, @as(TypeId, key.kind));
        switch (key.kind) {
            .Iterator => unreachable,
            .None => {},
            .Int => |int| autoHash(&hasher, int),
            .Num => |num| autoHash(&hasher, num),
            .Bool => |b| autoHash(&hasher, b),
            .Str => |str| hasher.update(str),
            .Tuple => |tuple| {
                autoHash(&hasher, tuple.len);
                autoHash(&hasher, tuple.ptr);
            },
            .Map => |map| {
                autoHash(&hasher, map.size);
                autoHash(&hasher, map.entries.len);
                autoHash(&hasher, map.entries.ptr);
                autoHash(&hasher, map.max_distance_from_start_index);
            },
            .List => |list| {
                autoHash(&hasher, list.len);
                autoHash(&hasher, list.items.len);
                autoHash(&hasher, list.items.ptr);
            },
            .Error => |err| autoHash(&hasher, @as(TypeId, err.kind)),
            .Range => |range| {
                autoHash(&hasher, @as(TypeId, range.begin.kind));
                autoHash(&hasher, @as(TypeId, range.end.kind));
            },
            .Fn => |func| {
                autoHash(&hasher, func.offset);
                autoHash(&hasher, func.arg_count);
                autoHash(&hasher, func.module);
            },
            .Native => |func| {
                autoHash(&hasher, func.arg_count);
                autoHash(&hasher, func.func);
            },
            _ => unreachable,
        }
        return @truncate(u32, hasher.final());
    }

    pub fn eql(a: *Value, b: *Value) bool {
        switch (a.kind) {
            .Int => |val| return switch (b.kind) {
                .Int => |b_val| val == b_val,
                .Num => |b_val| @intToFloat(f64, val) == b_val,
                else => false,
            },
            .Num => |val| return switch (b.kind) {
                .Int => |b_val| val == @intToFloat(f64, b_val),
                .Num => |b_val| val == b_val,
                else => false,
            },
            else => if (a.kind != @as(@TagType(@TypeOf(b.kind)), b.kind)) return false,
        }
        return switch (a.kind) {
            .Iterator, .Int, .Num => unreachable,
            .None => true,
            .Bool => |val| val == b.kind.Bool,
            .Str => |val| {
                const b_val = b.kind.Str;
                return std.mem.eql(u8, val, b_val);
            },
            .Tuple => |val| {
                const b_val = b.kind.Tuple;
                if (val.len != b_val.len) return false;
                for (val) |v, i| {
                    if (!v.eql(b_val[i])) return false;
                }
                return true;
            },
            .Map => |val| @panic("TODO eql for maps"),
            .List => |val| {
                const b_val = b.kind.List;
                if (val.len != b_val.len) return false;
                for (val.toSliceConst()) |v, i| {
                    if (!v.eql(b_val.toSliceConst()[i])) return false;
                }
                return true;
            },
            .Error => |val| val.eql(b.kind.Error),
            .Range => |val| @panic("TODO eql for ranges"),
            .Fn => |val| {
                const b_val = b.kind.Fn;
                return val.offset == b_val.offset and
                    val.arg_count == b_val.arg_count and
                    val.module == b_val.module;
            },
            .Native => |val| val.func == b.kind.Native.func,
            _ => unreachable,
        };
    }

    pub fn dump(value: Value, stream: var, level: u32) (@TypeOf(stream).Error || error{Unimplemented})!void {
        switch (value.kind) {
            .Iterator => unreachable,
            .Int => |val| try stream.print("{}", .{val}),
            .Num => |val| try stream.print("{d}", .{val}),
            .Bool => |val| try stream.writeAll(if (val) "true" else "false"),
            .None => try stream.writeAll("()"),
            .Range => |val| {
                if (level == 0) {
                    try stream.writeAll("(range)");
                } else {
                    try val.begin.dump(stream, level - 1);
                    try stream.writeAll("...");
                    try val.end.dump(stream, level - 1);
                }
            },
            .Tuple => |val| {
                if (level == 0) {
                    try stream.writeAll("(...)");
                } else {
                    try stream.writeByte('(');
                    for (val) |v, i| {
                        if (i != 0) try stream.writeAll(", ");
                        try v.dump(stream, level - 1);
                    }
                    try stream.writeByte(')');
                }
            },
            .Map => {
                if (level == 0) {
                    try stream.writeAll("{...}");
                } else {
                    return error.Unimplemented;
                }
            },
            .List => |val| {
                if (level == 0) {
                    try stream.writeAll("[...]");
                } else {
                    try stream.writeByte('[');
                    for (val.toSliceConst()) |v, i| {
                        if (i != 0) try stream.writeAll(", ");
                        try v.dump(stream, level - 1);
                    }
                    try stream.writeByte(']');
                }
            },
            .Error => |val| {
                if (level == 0) {
                    try stream.writeAll("error(...)");
                } else {
                    try stream.writeAll("error(");
                    try val.dump(stream, level - 1);
                    try stream.writeByte(')');
                }
            },
            .Str => |val| {
                try stream.writeByte('"');
                for (val) |c| {
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
            .Fn => |val| {
                try stream.print("fn({})@0x{X}", .{ val.arg_count, val.offset });
            },
            .Native => |val| {
                try stream.print("native({})@0x{}", .{ val.arg_count, @ptrToInt(val.func) });
            },
            _ => unreachable,
        }
    }

    pub fn mark(value: *Value) void {
        if (value.marked) return;
        value.marked = true;
        switch (value.kind) {
            .None, .Int, .Num, .Fn, .Bool, .Str, .Native => {},

            .Iterator => |val| val.value.mark(),
            .Tuple => |val| for (val) |v| v.mark(),
            .Map => @panic("TODO mark for maps"),
            .List => |val| for (val.toSliceConst()) |v| v.mark(),
            .Error => |val| val.mark(),
            .Range => |val| {
                val.begin.mark();
                val.end.mark();
            },
            _ => unreachable,
        }
    }

    pub fn get(val: *Value, vm: *Vm, index: *Value, res: *?*Value) !void {
        switch (val.kind) {
            .Tuple => |tuple| switch (index.kind) {
                .Int => {
                    var i = index.kind.Int;
                    if (i < 0)
                        i += @intCast(i64, tuple.len);
                    if (i < 0 or i >= tuple.len)
                        return vm.reportErr("index out of bounds");

                    res.* = tuple[@intCast(u32, i)];
                },
                .Range => return vm.reportErr("TODO get with ranges"),
                .Str => |s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "len")) {
                        res.*.?.* = .{
                            .kind = .{ .Int = @intCast(i64, tuple.len) },
                        };
                    } else {
                        return vm.reportErr("no such property");
                    }
                },
                else => return vm.reportErr("invalid index type"),
            },
            .List => |list| switch (index.kind) {
                .Int => {
                    var i = index.kind.Int;
                    if (i < 0)
                        i += @intCast(i64, list.len);
                    if (i < 0 or i >= list.len)
                        return vm.reportErr("index out of bounds");

                    res.* = list.at(@intCast(u32, i));
                },
                .Range => return vm.reportErr("TODO get with ranges"),
                .Str => |s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "len")) {
                        res.*.?.* = .{
                            .kind = .{ .Int = @intCast(i64, list.len) },
                        };
                    } else {
                        return vm.reportErr("no such property");
                    }
                },
                else => return vm.reportErr("invalid index type"),
            },
            .Map => |map| {
                return vm.reportErr("TODO get map");
            },
            .Str => |str| switch (index.kind) {
                .Int => return vm.reportErr("TODO get str"),
                .Range => return vm.reportErr("TODO get with ranges"),
                .Str => |s| {
                    if (res.* == null) {
                        res.* = try vm.gc.alloc();
                    }

                    if (mem.eql(u8, s, "len")) {
                        res.*.?.* = .{
                            .kind = .{ .Int = @intCast(i64, str.len) },
                        };
                    } else {
                        return vm.reportErr("no such property");
                    }
                },
                else => return vm.reportErr("invalid index type"),
            },
            .Iterator => unreachable,
            else => return vm.reportErr("invalid subscript type"),
        }
    }

    pub fn set(val: *Value, vm: *Vm, index: *Value, new_val: *Value) !void {
        switch (val.kind) {
            .Tuple => |tuple| if (index.kind == .Int) {
                var i = index.kind.Int;
                if (i < 0)
                    i += @intCast(i64, tuple.len);
                if (i < 0 or i >= tuple.len)
                    return vm.reportErr("index out of bounds");

                tuple[@intCast(u32, i)].* = new_val.*;
            } else {
                return vm.reportErr("TODO set with ranges");
            },
            .Map => |map| {
                return vm.reportErr("TODO set map");
            },
            .List => |list| if (index.kind == .Int) {
                var i = index.kind.Int;
                if (i < 0)
                    i += @intCast(i64, list.len);
                if (i < 0 or i >= list.len)
                    return vm.reportErr("index out of bounds");

                list.toSlice()[@intCast(u32, i)].* = new_val.*;
            } else {
                return vm.reportErr("TODO set with ranges");
            },
            .Str => |str| {
                return vm.reportErr("TODO set string");
            },
            .Iterator => unreachable,
            else => return vm.reportErr("invalid subscript type"),
        }
    }

    /// `type_id` must be valid and cannot be .Error, .Range, .Fn or .Native
    pub fn as(val: *Value, vm: *Vm, type_id: TypeId) !*Value {
        if (type_id == .None) {
            return &Value.None;
        }
        if (type_id == val.kind) {
            return val;
        }

        if (type_id == .Bool) {
            const bool_res = switch (val.kind) {
                .Int => |int| int != 0,
                .Num => |num| num != 0,
                .Bool => unreachable,
                .Str => |str| if (mem.eql(u8, str, "true"))
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
            .Bool, .None, .Error, .Range, .Fn, .Native, .Iterator => unreachable,
            .Int => .{
                .kind = .{
                    .Int = switch (val.kind) {
                        .Int => unreachable,
                        .Num => |num| @floatToInt(i64, num),
                        .Bool => |b| @boolToInt(b),
                        .Str => |str| util.parseInt(str) catch
                            return vm.reportErr("invalid cast to int"),
                        else => return vm.reportErr("invalid cast to int"),
                    },
                },
            },
            .Num => .{
                .kind = .{
                    .Num = switch (val.kind) {
                        .Num => unreachable,
                        .Int => |int| @intToFloat(f64, int),
                        .Bool => |b| @intToFloat(f64, @boolToInt(b)),
                        .Str => |str| util.parseNum(str) catch
                            return vm.reportErr("invalid cast to num"),
                        else => return vm.reportErr("invalid cast to num"),
                    },
                },
            },
            .Str,
            .Tuple,
            .Map,
            .List,
            => return vm.reportErr("TODO more casts"),
            _ => unreachable,
        };
        return new_val;
    }

    pub fn is(val: *Value, type_id: TypeId) bool {
        if (val.kind == type_id) return true;
        if (type_id == .Fn and val.kind == .Native) return true;
        return false;
    }

    pub fn in(val: *Value, container: *Value) bool {
        switch (container.kind) {
            .Str => |str| {
                if (val.kind != .Str) return false;
                return mem.indexOf(u8, str, val.kind.Str) != null;
            },
            .Tuple => |tuple| {
                for (tuple) |v| {
                    if (v.eql(val)) return true;
                }
                return false;
            },
            .List => |list| {
                for (list.toSliceConst()) |v| {
                    if (v.eql(val)) return true;
                }
                return false;
            },
            .Map => @panic("TODO in map"),
            .Range => @panic("TODO in range"),
            .Iterator => unreachable,
            else => unreachable,
        }
    }

    pub fn iterator(val: *Value, vm: *Vm) !*Value {
        switch (val.kind) {
            .Map => return vm.reportErr("TODO: map iterator"),
            .Range => return vm.reportErr("TODO: range iterator"),
            .Str, .Tuple, .List => {},
            .Iterator => unreachable,
            else => return vm.reportErr("invalid type for iteration"),
        }
        const iter = try vm.gc.alloc();
        iter.* = .{
            .kind = .{
                .Iterator = .{
                    .value = val,
                    .index = 0,
                },
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
    const result = out_buf.toSliceConst();

    if (!std.mem.eql(u8, result, expected)) {
        std.debug.warn("\n---expected----\n{}\n-----found-----\n{}\n---------------\n", .{ expected, result });
        return error.TestFailed;
    }
}

// TODO these cause a false dependency loop
// https://github.com/ziglang/zig/issues/4562
// test "dump int/num" {
//     var int = Value{
//         .kind = .{ .Int = 2 },
//     };
//     try testDump(int, "2");
//     var num = Value{
//         .kind = .{ .Num = 2.5 },
//     };
//     try testDump(num, "2.5");
// }

// test "dump error" {
//     var int = Value{
//         .kind = .{ .Int = 2 },
//     };
//     var err = Value{
//         .kind = .{
//             .Error = &int,
//         },
//     };
//     try testDump(err, "error(2)");
// }
