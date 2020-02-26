const std = @import("std");
const mem = std.mem;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Module = bog.Module;
const NativeFn = bog.NativeFn;

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

        /// Native being separate from .Fn is an implementation detail
        Native,
        _,
    };

    pub const Map = std.StringHashMap(*Value);
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
        Native: struct {
            arg_count: u8,

            func: NativeFn,
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
            .Int, .Num => unreachable,
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
            .Error => |val| @panic("TODO eql for errors"),
            .Range => |val| @panic("TODO eql for ranges"),
            .Fn => |val| {
                const b_val = b.kind.Fn;
                return val.offset == b_val.offset and
                    val.arg_count == b_val.arg_count and
                    val.module == b_val.module;
            },
            .Native => |val| return val.func == b.kind.Native.func,
            _ => unreachable,
        };
    }

    pub fn dump(value: Value, stream: var, level: u32) (@TypeOf(stream.*).Error || error{Unimplemented})!void {
        switch (value.kind) {
            .Int => |val| try stream.print("{}", .{val}),
            .Num => |val| try stream.print("{d}", .{val}),
            .Bool => |val| try stream.write(if (val) "true" else "false"),
            .None => try stream.write("()"),
            .Range => |val| {
                if (level == 0) {
                    try stream.write("(range)");
                } else {
                    try val.begin.dump(stream, level - 1);
                    try stream.write("...");
                    try val.end.dump(stream, level - 1);
                }
            },
            .Tuple => |val| {
                if (level == 0) {
                    try stream.write("(...)");
                } else {
                    try stream.writeByte('(');
                    for (val) |v, i| {
                        if (i != 0) try stream.write(", ");
                        try v.dump(stream, level - 1);
                    }
                    try stream.writeByte(')');
                }
            },
            .Map => {
                if (level == 0) {
                    try stream.write("{...}");
                } else {
                    return error.Unimplemented;
                }
            },
            .List => |val| {
                if (level == 0) {
                    try stream.write("[...]");
                } else {
                    try stream.writeByte('[');
                    for (val.toSliceConst()) |v, i| {
                        if (i != 0) try stream.write(", ");
                        try v.dump(stream, level - 1);
                    }
                    try stream.writeByte(']');
                }
            },
            .Error => |val| {
                if (level == 0) {
                    try stream.write("error(...)");
                } else {
                    try stream.write("error(");
                    try val.dump(stream, level - 1);
                    try stream.writeByte(')');
                }
            },
            .Str => |val| {
                try stream.writeByte('"');
                for (val) |c| {
                    switch (c) {
                        '\n' => try stream.write("\\n"),
                        '\t' => try stream.write("\\t"),
                        '\r' => try stream.write("\\r"),
                        '\'' => try stream.write("\\'"),
                        '"' => try stream.write("\\\""),
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

    pub fn get(val: *Value, index: *Value, vm: *Vm) !*Value {
        switch (val.kind) {
            .Tuple => |tuple| if (index.kind == .Int) {
                var i = index.kind.Int;
                if (i < 0)
                    i += @intCast(i64, tuple.len);
                if (i < 0 or i > tuple.len)
                    return vm.reportErr("index out of bounds");

                return tuple[@intCast(u32, i)];
            } else {
                return vm.reportErr("TODO get with ranges");
            },
            .Map => |map| {
                return vm.reportErr("TODO get map");
            },
            .List => |list| if (index.kind == .Int) {
                var i = index.kind.Int;
                if (i < 0)
                    i += @intCast(i64, list.len);
                if (i < 0 or i > list.len)
                    return vm.reportErr("index out of bounds");

                return list.toSliceConst()[@intCast(u32, i)];
            } else {
                return vm.reportErr("TODO get with ranges");
            },
            .Str => |str| {
                return vm.reportErr("TODO get string");
            },
            else => return vm.reportErr("invalid subscript type"),
        }
    }

    pub fn set(val: *Value, index: *Value, new_val: *Value, vm: *Vm) !void {
        switch (val.kind) {
            .Tuple => |tuple| if (index.kind == .Int) {
                var i = index.kind.Int;
                if (i < 0)
                    i += @intCast(i64, tuple.len);
                if (i < 0 or i > tuple.len)
                    return vm.reportErr("index out of bounds");

                tuple[@intCast(u32, i)] = new_val;
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
                if (i < 0 or i > list.len)
                    return vm.reportErr("index out of bounds");

                list.toSlice()[@intCast(u32, i)] = new_val;
            } else {
                return vm.reportErr("TODO set with ranges");
            },
            .Str => |str| {
                return vm.reportErr("TODO set string");
            },
            else => return vm.reportErr("invalid subscript type"),
        }
    }

    /// `type_id` must be valid and cannot be .Error, .Range, .Fn or .Native
    pub fn as(val: *Value, type_id: TypeId, vm: *Vm) !*Value {
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
            .Bool, .None, .Error, .Range, .Fn, .Native => unreachable,
            .Int => .{
                .kind = .{
                    .Int = switch (val.kind) {
                        .Int => unreachable,
                        .Num => |num| @floatToInt(i64, num),
                        .Bool => |b| @boolToInt(b),
                        // .Str => parseInt
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
                        // .Str => parseNum
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
            else => unreachable,
        }
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

test "dump int/num" {
    var int = Value{
        .kind = .{ .Int = 2 },
    };
    try testDump(int, "2");
    var num = Value{
        .kind = .{ .Num = 2.5 },
    };
    try testDump(num, "2.5");
}

test "dump error" {
    var int = Value{
        .kind = .{ .Int = 2 },
    };
    var err = Value{
        .kind = .{
            .Error = &int,
        },
    };
    try testDump(err, "error(2)");
}
