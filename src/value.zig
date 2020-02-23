const std = @import("std");

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
            .None => true,
            .Bool => |val| val == b.kind.Bool,
            .Str => |val| {
                const b_val = b.kind.Str;
                return std.mem.eql(u8, val, b_val);
            },
            .Tuple => |val| {
                const b_val = b.kind.Tuple;
                return val.len == b_val.len;
            },
            .Map => |val| @panic("TODO eql for maps"),
            .List => |val| {
                const b_val = b.kind.List;
                return val.len == b_val.len;
            },
            .Error => |val| @panic("TODO eql for errors"),
            .Range => |val| @panic("TODO eql for ranges"),
            .Fn => |val| {
                const b_val = b.kind.Fn;
                return val.offset == b_val.offset and val.arg_count == b_val.arg_count;
            },
            else => unreachable,
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
                try stream.print("fn({})", .{val.arg_count});
            },
            _ => unreachable,
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
