const std = @import("std");

pub const Value = struct {
    pub const TypeId = enum {
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
    };

    pub const Map = std.StringHashMap(*Ref);
    pub const List = std.ArrayList(*Ref);

    ref: u32 = 0,
    kind: union(TypeId) {
        Tuple: []Ref,
        Map: Map,
        List: *List,
        Error: *Ref,
        Int: i64,
        Num: f64,
        Range: struct {
            begin: *Ref,
            end: *Ref,
        },
        Str: []const u8,
        Fn: struct {
            arg_count: u8,
        },

        /// always inferred
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
};

pub const Ref = struct {
    value: ?*Value,

    pub fn dump(ref: Ref, stream: var, level: u32) (@TypeOf(stream.*).Error || error{Unimplemented})!void {
        const value = ref.value.?;
        switch (value.kind) {
            .Int => |val| try stream.print("{}", .{val}),
            .Num => |val| try stream.print("{}", .{val}),
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
            .List => {
                if (level == 0) {
                    try stream.write("[...]");
                } else {
                    return error.Unimplemented;
                }
            },
            .Error => |val| {
                try stream.write("error(...)");
                if (level == 0) {
                    try stream.write("(range)");
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
        }
    }
};
