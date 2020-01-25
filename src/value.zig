const std = @import("std");

const ValueMap = std.StringHashMap(*Ref);
const ValueList = std.ArrayList(*Ref);

pub const Value = struct {
    ref: u32 = 0,
    kind: union(enum) {
        Tuple: []*Ref,
        Map: ValueMap,
        List: *ValueList,
        Error: *Ref,
        Int: i64,
        Float: f64,
        Range: struct {
            begin: *Ref,
            end: *Ref,
        },
        Fn: struct {
            arg_count: u8,
        },

        /// always inferred
        Bool: bool,
        None,
    },

    pub var none = Value{
        .kind = .None,
    };
    pub var True = Value{
        .kind = .{ .Bool = true },
    };
    pub var False = Value{
        .kind = .{ .Bool = false },
    };

    pub fn dump(value: *Value, stream: var, level: u32) !void {
        switch (value.kind) {
            .Int => |val| try stream.print("{}", .{val}),
            .Float => |val| try stream.print("{}", .{val}),
            .Bool => |val| try stream.write(if (val) "true" else "false"),
            .None => try stream.write("()"),
            .Range => |val| {
                if (level == 0) {
                    try stream.write("(range)");
                } else {
                    return error.Unimplemented;
                    // try val.begin.dump(stream, level - 1);
                    // try stream.write("...");
                    // try val.end.dump(stream, level - 1);
                }
            },
            .Tuple => {
                if (level == 0) {
                try stream.write("(...)");
                } else {
                    return error.Unimplemented;
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
                try stream.write("[...]");
                if (level == 0) {
                    try stream.write("(range)");
                } else {
                    return error.Unimplemented;
                }
            },
            .Error => {
                try stream.write("error(...)");
                if (level == 0) {
                    try stream.write("(range)");
                } else {
                    try stream.write("error(");
                    // try val.end.dump(stream, level - 1);
                    try stream.writeByte(')');
                }
            },
            .Fn => |val| {
                try stream.print("fn({})", .{val.arg_count});
            },
        }
    }
};

pub const Ref = struct {
    value: *Value,
};
