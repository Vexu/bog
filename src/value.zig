const std = @import("std");

const ValueMap = std.StringHashMap(*Value);
const ValueList = std.ArrayList(*Value);

pub const Value = struct {
    ref: u32 = 0,
    mut: bool = false,
    kind: union(enum) {
        Tuple: []*Value,
        Map: ValueMap,
        Array: *ValueList,
        Error: *Value,
        Int: i64,
        Float: f64,
        Range: struct {
            begin: *Value,
            end: *Value,
        },

        /// always inferred
        Bool: bool,
        Fn: struct {
            arg_count: u8,
        },
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
};

pub const Ref = struct {
    value: *Value,
};
