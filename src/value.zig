const std = @import("std");

const ValueMap = std.StringHashMap(*Ref);
const ValueList = std.ArrayList(*Ref);

pub const Value = struct {
    ref: u32 = 0,
    mut: bool = false,
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
};

pub const Ref = struct {
    value: *Value,
};
