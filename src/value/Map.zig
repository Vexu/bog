const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

const Map = @This();

const ValueMapContext = struct {
    pub fn hash(self: @This(), v: *const Value) u32 {
        _ = self;
        return Value.hash(v);
    }

    pub fn eql(self: @This(), a: *const Value, b: *const Value, b_index: usize) bool {
        _ = self;
        _ = b_index;
        return Value.eql(a, b);
    }
};

inner: std.array_hash_map.ArrayHashMapUnmanaged(*const Value, *Value, Value.ValueMapContext, true),
