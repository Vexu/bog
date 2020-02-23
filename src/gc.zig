const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const value = @import("value.zig");
const Value = value.Value;

pub const Gc = struct {
    values: Pool,
    stack: Stack,

    const Stack = std.SegmentedList(?*Value, 512);
    const Pool = std.SegmentedList(Value, 1024);

    pub fn init(allocator: *Allocator) Gc {
        return .{
            .stack = Stack.init(allocator),
            .values = Pool.init(allocator),
        };
    }

    pub fn deinit(gc: *Gc) void {
        gc.stack.deinit();
        gc.values.deinit();
    }

    pub fn alloc(gc: *Gc) !*Value {
        return try gc.values.addOne();
    }

    pub fn free(gc: *Gc, Ref) void {}

    pub fn stackGet(gc: *Gc, index: usize) !*Value {
        if (index > gc.stack.len)
            return error.NullPtrDeref;

        return gc.stack.at(index).* orelse
            error.NullPtrDeref;
    }

    pub fn stackAlloc(gc: *Gc, index: usize) !*Value {
        const val = try gc.stackRef(index);
        if (val.* == null or
            val.*.?.kind == .None or
            val.*.?.kind == .Bool)
        {
            val.* = try gc.alloc();
        }
        return val.*.?;
    }

    pub fn stackRef(gc: *Gc, index: usize) !*?*Value {
        while (index >= gc.stack.len) {
            try gc.stack.push(null);
        }
        return gc.stack.at(index);
    }
};
