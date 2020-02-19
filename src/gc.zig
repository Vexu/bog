const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const value = @import("value.zig");
const Value = value.Value;
const Ref = value.Ref;

// TODO allocator interface
pub const Gc = struct {
    values: Pool,
    stack: Stack,

    const Stack = std.ArrayList(Ref);
    const Pool = std.SegmentedList(Value, 256);

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

    pub fn alloc(gc: *Gc) !Ref {
        const val = try gc.values.addOne();
        return Ref{
            .value = val,
        };
    }

    pub fn free(gc: *Gc, Ref) void {}

    pub fn stackAlloc(gc: *Gc, count: u8) !void {
        try gc.stack.resize(count);
        mem.set(Ref, gc.stack.toSlice()[gc.stack.len - count ..], Ref{ .value = null });
    }

    pub fn stackFree(gc: *Gc, count: usize) void {
        gc.stack.resize(stack.len) catch unreachable;
    }
};
