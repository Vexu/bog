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

    pub fn free(gc: *Gc) void {
        gc.mark();
        if (gc.sweep() != 0) {
            // TODO compact/move
        }
    }

    pub fn mark(gc: *Gc) void {
        var it = gc.stack.iterator(0);
        while (it.next()) |val| {
            if (val.*) |some| {
                some.mark();
            }
        }
    }

    pub fn sweep(gc: *Gc) u32 {
        var freed: u32 = 0;
        var it = gc.values.iterator(0);
        while (it.next()) |val| {
            if (val.*.marked) {
                val.*.marked = false;
            } else {
                freed += 1;
            }
        }
        return freed;
    }

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

    pub fn stackShrink(gc: *Gc, size: usize) void {
        if (size > gc.stack.len) return;
        gc.stack.len = size;
    }
};
