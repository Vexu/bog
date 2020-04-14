const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = mem.Allocator;
const value = @import("value.zig");
const Value = value.Value;

pub const Gc = struct {
    values: Pool,
    stack: Stack,

    const Stack = std.ArrayList(?*Value);
    const Pool = struct {
        items: []Value,
        free: usize = 0,
        allocator: *Allocator,

        const initial_size = @divExact(mem.page_size * 16, @sizeOf(Value));

        fn init(allocator: *Allocator) !Pool {
            return Pool{
                .allocator = allocator,
                .items = try allocator.alloc(Value, initial_size),
            };
        }

        fn deinit(pool: *Pool) void {
            pool.allocator.free(pool.items);
        }
    };

    pub fn init(allocator: *Allocator) !Gc {
        return Gc{
            .stack = Stack.init(allocator),
            .values = try Pool.init(allocator),
        };
    }

    pub fn deinit(gc: *Gc) void {
        gc.stack.deinit();
        gc.values.deinit();
    }

    pub fn alloc(gc: *Gc) !*Value {
        if (gc.values.free == gc.values.items.len) {
            try gc.collect();
            @panic("TODO collect");
        }
        const val = &gc.values.items[gc.values.free];
        gc.values.free += 1;
        return val;
    }

    pub fn collect(gc: *Gc) !void {
        gc.mark();
        if (gc.sweep() != 0) {
            // TODO compact/move
        }
    }

    pub fn mark(gc: *Gc) void {
        for (gc.stack.span()) |val| {
            if (val) |some| {
                some.mark();
            }
        }
    }

    pub fn sweep(gc: *Gc) u32 {
        var freed: u32 = 0;
        for (gc.values.items) |*val| {
            if (val.marked) {
                val.marked = false;
            } else {
                freed += 1;
            }
        }
        return freed;
    }

    pub fn stackGet(gc: *Gc, index: usize) !*Value {
        if (index > gc.stack.items.len)
            return error.NullPtrDeref;

        return gc.stack.span()[index] orelse
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

    /// Only valid while no allocations happen
    pub fn stackRef(gc: *Gc, index: usize) !*?*Value {
        while (index >= gc.stack.items.len) {
            try gc.stack.append(null);
        }
        return &gc.stack.span()[index];
    }

    pub fn stackShrink(gc: *Gc, size: usize) void {
        if (size > gc.stack.items.len) return;
        gc.stack.items.len = size;
    }
};
