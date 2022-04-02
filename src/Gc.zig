//! A non-moving garbage collector.
//! Inspired by https://www.pllab.riec.tohoku.ac.jp/papers/icfp2011UenoOhoriOtomoAuthorVersion.pdf
const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const log = std.log.scoped(.gc);
const bog = @import("bog.zig");
const Value = bog.Value;
const Type = bog.Type;
const expect = std.testing.expect;

/// A pool of values prefixed with a header containing two bitmaps for
/// the old and young generation.
const Page = struct {
    const max_size = 1_048_576;
    comptime {
        // 2^20, 1 MiB
        assert(@sizeOf(Page) == max_size);
    }
    const val_count = @divFloor(max_size - @sizeOf(u32), @sizeOf(Value));
    const pad_size = max_size - @sizeOf(u32) - @sizeOf(Value) * val_count;

    const List = std.ArrayListUnmanaged(*Page);

    /// Padding to ensure size is 1 MiB.
    __padding: [pad_size]u8 = @compileError("do not initiate directly"),

    /// Index to the first free slot.
    free: u32,

    /// Actual values, all pointers will stay valid as long as they are
    /// referenced from a root.
    values: [val_count]Value,

    fn create() !*Page {
        const page = try std.heap.page_allocator.create(Page);
        mem.set(usize, mem.bytesAsSlice(usize, mem.asBytes(page)), 0);
        return page;
    }

    fn destroy(page: *Page, gc: *Gc) void {
        for (page.values) |*val| {
            if (val.state == .empty) continue;
            val.deinit(gc.gpa);
        }
        std.heap.page_allocator.destroy(page);
    }

    fn alloc(page: *Page) ?*Value {
        while (page.free < page.values.len) {
            defer page.free += 1;

            const val = &page.values[page.free];
            if (val.state == .empty) {
                // initialize to a harmless value
                val.* = Value.int(0);
                return val;
            }
        }
        return null;
    }

    fn clear(page: *Page, gc: *Gc) u32 {
        var freed: u32 = 0;
        var i: u32 = val_count;
        while (i > 0) {
            i -= 1;
            const val = &page.values[i];
            switch (val.state) {
                .black, .gray => {
                    // value lives to see another day
                    val.state = .white;
                },
                .white => {
                    freed += 1;
                    val.deinit(gc.gpa);
                    val.state = .empty;
                    page.free = i;
                },
                .empty => {},
            }
        }
        return freed;
    }

    fn has(page: *Page, value: *Value) bool {
        return @ptrToInt(value) >= @ptrToInt(&page.values[0]) and
            @ptrToInt(value) <= @ptrToInt(&page.values[page.values.len - 1]);
    }
};

const Gc = @This();

values: Page.List = .{},
gray_stack: std.ArrayListUnmanaged(*Value) = .{},
gpa: Allocator,
page_limit: u32,
stack_protect_start: usize = 0,
allocated: u32 = 0,

fn markVal(gc: *Gc, maybe_val: ?*Value) !void {
    const val = maybe_val orelse return;
    if (val.state != .white) return;
    switch (val.ty) {
        .list,
        .tuple,
        .map,
        .err,
        .func,
        .frame,
        .iterator,
        .spread,
        .tagged,
        => {
            val.state = .gray;
            try gc.gray_stack.append(gc.gpa, val);
        },
        // These values contain no references to other values;
        // directly mark them black.
        .native,
        .str,
        .int,
        .num,
        .range,
        => val.state = .black,
        .@"null", .bool => {},
    }
}

fn markGray(gc: *Gc) !void {
    while (gc.gray_stack.popOrNull()) |val| {
        val.state = .black;
        switch (val.ty) {
            .list => {
                for (val.v.list.inner.items) |elem| {
                    try gc.markVal(elem);
                }
            },
            .tuple => {
                for (val.v.tuple) |elem| {
                    try gc.markVal(elem);
                }
            },
            .map => {
                var iter = val.v.map.iterator();
                while (iter.next()) |entry| {
                    try gc.markVal(entry.key_ptr.*);
                    try gc.markVal(entry.value_ptr.*);
                }
            },
            .err => {
                try gc.markVal(val.v.err);
            },
            .func => {
                for (val.v.func.captures()) |elem| {
                    try gc.markVal(elem);
                }
            },
            .frame => {
                for (val.v.frame.stack.items) |item| {
                    try gc.markVal(item);
                }
                for (val.v.frame.captures) |capture| {
                    try gc.markVal(capture);
                }
                try gc.markVal(val.v.frame.this);
            },
            .iterator => {
                try gc.markVal(val.v.iterator.value);
            },
            .spread => {
                try gc.markVal(val.v.spread.iterable);
            },
            .tagged => {
                try gc.markVal(val.v.tagged.value);
            },
            // These values do not contain references to other values
            .native, .str, .int, .num, .range, .@"null", .bool => {},
        }
    }
}

/// Collect all unreachable values.
pub fn collect(gc: *Gc) !usize {
    // mark roots as reachable
    if (gc.stack_protect_start != 0) {
        var i = @intToPtr([*]*Value, gc.stack_protect_start);
        outer: while (@ptrToInt(i) > @frameAddress()) : (i -= 1) {
            for (gc.values.items) |page| {
                if (page.has(i[0])) {
                    try gc.markVal(i[0]);
                    continue :outer;
                }
            }
        }
    }

    // mark values referenced from root values as reachable
    try gc.markGray();

    // free all unreachable values
    var freed: u32 = 0;
    for (gc.values.items) |page| {
        freed += page.clear(gc);
    }
    log.info("collected {d} out of {d} objects ({d:.2}%)", .{
        freed,
        gc.allocated,
        (@intToFloat(f32, freed) / @intToFloat(f32, gc.allocated)) * 100,
    });
    gc.allocated -= freed;
    return freed;
}

pub fn init(allocator: Allocator, page_limit: u32) Gc {
    std.debug.assert(page_limit >= 1);
    return .{
        .gpa = allocator,
        .page_limit = page_limit,
    };
}

/// Frees all values and their allocations.
pub fn deinit(gc: *Gc) void {
    for (gc.values.items) |page| page.destroy(gc);
    gc.values.deinit(gc.gpa);
    gc.gray_stack.deinit(gc.gpa);
}

/// Allocate a new Value on the heap.
pub fn alloc(gc: *Gc) !*Value {
    if (gc.values.items.len == 0) {
        const page = try Page.create();
        errdefer page.destroy(gc);
        try gc.values.append(gc.gpa, page);

        // we just created this page so it is empty.
        gc.allocated += 1;
        return page.alloc() orelse unreachable;
    }

    for (gc.values.items) |page| {
        if (page.alloc()) |some| {
            gc.allocated += 1;
            return some;
        }
    }

    const freed = try gc.collect();

    const threshold = 0.75;
    const new_capacity = @intToFloat(f32, freed) / @intToFloat(f32, gc.allocated);

    if (new_capacity < threshold and gc.values.items.len != gc.page_limit) {
        log.info("collected {d}, allocating a new page", .{freed});

        const page = try Page.create();
        errdefer page.destroy(gc);
        try gc.values.append(gc.gpa, page);

        // we just created this page so it is empty.
        gc.allocated += 1;
        return page.alloc() orelse unreachable;
    } else if (freed != 0) {
        // we just freed a whole bunch of values, allocation cannot fail
        return gc.alloc() catch unreachable;
    }

    // no values could be collected and page_limit has been reached
    return error.OutOfMemory;
}

/// Allocates a shallow copy of `val`.
pub fn dupe(gc: *Gc, val: *const Value) !*Value {
    // no need to copy always memoized values
    if (val == Value.Null) return Value.Null;
    if (val == Value.True) return Value.True;
    if (val == Value.False) return Value.False;

    const new = try gc.alloc();
    switch (val.ty) {
        .list => {
            new.* = Value.list();
            try new.v.list.inner.appendSlice(gc.gpa, val.v.list.inner.items);
        },
        .tuple => {
            new.* = Value.tuple(try gc.gpa.dupe(*Value, val.v.tuple));
        },
        .map => {
            new.* = Value.map();
            new.v.map = try val.v.map.clone(gc.gpa);
        },
        .str => {
            if (val.v.str.capacity != 0) {
                new.* = Value.string(try gc.gpa.dupe(u8, val.v.str.data));
            } else {
                new.* = val.*;
            }
        },
        else => new.* = val.*,
    }
    return new;
}

test "stack protect" {
    if (@import("builtin").os.tag == .windows) {
        // TODO @frameAddress returns an address after &val1 on windows?
        return error.SkipZigTest;
    }
    var gc = Gc.init(std.testing.allocator, 2);
    defer gc.deinit();

    gc.stack_protect_start = @frameAddress();

    _ = try gc.alloc();
    _ = try gc.alloc();

    try expect(gc.collect() == 0);

    gc.stack_protect_start = 0;
    try expect(gc.collect() == 2);
}
