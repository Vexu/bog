const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const bog = @import("bog.zig");
const Value = bog.Value;

//! A generational non-moving garbage collector.
//! Inspired by https://www.pllab.riec.tohoku.ac.jp/papers/icfp2011UenoOhoriOtomoAuthorVersion.pdf

/// A pool of values prefixed with a header containing two bitmaps for
/// the old and young generation.
const Page = struct {
    const max_size = 1_048_576;
    comptime {
        // 2^20, 1 MiB
        assert(@sizeOf(Page) == max_size);
    }
    const val_count = 25_574;
    const pad_size = max_size - @sizeOf(u32) - (@sizeOf(Value) + @sizeOf(State)) * val_count;

    const State = enum(u8) {
        empty,
        white,
        gray,
        black,
    };

    /// States of all values.
    meta: [val_count]State,
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

    fn deinit(page: *Page, gc: *Gc) void {
        for (page.meta) |s, i| {
            if (s == .empty) continue;
            page.values[i].deinit(gc.gpa);
        }
        std.heap.page_allocator.destroy(page);
    }

    fn alloc(page: *Page) ?*Value {
        while (page.free < page.values.len) {
            defer page.free += 1;

            if (page.meta[page.free] == .empty) {
                page.meta[page.free] = .white;
                return &page.values[page.free];
            }
        }
        return null;
    }
};

const Gc = @This();

pages: std.ArrayListUnmanaged(*Page),
stack: std.ArrayListUnmanaged(?*Value),
gpa: *Allocator,

const PageAndIndex = struct {
    page: *Page,
    index: usize,
};

fn findInPage(gc: *Gc, value: *Value) PageAndIndex {
    for (gc.pages.items) |page| {
        // is the value before this page
        if (@ptrToInt(value) < @ptrToInt(page)) continue;
        // is the value after this page
        if (@ptrToInt(value) > @ptrToInt(page) + @sizeOf(Page)) continue;

        // value is in this page
        return .{
            .page = page,
            // calculate index from offset from `Page.values`
            .index = (@ptrToInt(value) - (@ptrToInt(page) + @byteOffsetOf(Page, values))) / @sizeOf(Value),
        };
    }

    unreachable; // value was not allocated by the gc.
}

pub fn init(allocator: *Allocator) Gc {
    return .{
        .pages = .{},
        .stack = .{},
        .gpa = allocator,
    };
}

/// Frees all values and their allocations.
pub fn deinit(gc: *Gc) void {
    for (gc.pages.items) |page| page.deinit(gc);
    gc.pages.deinit(gc.gpa);
    gc.stack.deinit(gc.gpa);
}

/// Allocate a new Value on the heap.
pub fn alloc(gc: *Gc) !*Value {
    if (gc.pages.items.len == 0) {
        const page = try Page.create();
        errdefer page.deinit(gc);
        try gc.pages.append(gc.gpa, page);

        // we just created this page so it is empty.
        return page.alloc() orelse unreachable;
    }

    for (gc.pages.items) |page| {
        if (page.alloc()) |some| return some;
    }

    // TODO collect
    return error.OutOfMemory;
}

/// Allocates a shallow copy of `val`.
pub fn dupe(gc: *Gc, val: *const Value) !*Value {
    // no need to copy always memoized values
    switch (val.*) {
        .none => return &Value.None,
        .bool => |b| return if (b) &Value.True else &Value.False,
        else => {},
    }

    const new = try gc.alloc();
    switch (val.*) {
        .list => |*l| {
            new.* = .{ .list = .{} };
            try new.list.appendSlice(gc.gpa, l.items);
        },
        .tuple => |t| {
            new.* = .{ .tuple = try gc.gpa.dupe(*Value, t) };
        },
        .map => |*m| {
            new.* = .{ .map = try m.clone(gc.gpa) };
        },
        .str => |*s| {
            if (s.capacity != 0) {
                new.* = Value.string(try gc.gpa.dupe(u8, s.data));
            } else {
                new.* = val.*;
            }
        },
        else => new.* = val.*,
    }
    return new;
}

/// Get value from stack at `index`.
/// Returns `error.NullPtrDeref` if stack has no value at `index`.
pub fn stackGet(gc: *Gc, index: usize) !*Value {
    if (index >= gc.stack.items.len)
        return error.NullPtrDeref;

    return gc.stack.items[index] orelse
        error.NullPtrDeref;
}

/// Only valid until next `stackAlloc` call.
pub fn stackRef(gc: *Gc, index: usize) !*?*Value {
    while (index >= gc.stack.items.len) {
        try gc.stack.append(gc.gpa, null);
    }
    return &gc.stack.items[index];
}

/// Allocates new value on stack, invalidates all references to stack values.
pub fn stackAlloc(gc: *Gc, index: usize) !*Value {
    const val = try gc.stackRef(index);
    if (val.*) |some| switch (some.*) {
        .int, .num, .native, .tagged, .str => {},
        else => val.* = try gc.alloc(),
    } else {
        val.* = try gc.alloc();
    }
    return val.*.?;
}

/// Shrinks stack to `size`, doesn't free any memory.
pub fn stackShrink(gc: *Gc, size: usize) void {
    if (size > gc.stack.items.len) return;
    gc.stack.items.len = size;
}
