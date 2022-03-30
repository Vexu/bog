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
    const val_count = @divFloor(max_size - @sizeOf(u32) * 2, (@sizeOf(Value) + @sizeOf(State)));
    const pad_size = max_size - @sizeOf(u32) * 2 - (@sizeOf(Value) + @sizeOf(State)) * val_count;

    const State = enum {
        empty,
        white,
        gray,
        black,
    };

    const List = std.ArrayListUnmanaged(*Page);

    /// States of all values.
    meta: [val_count]State,
    /// Padding to ensure size is 1 MiB.
    __padding: [pad_size]u8 = @compileError("do not initiate directly"),

    /// Index to the first free slot.
    free: u32,

    /// used during the collection phase to detect whether the Gc should keep
    /// checking values in this page.
    marked: u32,

    /// Actual values, all pointers will stay valid as long as they are
    /// referenced from a root.
    values: [val_count]Value,

    fn create() !*Page {
        const page = try std.heap.page_allocator.create(Page);
        mem.set(usize, mem.bytesAsSlice(usize, mem.asBytes(page)), 0);
        return page;
    }

    fn destroy(page: *Page, gc: *Gc) void {
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

    fn clear(page: *Page, gc: *Gc) u32 {
        var freed: u32 = 0;
        var i: u32 = val_count;
        while (i > 0) {
            i -= 1;
            switch (page.meta[i]) {
                .black, .gray => {
                    // value lives to see another day
                    page.meta[i] = .white;
                },
                .white => {
                    freed += 1;
                    page.meta[i] = .empty;
                    page.values[i].deinit(gc.gpa);
                    page.free = i;
                },
                .empty => {},
            }
        }
        return freed;
    }

    fn indexOf(page: *Page, value: *const Value) ?u32 {
        // is the value before this page
        if (@ptrToInt(value) < @ptrToInt(&page.values[0])) return null;
        // is the value after this page
        if (@ptrToInt(value) > @ptrToInt(&page.values[page.values.len - 1])) return null;

        // value is in this page
        return @intCast(u32, (@ptrToInt(value) - @ptrToInt(&page.values[0])) / @sizeOf(Value));
    }
};

const Gc = @This();

simple_pages: Page.List = .{},
aggregate_pages: Page.List = .{},
gpa: Allocator,
page_limit: u32,
stack_protect_start: usize = 0,
allocated: u32 = 0,

const PageAndIndex = struct {
    page: *Page,
    index: usize,
};

fn markVal(gc: *Gc, maybe_value: ?*const Value) void {
    const value = maybe_value orelse return;
    // These will never be allocated
    if (value == Value.Null or
        value == Value.True or
        value == Value.False) return;

    for (gc.simple_pages.items) |page| {
        const index = page.indexOf(value) orelse continue;
        if (page.meta[index] == .white) {
            page.meta[index] = .black;
        }
        return;
    }

    for (gc.aggregate_pages.items) |page| {
        const index = page.indexOf(value) orelse continue;
        if (page.meta[index] == .white) {
            page.meta[index] = .gray;
        }
        return;
    }

    // Value was not allocated by gc.
}

fn markGray(gc: *Gc) void {
    // mark all pages as dirty
    for (gc.aggregate_pages.items) |page| {
        page.marked = Page.val_count;
    }

    // mark all white values reachable from gray values as gray
    var marked_any = true;
    while (marked_any) {
        marked_any = false;
        for (gc.aggregate_pages.items) |page| {
            if (page.marked == 0) continue;
            page.marked = 0;
            for (page.meta) |*s, i| {
                if (s.* != .gray) continue;

                s.* = .black;
                page.marked += 1;
                switch (page.values[i]) {
                    .list => |list| {
                        for (list.items) |val| {
                            gc.markVal(val);
                        }
                    },
                    .tuple => |tuple| {
                        for (tuple) |val| {
                            gc.markVal(val);
                        }
                    },
                    .map => |map| {
                        var iter = map.iterator();
                        while (iter.next()) |entry| {
                            gc.markVal(entry.key_ptr.*);
                            gc.markVal(entry.value_ptr.*);
                        }
                    },
                    .err => |err| {
                        gc.markVal(err);
                    },
                    .func => |func| {
                        for (func.captures()) |val| {
                            gc.markVal(val);
                        }
                    },
                    .frame => |frame| {
                        for (frame.stack.items) |val| {
                            gc.markVal(val);
                        }
                        for (frame.captures) |val| {
                            gc.markVal(val);
                        }
                        gc.markVal(frame.this);
                    },
                    .iterator => |iter| {
                        gc.markVal(iter.value);
                    },
                    .tagged => |tag| {
                        gc.markVal(tag.value);
                    },
                    // These values cannot be allocated in aggregate_pages
                    .native, .str, .int, .num, .range, .@"null", .bool => {},
                }
            }
            if (page.marked != 0) marked_any = true;
        }
    }
}

/// Collect all unreachable values.
pub fn collect(gc: *Gc) usize {
    // mark roots as reachable
    if (gc.stack_protect_start != 0) {
        var i = @intToPtr([*]*Value, gc.stack_protect_start);
        while (@ptrToInt(i) > @frameAddress()) : (i -= 1) {
            gc.markVal(i[0]);
        }
    }

    // mark values referenced from root values as reachable
    gc.markGray();

    // free all unreachable values
    var freed: u32 = 0;
    for (gc.simple_pages.items) |page| {
        freed += page.clear(gc);
    }
    for (gc.aggregate_pages.items) |page| {
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
    for (gc.aggregate_pages.items) |page| page.destroy(gc);
    gc.aggregate_pages.deinit(gc.gpa);
    for (gc.simple_pages.items) |page| page.destroy(gc);
    gc.simple_pages.deinit(gc.gpa);
}

/// Allocate a new Value on the heap.
pub fn alloc(gc: *Gc, ty: Type) !*Value {
    switch (ty) {
        .@"null" => unreachable,
        .bool => unreachable,
        .native,
        .str,
        .int,
        .num,
        .range,
        => return gc.allocExtra(&gc.simple_pages),
        .list,
        .tuple,
        .map,
        .err,
        .func,
        .frame,
        .iterator,
        .tagged,
        => return gc.allocExtra(&gc.aggregate_pages),
    }
}

fn allocExtra(gc: *Gc, pages: *Page.List) !*Value {
    if (pages.items.len == 0) {
        const page = try Page.create();
        errdefer page.destroy(gc);
        try pages.append(gc.gpa, page);

        // we just created this page so it is empty.
        gc.allocated += 1;
        return page.alloc() orelse unreachable;
    }

    for (pages.items) |page| {
        if (page.alloc()) |some| {
            gc.allocated += 1;
            return some;
        }
    }

    const freed = gc.collect();

    const threshold = 0.75;
    const new_capacity = @intToFloat(f32, freed) / @intToFloat(f32, gc.allocated);

    if (new_capacity < threshold and pages.items.len != gc.page_limit) {
        log.info("collected {d}, allocating a new page", .{freed});

        const page = try Page.create();
        errdefer page.destroy(gc);
        try pages.append(gc.gpa, page);

        // we just created this page so it is empty.
        gc.allocated += 1;
        return page.alloc() orelse unreachable;
    } else if (freed != 0) {
        // we just freed a whole bunch of values, allocation cannot fail
        return gc.allocExtra(pages) catch unreachable;
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

    const new = try gc.alloc(val.*);
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

test "stack protect" {
    if (@import("builtin").os.tag == .windows) {
        // TODO @frameAddress returns an address after &val1 on windows?
        return error.SkipZigTest;
    }
    var gc = Gc.init(std.testing.allocator, 2);
    defer gc.deinit();

    gc.stack_protect_start = @frameAddress();

    _ = try gc.alloc(.int);
    _ = try gc.alloc(.int);

    try expect(gc.collect() == 0);

    gc.stack_protect_start = 0;
    try expect(gc.collect() == 2);
}
