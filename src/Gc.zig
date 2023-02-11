//! A non-moving garbage collector.

const std = @import("std");
const Allocator = std.mem.Allocator;
const bog = @import("bog.zig");
const Value = bog.Value;
const Type = bog.Type;

/// the number of allocated objects per page
const PAGE_LEN = 16; // 32 * 1024;

comptime {
    if (@popCount(@as(usize, PAGE_LEN)) != 1) {
        @compileError("PAGE_LEN must be a power of 2.");
    }
}

/// a value with a header
const Object = struct {
    page_index: u32,
    generation: u8,
    value: Value,
};

/// tracks live objects in a block of memory
const Page = struct {
    const SET_SIZE = PAGE_LEN << 1;
    /// the index into StateSet that represents the first object
    const SET_BASE = PAGE_LEN;
    const StateSet = std.StaticBitSet(SET_SIZE);

    /// the raw object memory
    data: [PAGE_LEN]Object = undefined,
    /// used to track empty objects
    chalkboard: StateSet = StateSet.initEmpty(),

    /// used for indexing into chalkboard
    const Bit = struct {
        index: usize,

        const ROOT = Bit.of(1);

        fn of(index: usize) Bit {
            std.debug.assert(index != 0);
            return Bit{ .index = index };
        }

        fn isRoot(bit: Bit) bool {
            return bit.index == 1;
        }

        fn isLeaf(bit: Bit) bool {
            return bit.index >= SET_BASE;
        }

        fn parent(bit: Bit) Bit {
            return Bit.of(bit.index >> 1);
        }

        fn left(bit: Bit) Bit {
            return Bit.of(bit.index << 1);
        }

        fn right(bit: Bit) Bit {
            return Bit.of((bit.index << 1) | 1);
        }

        fn sibling(bit: Bit) Bit {
            return Bit.of(bit.index ^ 1);
        }
    };

    fn init(ally: Allocator) Allocator.Error!*Page {
        const self = try ally.create(Page);
        self.* = .{};

        return self;
    }

    fn isFull(self: *const Page) bool {
        return self.get(Bit.ROOT);
    }

    fn get(self: *const Page, bit: Bit) bool {
        return self.chalkboard.isSet(bit.index - 1);
    }

    fn set(self: *Page, bit: Bit) void {
        self.chalkboard.set(bit.index - 1);
    }

    fn unset(self: *Page, bit: Bit) void {
        self.chalkboard.unset(bit.index - 1);
    }

    /// whether an object lives
    fn lives(self: *Page, index: usize) bool {
        return self.get(Bit.of(index + SET_BASE));
    }

    /// estimates how full this page is to determine whether it is 'saturated'
    /// (should be collected)
    fn isSaturated(self: *const Page) bool {
        const LEVEL = 3;
        const N = 1 << LEVEL; // both the index and the length of the level

        comptime {
            if (PAGE_LEN < (1 << LEVEL)) {
                @compileError("isSaturated LEVEL is too large for PAGE_LEN");
            }
        }

        // count set bits in level
        var count: usize = 0;
        var i: usize = 0;
        while (i < N) : (i += 1) {
            const index = i + N;
            count += @boolToInt(self.chalkboard.isSet(index));
        }

        // if this page is 3/4 full, it is saturated
        const ratio = @intToFloat(f64, count) / @intToFloat(f64, N);
        return ratio > 0.75;
    }

    /// if this page is saturated, collects all objects of the wrong generation
    fn collect(self: *Page, ally: Allocator, gen: u8) void {
        std.debug.print("collecting a page\n", .{});
        if (!self.isSaturated()) {
            std.debug.print("unsaturated\n", .{});
            return;
        }

        for (self.data) |*obj, index| {
            if (self.lives(index) and obj.generation != gen) {
                self.setFree(index);
                obj.value.deinit(ally);
            }
        }
    }

    /// finds the index of the first free object
    fn firstFreeObject(self: *const Page) ?usize {
        if (self.isFull()) {
            return null;
        }

        // traverse chalkboard to find first open index, setting bits in the
        // process
        var bit = Bit.ROOT;
        while (!bit.isLeaf()) {
            const lhs = bit.left();
            const rhs = bit.right();

            bit = if (self.get(lhs)) rhs else lhs;
        }

        return bit.index - SET_BASE;
    }

    fn setUsed(self: *Page, obj_index: usize) void {
        // while all children are used, iterate through parents and set each
        // the parent bit to used as well
        var bit = Bit.of(obj_index + SET_BASE);
        while (true) : (bit = bit.parent()) {
            self.set(bit);
            if (bit.isRoot() or !self.get(bit.sibling())) break;
        }
    }

    fn setFree(self: *Page, obj_index: usize) void {
        // iterate through all parents and set each parent bit to free
        var bit = Bit.of(obj_index + SET_BASE);
        while (true) : (bit = bit.parent()) {
            self.unset(bit);
            if (bit.isRoot()) break;
        }
    }

    fn alloc(self: *Page, page_index: u32, gen: u8) error{PageFull}!*Object {
        const index = self.firstFreeObject() orelse {
            return error.PageFull;
        };

        self.setUsed(index);

        const obj = &self.data[index];
        obj.page_index = page_index;
        obj.generation = gen;

        std.debug.print(
            "alloc: page {} gen {} index {}\n",
            .{ page_index, gen, index },
        );

        return obj;
    }

    fn free(self: *Page, obj: *Object) error{WrongPage}!void {
        const base = @ptrToInt(&self.data);
        const element = @ptrToInt(obj);

        if (element < base or element >= base + self.data.len) {
            return error.WrongPage;
        }

        const index = @divExact(element - base, @sizeOf(Object));
        self.setFree(index);

        std.debug.print(
            "free: page {} gen {} index {}\n",
            .{ obj.page_index, obj.generation, index },
        );
    }
};

const Gc = @This();

gpa: Allocator,
page_limit: u32,
generation: u8 = 0,
/// every bog program has a base frame which references all live values
base_frame: ?*Value = null,
pages: std.ArrayListUnmanaged(*Page) = .{},

pub fn init(ally: Allocator, page_limit: u32) Gc {
    std.debug.assert(page_limit > 0);
    return Gc{
        .gpa = ally,
        .page_limit = page_limit,
    };
}

/// Frees all values and their allocations.
pub fn deinit(gc: *Gc) void {
    for (gc.pages.items) |page| gc.gpa.destroy(page);
    gc.pages.deinit(gc.gpa);
}

/// call at beginning of execution
pub fn setBaseFrame(gc: *Gc, frame: *Value) !void {
    gc.base_frame = frame;
}

/// call at end of execution
pub fn clearBaseFrame(gc: *Gc) void {
    gc.base_frame = null;
}

/// used by the gc and native values
///
/// sets the generation of the value's header to the current gc generation
pub fn markVal(gc: *Gc, maybe_value: ?*const Value) void {
    const value = maybe_value orelse return;

    // mark header
    const obj = @qualCast(*Object, @fieldParentPtr(Object, "value", value));
    obj.generation = gc.generation;

    // traverse children
    switch (value.*) {
        // memoized
        .null, .bool => {},
        // simple
        .native, .str, .int, .num, .range => {},
        // aggregate
        .list => |list| {
            for (list.inner.items) |val| {
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
        .spread => |spread| {
            gc.markVal(spread.iterable);
        },
        .tagged => |tag| {
            gc.markVal(tag.value);
        },
        .native_val => |n| {
            if (n.vtable.markVal) |some| {
                some(n.ptr, gc);
            }
        },
    }
}

/// attempt to allocate a value without creating a new page
fn noAlloc(gc: *Gc) ?*Value {
    var i = @intCast(u32, gc.pages.items.len);
    while (i > 0) {
        i -= 1;

        const page = gc.pages.items[i];
        if (!page.isFull()) {
            const obj = page.alloc(i, gc.generation) catch |e| switch (e) {
                error.PageFull => unreachable,
            };

            return &obj.value;
        }
    }

    return null;
}

/// iterate the generation, collect within each page
fn collect(gc: *Gc) void {
    gc.generation +%= 1;
    gc.markVal(gc.base_frame);

    for (gc.pages.items) |page| {
        page.collect(gc.gpa, gc.generation);
    }
}

pub fn alloc(gc: *Gc) Allocator.Error!*Value {
    // attempt to allocate from an already allocated page
    if (gc.noAlloc()) |value| {
        return value;
    }

    // collect and then try again
    gc.collect();

    if (gc.noAlloc()) |value| {
        return value;
    }

    // there is really no memory. make a new page and assert success
    const page = try Page.init(gc.gpa);
    try gc.pages.append(gc.gpa, page);

    return gc.noAlloc().?;
}

/// Allocates a shallow copy of `val`.
pub fn dupe(gc: *Gc, val: *const Value) !*Value {
    // no need to copy always memoized values
    if (val == Value.Null) return Value.Null;
    if (val == Value.True) return Value.True;
    if (val == Value.False) return Value.False;

    const new = try gc.alloc();
    switch (val.*) {
        .list => |*l| {
            new.* = .{ .list = .{} };
            try new.list.inner.appendSlice(gc.gpa, l.inner.items);
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
