const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const bog = @import("bog.zig");
const Value = bog.Value;

//! A generational non-moving garbage collector.
//! Inspired by https://www.pllab.riec.tohoku.ac.jp/papers/icfp2011UenoOhoriOtomoAuthorVersion.pdf

/// Simple bit map, `size` bytes big, has set and get,
fn BitMap(comptime size: usize) type {
    return struct {
        bytes: [size]u8,

        const Self = @This();

        fn get(self: Self, index: usize) bool {
            return self.bytes[index / 8] >> @truncate(u3, index % 8) & 1 != 0;
        }

        fn set(self: *Self, index: usize, val: bool) void {
            if (val) {
                self.bytes[index / 8] |= (@as(u8, 1) << @truncate(u3, index % 8));
            } else {
                self.bytes[index / 8] &= ~(@as(u8, 1) << @truncate(u3, index % 8));
            }
        }
    };
}

test "bitmap" {
    var map: BitMap(64) = undefined;
    mem.set(u8, mem.asBytes(&map), 0);

    const max = 64 * 8;

    var i: usize = 0;
    while (i < max) : (i += 1) {
        std.testing.expect(map.get(i) == false);
        if (i & 1 == 0) map.set(i, true);
    }

    i = 0;
    while (i < max) : (i += 1) {
        const expect = i & 1 == 0;
        std.testing.expect(map.get(i) == expect);
    }
}

/// A pool of values prefixed with a header containing two bitmaps for
/// the old and young generation.
const Page = struct {
    comptime {
        // 2^20, 1 MiB
        assert(@sizeOf(Page) == 1_048_576);
    }

    const bit_map_size = blk: {
        const v = @sizeOf(Value);
        const max_size = 1_048_576;

        var n = 2000;
        while (n * 3 + n * 8 * v < max_size) : (n += 1) {}

        break :blk n - 1;
    };
    const val_count = bit_map_size * 8;

    free: BitMap(bit_map_size),
    old: BitMap(bit_map_size),
    young: BitMap(bit_map_size),

    vals: [val_count]Value,
};
