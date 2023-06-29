const std = @import("std");
const debug = std.debug;
const assert = debug.assert;
const testing = std.testing;
const math = std.math;
const mem = std.mem;
const meta = std.meta;
const trait = meta.trait;
const autoHash = std.hash.autoHash;
const Wyhash = std.hash.Wyhash;
const Allocator = mem.Allocator;
const Value = @import("bog.zig").Value;

/// It is permitted to access this field directly.
entries: DataList = .{},

/// When entries length is less than `linear_scan_max`, this remains `null`.
/// Once entries length grows big enough, this field is allocated. There is
/// an IndexHeader followed by an array of Index(I) structs, where I is defined
/// by how many total indexes there are.
index_header: ?*IndexHeader = null,

/// Modifying the key is allowed only if it does not change the hash.
/// Modifying the value is allowed.
/// Entry pointers become invalid whenever this ArrayHashMap is modified,
/// unless `ensureTotalCapacity`/`ensureUnusedCapacity` was previously used.
pub const Entry = struct {
    key_ptr: **const Value,
    value_ptr: **Value,
};

/// A KV pair which has been copied out of the backing store
pub const KV = struct {
    key: *const Value,
    value: *Value,
};

/// The Data type used for the MultiArrayList backing this map
pub const Data = struct {
    hash: Hash,
    key: *const Value,
    value: *Value,
};

/// The MultiArrayList type backing this map
pub const DataList = @import("multi_array_list.zig").MultiArrayList(Data);

/// The stored hash type, either u32 or void.
pub const Hash = u32;

/// getOrPut variants return this structure, with pointers
/// to the backing store and a flag to indicate whether an
/// existing entry was found.
/// Modifying the key is allowed only if it does not change the hash.
/// Modifying the value is allowed.
/// Entry pointers become invalid whenever this ArrayHashMap is modified,
/// unless `ensureTotalCapacity`/`ensureUnusedCapacity` was previously used.
pub const GetOrPutResult = struct {
    key_ptr: **const Value,
    value_ptr: **Value,
    found_existing: bool,
    index: u32,
};

const Map = @This();

const linear_scan_max = 8;

const RemovalType = enum {
    swap,
    ordered,
};

/// Frees the backing allocation and leaves the map in an undefined state.
/// Note that this does not free keys or values.  You must take care of that
/// before calling this function, if it is needed.
pub fn deinit(self: *Map, allocator: Allocator) void {
    self.entries.deinit(allocator);
    if (self.index_header) |header| {
        header.free(allocator);
    }
    self.* = undefined;
}

/// Returns the number of KV pairs stored in this map.
pub fn count(self: Map) u32 {
    return self.entries.len;
}

/// Returns the backing array of keys in this map.
/// Modifying the map may invalidate this array.
pub fn keys(self: Map) []*const Value {
    return self.entries.items(.key);
}
/// Returns the backing array of values in this map.
/// Modifying the map may invalidate this array.
pub fn values(self: Map) []*Value {
    return self.entries.items(.value);
}

/// Returns an iterator over the pairs in this map.
/// Modifying the map may invalidate this iterator.
pub fn iterator(self: Map) Iterator {
    const slice = self.entries.slice();
    return .{
        .keys = slice.items(.key).ptr,
        .values = slice.items(.value).ptr,
        .len = @intCast(u32, slice.len),
    };
}
pub const Iterator = struct {
    keys: [*]*const Value,
    values: [*]*Value,
    len: u32,
    index: u32 = 0,

    pub fn next(it: *Iterator) ?Entry {
        if (it.index >= it.len) return null;
        const result = Entry{
            .key_ptr = &it.keys[it.index],
            .value_ptr = &it.values[it.index],
        };
        it.index += 1;
        return result;
    }

    /// Reset the iterator to the initial index
    pub fn reset(it: *Iterator) void {
        it.index = 0;
    }
};

/// If key exists this function cannot fail.
/// If there is an existing item with `key`, then the result
/// `Entry` pointer points to it, and found_existing is true.
/// Otherwise, puts a new item with undefined value, and
/// the `Entry` pointer points to it. Caller should then initialize
/// the value (but not the key).
pub fn getOrPut(self: *Map, allocator: Allocator, key: *const Value) !GetOrPutResult {
    const gop = try self.getOrPutContextAdapted(allocator, key);
    if (!gop.found_existing) {
        gop.key_ptr.* = key;
    }
    return gop;
}
fn getOrPutContextAdapted(self: *Map, allocator: Allocator, key: *const Value) !GetOrPutResult {
    self.ensureTotalCapacity(allocator, self.entries.len + 1) catch |err| {
        // "If key exists this function cannot fail."
        const index = self.getIndex(key) orelse return err;
        const slice = self.entries.slice();
        return GetOrPutResult{
            .key_ptr = &slice.items(.key)[index],
            .value_ptr = &slice.items(.value)[index],
            .found_existing = true,
            .index = index,
        };
    };
    return self.getOrPutAssumeCapacityAdapted(key);
}

/// If there is an existing item with `key`, then the result
/// `Entry` pointer points to it, and found_existing is true.
/// Otherwise, puts a new item with undefined value, and
/// the `Entry` pointer points to it. Caller should then initialize
/// the value (but not the key).
/// If a new entry needs to be stored, this function asserts there
/// is enough capacity to store it.
pub fn getOrPutAssumeCapacity(self: *Map, key: *const Value) GetOrPutResult {
    const gop = self.getOrPutAssumeCapacityAdapted(key);
    if (!gop.found_existing) {
        gop.key_ptr.* = key;
    }
    return gop;
}
/// If there is an existing item with `key`, then the result
/// `Entry` pointers point to it, and found_existing is true.
/// Otherwise, puts a new item with undefined key and value, and
/// the `Entry` pointers point to it. Caller must then initialize
/// both the key and the value.
/// If a new entry needs to be stored, this function asserts there
/// is enough capacity to store it.
fn getOrPutAssumeCapacityAdapted(self: *Map, key: *const Value) GetOrPutResult {
    const header = self.index_header orelse {
        // Linear scan.
        const h = key.hash();
        const slice = self.entries.slice();
        const hashes_array = slice.items(.hash);
        const keys_array = slice.items(.key);
        for (keys_array, 0..) |*item_key, i| {
            if (hashes_array[i] == h and key.eql(item_key.*)) {
                return GetOrPutResult{
                    .key_ptr = item_key,
                    .value_ptr = &slice.items(.value)[i],
                    .found_existing = true,
                    .index = @intCast(u32, i),
                };
            }
        }

        const index = self.entries.addOneAssumeCapacity();
        // unsafe indexing because the length changed
        hashes_array.ptr[index] = h;

        return GetOrPutResult{
            .key_ptr = &keys_array.ptr[index],
            .value_ptr = &slice.items(.value).ptr[index],
            .found_existing = false,
            .index = index,
        };
    };

    switch (header.capacityIndexType()) {
        .u8 => return self.getOrPutInternal(key, header, u8),
        .u16 => return self.getOrPutInternal(key, header, u16),
        .u32 => return self.getOrPutInternal(key, header, u32),
    }
}

/// Increases capacity, guaranteeing that insertions up until the
/// `expected_count` will not cause an allocation, and therefore cannot fail.
pub fn ensureTotalCapacity(self: *Map, allocator: Allocator, new_capacity: u32) !void {
    if (new_capacity <= linear_scan_max) {
        try self.entries.ensureTotalCapacity(allocator, new_capacity);
        return;
    }

    if (self.index_header) |header| {
        if (new_capacity <= header.capacity()) {
            try self.entries.ensureTotalCapacity(allocator, new_capacity);
            return;
        }
    }

    const new_bit_index = try IndexHeader.findBitIndex(new_capacity);
    const new_header = try IndexHeader.alloc(allocator, new_bit_index);
    try self.entries.ensureTotalCapacity(allocator, new_capacity);

    if (self.index_header) |old_header| old_header.free(allocator);
    self.insertAllEntriesIntoNewHeader(new_header);
    self.index_header = new_header;
}

/// Increases capacity, guaranteeing that insertions up until
/// `additional_count` **more** items will not cause an allocation, and
/// therefore cannot fail.
pub fn ensureUnusedCapacity(
    self: *Map,
    allocator: Allocator,
    additional_capacity: u32,
) !void {
    return self.ensureTotalCapacity(allocator, self.count() + additional_capacity);
}

/// Clobbers any existing data. To detect if a put would clobber
/// existing data, see `getOrPut`.
pub fn put(self: *Map, allocator: Allocator, key: *const Value, value: *Value) !void {
    const result = try self.getOrPut(allocator, key);
    result.value_ptr.* = value;
}

/// Inserts a key-value pair into the hash map, asserting that no previous
/// entry with the same key is already present
pub fn putNoClobber(self: *Map, allocator: Allocator, key: *const Value, value: *Value) !void {
    const result = try self.getOrPut(allocator, key);
    assert(!result.found_existing);
    result.value_ptr.* = value;
}

/// Asserts there is enough capacity to store the new key-value pair.
/// Clobbers any existing data. To detect if a put would clobber
/// existing data, see `getOrPutAssumeCapacity`.
pub fn putAssumeCapacity(self: *Map, key: *const Value, value: *Value) void {
    const result = self.getOrPutAssumeCapacity(key);
    result.value_ptr.* = value;
}

/// Asserts there is enough capacity to store the new key-value pair.
/// Asserts that it does not clobber any existing data.
/// To detect if a put would clobber existing data, see `getOrPutAssumeCapacity`.
pub fn putAssumeCapacityNoClobber(self: *Map, key: *const Value, value: *Value) void {
    const result = self.getOrPutAssumeCapacity(key);
    assert(!result.found_existing);
    result.value_ptr.* = value;
}

/// Finds the index in the `entries` array where a key is stored
pub fn getIndex(self: Map, key: *const Value) ?u32 {
    const header = self.index_header orelse {
        // Linear scan.
        const h = key.hash();
        const slice = self.entries.slice();
        const hashes_array = slice.items(.hash);
        const keys_array = slice.items(.key);
        for (keys_array, 0..) |*item_key, i| {
            if (hashes_array[i] == h and key.eql(item_key.*)) {
                return @intCast(u32, i);
            }
        }
        return null;
    };
    switch (header.capacityIndexType()) {
        .u8 => return self.getIndexWithHeaderGeneric(key, header, u8),
        .u16 => return self.getIndexWithHeaderGeneric(key, header, u16),
        .u32 => return self.getIndexWithHeaderGeneric(key, header, u32),
    }
}
fn getIndexWithHeaderGeneric(self: Map, key: anytype, header: *IndexHeader, comptime I: type) ?u32 {
    const indexes = header.indexes(I);
    const slot = self.getSlotByKey(key, header, I, indexes) orelse return null;
    return indexes[slot].entry_index;
}

/// Find the value associated with a key
pub fn get(self: Map, key: *const Value) ?*Value {
    const index = self.getIndex(key) orelse return null;
    return self.values()[index];
}

/// Check whether a key is stored in the map
pub fn contains(self: Map, key: *const Value) bool {
    return self.getIndex(key) != null;
}

/// Create a copy of the hash map which can be modified separately.
/// The copy uses the same context and allocator as this instance.
pub fn clone(self: Map, allocator: Allocator) !Map {
    var other: Map = .{};
    other.entries = try self.entries.clone(allocator);
    errdefer other.entries.deinit(allocator);

    if (self.index_header) |header| {
        const new_header = try IndexHeader.alloc(allocator, header.bit_index);
        other.insertAllEntriesIntoNewHeader(new_header);
        other.index_header = new_header;
    }
    return other;
}

// // ------------------ No pub fns below this point ------------------

/// Must `ensureTotalCapacity`/`ensureUnusedCapacity` before calling this.
fn getOrPutInternal(self: *Map, key: *const Value, header: *IndexHeader, comptime I: type) GetOrPutResult {
    const slice = self.entries.slice();
    const hashes_array = slice.items(.hash);
    const keys_array = slice.items(.key);
    const values_array = slice.items(.value);
    const indexes = header.indexes(I);

    const h = key.hash();
    const start_index = h;
    const end_index = start_index +% indexes.len;

    var index = start_index;
    var distance_from_start_index: I = 0;
    while (index != end_index) : ({
        index +%= 1;
        distance_from_start_index += 1;
    }) {
        var slot = header.constrainIndex(index);
        var slot_data = indexes[slot];

        // If the slot is empty, there can be no more items in this run.
        // We didn't find a matching item, so this must be new.
        // Put it in the empty slot.
        if (slot_data.isEmpty()) {
            const new_index = self.entries.addOneAssumeCapacity();
            indexes[slot] = .{
                .distance_from_start_index = distance_from_start_index,
                .entry_index = @intCast(I, new_index),
            };

            // update the hash if applicable
            hashes_array.ptr[new_index] = h;

            return .{
                .found_existing = false,
                .key_ptr = &keys_array.ptr[new_index],
                .value_ptr = &values_array.ptr[new_index],
                .index = new_index,
            };
        }

        // This pointer survives the following append because we call
        // entries.ensureTotalCapacity before getOrPutInternal.
        const i = slot_data.entry_index;
        const hash_match = h == hashes_array[i];
        if (hash_match and key.eql(keys_array[i])) {
            return .{
                .found_existing = true,
                .key_ptr = &keys_array[slot_data.entry_index],
                .value_ptr = &values_array[slot_data.entry_index],
                .index = slot_data.entry_index,
            };
        }

        // If the entry is closer to its target than our current distance,
        // the entry we are looking for does not exist.  It would be in
        // this slot instead if it was here.  So stop looking, and switch
        // to insert mode.
        if (slot_data.distance_from_start_index < distance_from_start_index) {
            // In this case, we did not find the item. We will put a new entry.
            // However, we will use this index for the new entry, and move
            // the previous index down the line, to keep the max distance_from_start_index
            // as small as possible.
            const new_index = self.entries.addOneAssumeCapacity();
            hashes_array.ptr[new_index] = h;
            indexes[slot] = .{
                .entry_index = @intCast(I, new_index),
                .distance_from_start_index = distance_from_start_index,
            };
            distance_from_start_index = slot_data.distance_from_start_index;
            var displaced_index = slot_data.entry_index;

            // Find somewhere to put the index we replaced by shifting
            // following indexes backwards.
            index +%= 1;
            distance_from_start_index += 1;
            while (index != end_index) : ({
                index +%= 1;
                distance_from_start_index += 1;
            }) {
                slot = header.constrainIndex(index);
                slot_data = indexes[slot];
                if (slot_data.isEmpty()) {
                    indexes[slot] = .{
                        .entry_index = displaced_index,
                        .distance_from_start_index = distance_from_start_index,
                    };
                    return .{
                        .found_existing = false,
                        .key_ptr = &keys_array.ptr[new_index],
                        .value_ptr = &values_array.ptr[new_index],
                        .index = new_index,
                    };
                }

                if (slot_data.distance_from_start_index < distance_from_start_index) {
                    indexes[slot] = .{
                        .entry_index = displaced_index,
                        .distance_from_start_index = distance_from_start_index,
                    };
                    displaced_index = slot_data.entry_index;
                    distance_from_start_index = slot_data.distance_from_start_index;
                }
            }
            unreachable;
        }
    }
    unreachable;
}

fn getSlotByKey(self: Map, key: anytype, header: *IndexHeader, comptime I: type, indexes: []Index(I)) ?u32 {
    const slice = self.entries.slice();
    const hashes_array = slice.items(.hash);
    const keys_array = slice.items(.key);
    const h = key.hash();

    const start_index = h;
    const end_index = start_index +% indexes.len;

    var index = start_index;
    var distance_from_start_index: I = 0;
    while (index != end_index) : ({
        index +%= 1;
        distance_from_start_index += 1;
    }) {
        const slot = header.constrainIndex(index);
        const slot_data = indexes[slot];
        if (slot_data.isEmpty() or slot_data.distance_from_start_index < distance_from_start_index)
            return null;

        const i = slot_data.entry_index;
        const hash_match = h == hashes_array[i];
        if (hash_match and key.eql(keys_array[i]))
            return slot;
    }
    unreachable;
}

fn insertAllEntriesIntoNewHeader(self: *Map, header: *IndexHeader) void {
    switch (header.capacityIndexType()) {
        .u8 => return self.insertAllEntriesIntoNewHeaderGeneric(header, u8),
        .u16 => return self.insertAllEntriesIntoNewHeaderGeneric(header, u16),
        .u32 => return self.insertAllEntriesIntoNewHeaderGeneric(header, u32),
    }
}
fn insertAllEntriesIntoNewHeaderGeneric(self: *Map, header: *IndexHeader, comptime I: type) void {
    const slice = self.entries.slice();
    const items = slice.items(.hash);
    const indexes = header.indexes(I);

    entry_loop: for (items, 0..) |key, i| {
        const start_index = key;
        const end_index = start_index +% indexes.len;
        var index = start_index;
        var entry_index = @intCast(I, i);
        var distance_from_start_index: I = 0;
        while (index != end_index) : ({
            index +%= 1;
            distance_from_start_index += 1;
        }) {
            const slot = header.constrainIndex(index);
            const next_index = indexes[slot];
            if (next_index.isEmpty()) {
                indexes[slot] = .{
                    .distance_from_start_index = distance_from_start_index,
                    .entry_index = entry_index,
                };
                continue :entry_loop;
            }
            if (next_index.distance_from_start_index < distance_from_start_index) {
                indexes[slot] = .{
                    .distance_from_start_index = distance_from_start_index,
                    .entry_index = entry_index,
                };
                distance_from_start_index = next_index.distance_from_start_index;
                entry_index = next_index.entry_index;
            }
        }
        unreachable;
    }
}

const CapacityIndexType = enum { u8, u16, u32 };

fn capacityIndexType(bit_index: u8) CapacityIndexType {
    if (bit_index <= 8)
        return .u8;
    if (bit_index <= 16)
        return .u16;
    assert(bit_index <= 32);
    return .u32;
}

fn capacityIndexSize(bit_index: u8) u32 {
    switch (capacityIndexType(bit_index)) {
        .u8 => return @sizeOf(Index(u8)),
        .u16 => return @sizeOf(Index(u16)),
        .u32 => return @sizeOf(Index(u32)),
    }
}

/// A single entry in the lookup acceleration structure.  These structs
/// are found in an array after the IndexHeader.  Hashes index into this
/// array, and linear probing is used for collisions.
fn Index(comptime I: type) type {
    return extern struct {
        const Self = @This();

        /// The index of this entry in the backing store.  If the index is
        /// empty, this is empty_sentinel.
        entry_index: I,

        /// The distance between this slot and its ideal placement.  This is
        /// used to keep maximum scan length small.  This value is undefined
        /// if the index is empty.
        distance_from_start_index: I,

        /// The special entry_index value marking an empty slot.
        const empty_sentinel = ~@as(I, 0);

        /// A constant empty index
        const empty = Self{
            .entry_index = empty_sentinel,
            .distance_from_start_index = undefined,
        };

        /// Checks if a slot is empty
        fn isEmpty(idx: Self) bool {
            return idx.entry_index == empty_sentinel;
        }

        /// Sets a slot to empty
        fn setEmpty(idx: *Self) void {
            idx.entry_index = empty_sentinel;
            idx.distance_from_start_index = undefined;
        }
    };
}

// /// the byte size of the index must fit in a u32.  This is a power of two
// /// length * the size of an Index(u32).  The index is 8 bytes (3 bits repr)
// /// and max_usize + 1 is not representable, so we need to subtract out 4 bits.
const max_representable_index_len = @bitSizeOf(u32) - 4;
const max_bit_index = @min(32, max_representable_index_len);
const min_bit_index = 5;
const max_capacity = (1 << max_bit_index) - 1;
const index_capacities = blk: {
    var caps: [max_bit_index + 1]u32 = undefined;
    for (caps[0..max_bit_index], 0..) |*item, i| {
        item.* = (1 << i) * 3 / 5;
    }
    caps[max_bit_index] = max_capacity;
    break :blk caps;
};

/// This struct is trailed by two arrays of length indexes_len
/// of integers, whose integer size is determined by indexes_len.
/// These arrays are indexed by constrainIndex(hash).  The
/// entryIndexes array contains the index in the dense backing store
/// where the entry's data can be found.  Entries which are not in
/// use have their index value set to emptySentinel(I).
/// The entryDistances array stores the distance between an entry
/// and its ideal hash bucket.  This is used when adding elements
/// to balance the maximum scan length.
const IndexHeader = struct {
    /// This field tracks the total number of items in the arrays following
    /// this header.  It is the bit index of the power of two number of indices.
    /// This value is between min_bit_index and max_bit_index, inclusive.
    bit_index: u8 align(@alignOf(u32)),

    /// Map from an incrementing index to an index slot in the attached arrays.
    fn constrainIndex(header: IndexHeader, i: u32) u32 {
        // This is an optimization for modulo of power of two integers;
        // it requires `indexes_len` to always be a power of two.
        return @intCast(u32, i & header.mask());
    }

    /// Returns the attached array of indexes.  I must match the type
    /// returned by capacityIndexType.
    fn indexes(header: *IndexHeader, comptime I: type) []Index(I) {
        const start_ptr = @ptrCast([*]Index(I), @ptrCast([*]u8, header) + @sizeOf(IndexHeader));
        return start_ptr[0..header.length()];
    }

    /// Returns the type used for the index arrays.
    fn capacityIndexType(header: IndexHeader) CapacityIndexType {
        return Map.capacityIndexType(header.bit_index);
    }

    fn capacity(self: IndexHeader) u32 {
        return index_capacities[self.bit_index];
    }
    fn length(self: IndexHeader) u32 {
        return @as(u32, 1) << @intCast(math.Log2Int(u32), self.bit_index);
    }
    fn mask(self: IndexHeader) u32 {
        return @intCast(u32, self.length() - 1);
    }

    fn findBitIndex(desired_capacity: u32) !u8 {
        if (desired_capacity > max_capacity) return error.OutOfMemory;
        var new_bit_index = @intCast(u8, std.math.log2_int_ceil(u32, desired_capacity));
        if (desired_capacity > index_capacities[new_bit_index]) new_bit_index += 1;
        if (new_bit_index < min_bit_index) new_bit_index = min_bit_index;
        assert(desired_capacity <= index_capacities[new_bit_index]);
        return new_bit_index;
    }

    /// Allocates an index header, and fills the entryIndexes array with empty.
    /// The distance array contents are undefined.
    fn alloc(allocator: Allocator, new_bit_index: u8) !*IndexHeader {
        const len = @as(usize, 1) << @intCast(math.Log2Int(usize), new_bit_index);
        const index_size = Map.capacityIndexSize(new_bit_index);
        const nbytes = @sizeOf(IndexHeader) + index_size * len;
        const bytes = try allocator.alignedAlloc(u8, @alignOf(IndexHeader), nbytes);
        @memset(bytes[@sizeOf(IndexHeader)..], 0xff);
        const result = @ptrCast(*IndexHeader, bytes.ptr);
        result.* = .{
            .bit_index = new_bit_index,
        };
        return result;
    }

    /// Releases the memory for a header and its associated arrays.
    fn free(header: *IndexHeader, allocator: Allocator) void {
        const index_size = Map.capacityIndexSize(header.bit_index);
        const ptr = @ptrCast([*]align(@alignOf(IndexHeader)) u8, header);
        const slice = ptr[0 .. @sizeOf(IndexHeader) + header.length() * index_size];
        allocator.free(slice);
    }
};
