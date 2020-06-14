const std = @import("std");
const assert = std.debug.assert;
const math = std.math;
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("../bog.zig");
const Value = bog.Value;

//! An unmaintained HashMap, modified from `std.HashMap`.
//! Reduces size from 48 to 32 bytes.

entries: []Entry = &[_]Entry{},
size: usize = 0,
max_distance_from_start_index: usize = 0,

const Map = @This();

/// A *KV is a mutable pointer into this HashMap's internal storage.
/// Modifying the key is undefined behavior.
/// Modifying the value is harmless.
/// *KV pointers become invalid whenever this HashMap is modified,
/// and then any access to the *KV is undefined behavior.
pub const KV = struct {
    key: *const Value,
    value: *Value,
};

const Entry = struct {
    used: bool,
    distance_from_start_index: usize,
    kv: KV,
};

pub const GetOrPutResult = struct {
    kv: *KV,
    found_existing: bool,
};

pub const Iterator = struct {
    hm: *const Map,
    // how many items have we returned
    count: usize,
    // iterator through the entry array
    index: usize,

    pub fn next(it: *Iterator) ?*KV {
        if (it.count >= it.hm.size) return null;
        while (it.index < it.hm.entries.len) : (it.index += 1) {
            const entry = &it.hm.entries[it.index];
            if (entry.used) {
                it.index += 1;
                it.count += 1;
                return &entry.kv;
            }
        }
        unreachable; // no next item
    }

    // Reset the iterator to the initial index
    pub fn reset(it: *Iterator) void {
        it.count = 0;
        it.index = 0;
    }
};

pub fn deinit(hm: Map, allocator: *Allocator) void {
    allocator.free(hm.entries);
}

pub fn clear(hm: *Map) void {
    for (hm.entries) |*entry| {
        entry.used = false;
    }
    hm.size = 0;
    hm.max_distance_from_start_index = 0;
}

/// If key exists this function cannot fail.
/// If there is an existing item with `key`, then the result
/// kv pointer points to it, and found_existing is true.
/// Otherwise, puts a new item with undefined value, and
/// the kv pointer points to it. Caller should then initialize
/// the data.
pub fn getOrPut(self: *Map, allocator: *Allocator, key: *const Value) !GetOrPutResult {
    // TODO this implementation can be improved - we should only
    // have to hash once and find the entry once.
    if (self.get(key)) |kv| {
        return GetOrPutResult{
            .kv = kv,
            .found_existing = true,
        };
    }
    try self.autoCapacity(allocator);
    const put_result = self.internalPut(key);
    assert(put_result.old_kv == null);
    return GetOrPutResult{
        .kv = &put_result.new_entry.kv,
        .found_existing = false,
    };
}

pub fn getOrPutValue(self: *Map, key: *const Value, value: *Value) !*KV {
    const res = try self.getOrPut(key);
    if (!res.found_existing)
        res.kv.value = value;

    return res.kv;
}

fn optimizedCapacity(expected_count: usize) usize {
    // ensure that the hash map will be at most 60% full if
    // expected_count items are put into it
    var optimized_capacity = expected_count * 5 / 3;
    // an overflow here would mean the amount of memory required would not
    // be representable in the address space
    return math.ceilPowerOfTwo(usize, optimized_capacity) catch unreachable;
}

/// Increases capacity so that the hash map will be at most
/// 60% full when expected_count items are put into it
pub fn ensureCapacity(self: *Map, allocator: *Allocator, expected_count: usize) !void {
    if (expected_count == 0) return;
    const optimized_capacity = optimizedCapacity(expected_count);
    return self.ensureCapacityExact(allocator, optimized_capacity);
}

/// Sets the capacity to the new capacity if the new
/// capacity is greater than the current capacity.
/// New capacity must be a power of two.
fn ensureCapacityExact(self: *Map, allocator: *Allocator, new_capacity: usize) !void {
    // capacity must always be a power of two to allow for modulo
    // optimization in the constrainIndex fn
    assert(math.isPowerOfTwo(new_capacity));

    if (new_capacity <= self.entries.len) {
        return;
    }

    const old_entries = self.entries;
    try self.initCapacity(allocator, new_capacity);
    if (old_entries.len > 0) {
        // dump all of the old elements into the new table
        for (old_entries) |*old_entry| {
            if (old_entry.used) {
                self.internalPut(old_entry.kv.key).new_entry.kv.value = old_entry.kv.value;
            }
        }
        allocator.free(old_entries);
    }
}

/// Returns the kv pair that was already there.
pub fn put(self: *Map, allocator: *Allocator, key: *const Value, value: *Value) !?KV {
    try self.autoCapacity(allocator);
    return putAssumeCapacity(self, key, value);
}

/// Calls put() and asserts that no kv pair is clobbered.
pub fn putNoClobber(self: *Map, allocator: *Allocator, key: *const Value, value: *Value) !void {
    assert((try self.put(allocator, key, value)) == null);
}

pub fn putAssumeCapacity(self: *Map, key: *const Value, value: *Value) ?KV {
    assert(self.size < self.entries.len);

    const put_result = self.internalPut(key);
    put_result.new_entry.kv.value = value;
    return put_result.old_kv;
}

pub fn putAssumeCapacityNoClobber(self: *Map, key: *const Value, value: *Value) void {
    assert(self.putAssumeCapacity(key, value) == null);
}

pub fn get(hm: *const Map, key: *const Value) ?*KV {
    if (hm.entries.len == 0) {
        return null;
    }
    return hm.internalGet(key);
}

pub fn getValue(hm: *const Map, key: *const Value) ?*Value {
    return if (hm.get(key)) |kv| kv.value else null;
}

pub fn contains(hm: *const Map, key: *const Value) bool {
    return hm.get(key) != null;
}

/// Returns any kv pair that was removed.
pub fn remove(hm: *Map, key: *const Value) ?KV {
    if (hm.entries.len == 0) return null;
    const start_index = hm.keyToIndex(key);
    {
        var roll_over: usize = 0;
        while (roll_over <= hm.max_distance_from_start_index) : (roll_over += 1) {
            const index = hm.constrainIndex(start_index + roll_over);
            var entry = &hm.entries[index];

            if (!entry.used) return null;

            if (!entry.kv.key.eql(key)) continue;

            const removed_kv = entry.kv;
            while (roll_over < hm.entries.len) : (roll_over += 1) {
                const next_index = hm.constrainIndex(start_index + roll_over + 1);
                const next_entry = &hm.entries[next_index];
                if (!next_entry.used or next_entry.distance_from_start_index == 0) {
                    entry.used = false;
                    hm.size -= 1;
                    return removed_kv;
                }
                entry.* = next_entry.*;
                entry.distance_from_start_index -= 1;
                entry = next_entry;
            }
            unreachable; // shifting everything in the table
        }
    }
    return null;
}

/// Calls remove(), asserts that a kv pair is removed, and discards it.
pub fn removeAssertDiscard(hm: *Map, key: *const Value) void {
    assert(hm.remove(key) != null);
}

pub fn iterator(hm: *const Map) Iterator {
    return Iterator{
        .hm = hm,
        .count = 0,
        .index = 0,
    };
}

pub fn clone(self: Map, allocator: *Allocator) !Map {
    var other = Map{};
    try other.initCapacity(allocator, self.entries.len);
    var it = self.iterator();
    while (it.next()) |entry| {
        try other.putNoClobber(allocator, entry.key, entry.value);
    }
    return other;
}

fn autoCapacity(self: *Map, allocator: *Allocator) !void {
    if (self.entries.len == 0) {
        return self.ensureCapacityExact(allocator, 16);
    }
    // if we get too full (60%), double the capacity
    if (self.size * 5 >= self.entries.len * 3) {
        return self.ensureCapacityExact(allocator, self.entries.len * 2);
    }
}

fn initCapacity(hm: *Map, allocator: *Allocator, capacity: usize) !void {
    hm.entries = try allocator.alloc(Entry, capacity);
    hm.size = 0;
    hm.max_distance_from_start_index = 0;
    for (hm.entries) |*entry| {
        entry.used = false;
    }
}

const InternalPutResult = struct {
    new_entry: *Entry,
    old_kv: ?KV,
};

/// Returns a pointer to the new entry.
/// Asserts that there is enough space for the new item.
fn internalPut(self: *Map, orig_key: *const Value) InternalPutResult {
    var key = orig_key;
    var value: *Value = undefined;
    const start_index = self.keyToIndex(key);
    var roll_over: usize = 0;
    var distance_from_start_index: usize = 0;
    var got_result_entry = false;
    var result = InternalPutResult{
        .new_entry = undefined,
        .old_kv = null,
    };
    while (roll_over < self.entries.len) : ({
        roll_over += 1;
        distance_from_start_index += 1;
    }) {
        const index = self.constrainIndex(start_index + roll_over);
        const entry = &self.entries[index];

        if (entry.used and !entry.kv.key.eql(key)) {
            if (entry.distance_from_start_index < distance_from_start_index) {
                // robin hood to the rescue
                const tmp = entry.*;
                self.max_distance_from_start_index = math.max(self.max_distance_from_start_index, distance_from_start_index);
                if (!got_result_entry) {
                    got_result_entry = true;
                    result.new_entry = entry;
                }
                entry.* = Entry{
                    .used = true,
                    .distance_from_start_index = distance_from_start_index,
                    .kv = KV{
                        .key = key,
                        .value = value,
                    },
                };
                key = tmp.kv.key;
                value = tmp.kv.value;
                distance_from_start_index = tmp.distance_from_start_index;
            }
            continue;
        }

        if (entry.used) {
            result.old_kv = entry.kv;
        } else {
            // adding an entry. otherwise overwriting old value with
            // same key
            self.size += 1;
        }

        self.max_distance_from_start_index = math.max(distance_from_start_index, self.max_distance_from_start_index);
        if (!got_result_entry) {
            result.new_entry = entry;
        }
        entry.* = Entry{
            .used = true,
            .distance_from_start_index = distance_from_start_index,
            .kv = KV{
                .key = key,
                .value = value,
            },
        };
        return result;
    }
    unreachable; // put into a full map
}

fn internalGet(hm: Map, key: *const Value) ?*KV {
    const start_index = hm.keyToIndex(key);
    {
        var roll_over: usize = 0;
        while (roll_over <= hm.max_distance_from_start_index) : (roll_over += 1) {
            const index = hm.constrainIndex(start_index + roll_over);
            const entry = &hm.entries[index];

            if (!entry.used) return null;
            if (entry.kv.key.eql(key)) return &entry.kv;
        }
    }
    return null;
}

fn keyToIndex(hm: Map, key: *const Value) usize {
    return hm.constrainIndex(@as(usize, key.hash()));
}

fn constrainIndex(hm: Map, i: usize) usize {
    // this is an optimization for modulo of power of two integers;
    // it requires hm.entries.len to always be a power of two
    return i & (hm.entries.len - 1);
}
