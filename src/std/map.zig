const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

/// Creates a list of the maps keys
pub fn keys(vm: *Vm, map: *const Value.Map) !*Value {
    var ret = try vm.gc.alloc();
    ret.* = .{ .list = .{} };
    try ret.list.resize(vm.gc.gpa, map.count());
    const items = ret.list.items;

    var i: usize = 0;
    var iter = map.iterator();
    while (iter.next()) |e| : (i += 1) {
        items[i] = try vm.gc.dupe(e.key_ptr.*);
    }

    return ret;
}

/// Creates a list of the maps values
pub fn values(vm: *Vm, map: *const Value.Map) !*Value {
    var ret = try vm.gc.alloc();
    ret.* = .{ .list = .{} };
    try ret.list.resize(vm.gc.gpa, map.count());
    const items = ret.list.items;

    var i: usize = 0;
    var iter = map.iterator();
    while (iter.next()) |e| : (i += 1) {
        items[i] = try vm.gc.dupe(e.value_ptr.*);
    }

    return ret;
}

/// Creates a list of kv pairs
pub fn entries(vm: *Vm, map: *const Value.Map) !*Value {
    var ret = try vm.gc.alloc();
    ret.* = .{ .list = .{} };
    try ret.list.resize(vm.gc.gpa, map.count());
    const items = ret.list.items;

    var i: usize = 0;
    var iter = map.iterator();
    while (iter.next()) |e| : (i += 1) {
        var entry = try vm.gc.alloc();
        entry.* = .{ .map = .{} };
        try entry.map.ensureCapacity(vm.gc.gpa, 2);

        const val_str = Value.string("value");
        const key_str = Value.string("key");
        entry.map.putAssumeCapacityNoClobber(try vm.gc.dupe(&key_str), try vm.gc.dupe(e.key_ptr.*));
        entry.map.putAssumeCapacityNoClobber(try vm.gc.dupe(&val_str), try vm.gc.dupe(e.value_ptr.*));

        items[i] = entry;
    }

    return ret;
}

/// Returns the amount of key value pairs in the map.
pub fn size(map: *const Value.Map) !i64 {
    return @intCast(i64, map.count());
}
