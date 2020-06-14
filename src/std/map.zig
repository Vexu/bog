const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

/// Creates a list of the maps keys
pub fn keys(vm: *Vm, val: *const Value) !*Value {
    if (val.* != .map) return error.ExpectedMap;

    var ret = try vm.gc.alloc();
    ret.* = .{ .list = .{} };
    try ret.list.resize(vm.gc.gpa, val.map.size);
    const items = ret.list.items;
    var i: usize = 0;
    for (val.map.entries) |*e| {
        if (e.used) {
            items[i] = try vm.gc.dupe(e.kv.key);
            i += 1;
        }
    }

    return ret;
}

/// Creates a list of the maps values
pub fn values(vm: *Vm, val: *const Value) !*Value {
    if (val.* != .map) return error.ExpectedMap;

    var ret = try vm.gc.alloc();
    ret.* = .{ .list = .{} };
    try ret.list.resize(vm.gc.gpa, val.map.size);
    const items = ret.list.items;
    var i: usize = 0;
    for (val.map.entries) |*e| {
        if (e.used) {
            items[i] = try vm.gc.dupe(e.kv.value);
            i += 1;
        }
    }

    return ret;
}

/// Creates a list of kv pairs
pub fn entries(vm: *Vm, val: *const Value) !*Value {
    if (val.* != .map) return error.ExpectedMap;

    var ret = try vm.gc.alloc();
    ret.* = .{ .list = .{} };
    try ret.list.resize(vm.gc.gpa, val.map.size);
    const items = ret.list.items;
    var i: usize = 0;
    for (val.map.entries) |*e| {
        if (e.used) {
            var entry = try vm.gc.alloc();
            const val_str = Value{ .str = "value" };
            const key_str = Value{ .str = "key" };
            entry.* = .{ .map = .{} };
            try entry.map.ensureCapacity(vm.gc.gpa, 2);
            entry.map.putAssumeCapacityNoClobber(try vm.gc.dupe(&key_str), try vm.gc.dupe(e.kv.key));
            entry.map.putAssumeCapacityNoClobber(try vm.gc.dupe(&val_str), try vm.gc.dupe(e.kv.value));

            items[i] = entry;
            i += 1;
        }
    }

    return ret;
}

pub fn size(val: *const Value) !i64 {
    if (val.* != .map) return error.ExpectedMap;
    return @intCast(i64, val.map.size);
}
