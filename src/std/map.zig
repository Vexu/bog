const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

/// Creates a list of the maps keys
pub fn keys(ctx: Vm.Context, map: *const Value.Map) !*Value {
    const gc = &ctx.vm.gc;
    var ret = try gc.alloc(.list);
    ret.* = .{ .list = .{} };
    try ret.list.resize(gc.gpa, map.count());
    const items = ret.list.items;

    var i: usize = 0;
    var iter = map.iterator();
    while (iter.next()) |e| : (i += 1) {
        items[i] = try gc.dupe(e.key_ptr.*);
    }

    return ret;
}

/// Creates a list of the maps values
pub fn values(ctx: Vm.Context, map: *const Value.Map) !*Value {
    const gc = &ctx.vm.gc;
    var ret = try gc.alloc(.list);
    ret.* = .{ .list = .{} };
    try ret.list.resize(gc.gpa, map.count());
    const items = ret.list.items;

    var i: usize = 0;
    var iter = map.iterator();
    while (iter.next()) |e| : (i += 1) {
        items[i] = try gc.dupe(e.value_ptr.*);
    }

    return ret;
}

/// Creates a list of kv pairs
pub fn entries(ctx: Vm.Context, map: *const Value.Map) !*Value {
    const gc = &ctx.vm.gc;
    var ret = try ctx.vm.gc.alloc(.list);
    ret.* = .{ .list = .{} };
    try ret.list.resize(gc.gpa, map.count());
    const items = ret.list.items;

    var i: usize = 0;
    var iter = map.iterator();
    while (iter.next()) |e| : (i += 1) {
        var entry = try gc.alloc(.map);
        entry.* = .{ .map = .{} };
        try entry.map.ensureTotalCapacity(gc.gpa, 2);

        const val_str = Value.string("value");
        const key_str = Value.string("key");
        entry.map.putAssumeCapacityNoClobber(try gc.dupe(&key_str), try gc.dupe(e.key_ptr.*));
        entry.map.putAssumeCapacityNoClobber(try gc.dupe(&val_str), try gc.dupe(e.value_ptr.*));

        items[i] = entry;
    }

    return ret;
}

/// Returns the amount of key value pairs in the map.
pub fn size(map: *const Value.Map) !i64 {
    return @intCast(i64, map.count());
}
