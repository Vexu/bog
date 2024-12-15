const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Value = bog.Value;
const Type = bog.Type;

const List = @This();

inner: std.ArrayListUnmanaged(*Value) = .{},

pub fn deinit(l: *List, allocator: Allocator) void {
    l.inner.deinit(allocator);
    l.* = undefined;
}

pub fn eql(a: List, b: List) bool {
    if (a.inner.items.len != b.inner.items.len) return false;
    for (a.inner.items, 0..) |v, i| {
        if (!v.eql(b.inner.items[i])) return false;
    }
    return true;
}

pub fn get(list: *const List, ctx: Vm.Context, index: *const Value, res: *?*Value) Value.NativeError!void {
    switch (index.*) {
        .int => {
            var i = index.int;
            if (i < 0)
                i += @intCast(list.inner.items.len);
            if (i < 0 or i >= list.inner.items.len)
                return ctx.throw("index out of bounds");

            res.* = list.inner.items[@intCast(i)];
        },
        .range => |r| {
            if (r.start < 0 or r.end > list.inner.items.len)
                return ctx.throw("index out of bounds");

            res.* = try ctx.vm.gc.alloc(.list);
            res.*.?.* = .{ .list = .{} };
            const res_list = &res.*.?.*.list;
            try res_list.inner.ensureUnusedCapacity(ctx.vm.gc.gpa, @intCast(r.count()));

            var it = r.iterator();
            while (it.next()) |some| {
                res_list.inner.appendAssumeCapacity(list.inner.items[@intCast(some)]);
            }
        },
        .str => |s| {
            if (res.* == null) {
                res.* = try ctx.vm.gc.alloc(.int);
            }

            if (mem.eql(u8, s.data, "len")) {
                res.*.?.* = .{ .int = @intCast(list.inner.items.len) };
            } else inline for (@typeInfo(methods).@"struct".decls) |method| {
                if (mem.eql(u8, s.data, method.name)) {
                    res.* = try Value.zigFnToBog(ctx.vm, @field(methods, method.name));
                    return;
                }
            } else {
                return ctx.throw("no such property");
            }
        },
        else => return ctx.throw("invalid index type"),
    }
}

pub const methods = struct {
    fn append(list: Value.This(*List), ctx: Vm.Context, vals: Value.Variadic(*Value)) !void {
        try list.t.inner.appendSlice(ctx.vm.gc.gpa, vals.t);
    }
};

pub fn set(list: *List, ctx: Vm.Context, index: *const Value, new_val: *Value) Value.NativeError!void {
    switch (index.*) {
        .int => {
            var i = index.int;
            if (i < 0)
                i += @intCast(list.inner.items.len);
            if (i < 0 or i >= list.inner.items.len)
                return ctx.throw("index out of bounds");

            list.inner.items[@intCast(i)] = new_val;
        },
        .range => |r| {
            if (r.start < 0 or r.end > list.inner.items.len)
                return ctx.throw("index out of bounds");

            var it = r.iterator();
            while (it.next()) |some| {
                list.inner.items[@intCast(some)] = new_val;
            }
        },
        else => return ctx.throw("invalid index type"),
    }
}

pub fn as(list: *List, ctx: Vm.Context, type_id: Type) Vm.Error!*Value {
    _ = list;
    _ = type_id;
    return ctx.frame.fatal(ctx.vm, "TODO cast to list");
}

pub fn from(val: *Value, ctx: Vm.Context) Vm.Error!*Value {
    _ = val;
    return ctx.frame.fatal(ctx.vm, "TODO cast from list");
}

pub fn in(list: *const List, val: *const Value) bool {
    for (list.inner.items) |v| {
        if (v.eql(val)) return true;
    }
    return false;
}
