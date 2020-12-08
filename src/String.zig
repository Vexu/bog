const std = @import("std");
const String = @This();
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Value = bog.Value;
const Type = bog.Type;

const default_dump_depth = 4;

/// Memory used by the string contents.
data: []const u8,
/// If 0 memory is a constant reference
capacity: usize = 0,

pub const Builder = struct {
    inner: std.ArrayList(u8),

    pub fn finish(b: Builder) String {
        return .{
            .data = b.inner.items,
            .capacity = b.inner.capacity,
        };
    }

    pub fn append(b: *Builder, data: []const u8) !void {
        try b.inner.appendSlice(data);
    }

    pub const Writer = std.io.Writer(*Builder, error{OutOfMemory}, appendWrite);

    /// Initializes a Writer which will append to the builder.
    pub fn writer(self: *Builder) Writer {
        return .{ .context = self };
    }

    /// Same as `append` except it returns the number of bytes written, which is always the same
    /// as `m.len`. The purpose of this function existing is to match `std.io.Writer` API.
    fn appendWrite(self: *Builder, data: []const u8) !usize {
        try self.append(data);
        return data.len;
    }
};

pub fn builder(allocator: *Allocator) Builder {
    return Builder{
        .inner = std.ArrayList(u8).init(allocator),
    };
}

pub fn toBuilder(string: String, allocator: *Allocator) !Builder {
    if (string.capacity == 0) {
        var b = Builder{
            .inner = std.ArrayList(u8).init(allocator),
        };
        try b.append(string.data);
        return b;
    } else {
        return Builder{
            .inner = std.ArrayList(u8).fromOwnedSlice(allocator, @bitCast([]u8, string.data)),
        };
    }
}

pub fn deinit(str: *String, allocator: *Allocator) void {
    if (str.capacity != 0) {
        allocator.free(str.data);
    }
    str.* = undefined;
}

pub fn eql(a: String, b: String) bool {
    return mem.eql(u8, a.data, b.data);
}

pub fn print(str: String, writer: anytype) !void {
    try writer.writeByte('"');
    try std.fmt.formatZigEscapes(str.data, .{}, writer);
    try writer.writeByte('"');
}

pub fn get(str: *const String, vm: *Vm, index: *const Value, res: *?*Value) Vm.Error!void {
    switch (index.*) {
        .int => return vm.reportErr("TODO get str"),
        .range => return vm.reportErr("TODO get with ranges"),
        .str => |*s| {
            if (res.* == null) {
                res.* = try vm.gc.alloc();
            }

            if (mem.eql(u8, s.data, "len")) {
                res.*.?.* = .{ .int = @intCast(i64, str.data.len) };
            } else if (mem.eql(u8, s.data, "append")) {
                res.* = try Value.zigToBog(vm, struct {
                    fn append(_vm: *Vm, val: String) !void {
                        if (_vm.last_get.* != .str)
                            return _vm.reportErr("expected string");

                        try _vm.last_get.str.append(_vm.gc.gpa, val.data);
                    }
                }.append);
            } else {
                return vm.reportErr("no such property");
            }
        },
        else => return vm.reportErr("invalid index type"),
    }
}

pub fn set(str: *String, vm: *Vm, index: *const Value, new_val: *const Value) Vm.Error!void {
    return vm.reportErr("TODO set string");
}

pub fn as(str: *String, vm: *Vm, type_id: Type) Vm.Error!*Value {
    if (type_id == .none) {
        return &Value.None;
    } else if (type_id == .bool) {
        if (mem.eql(u8, str.data, "true"))
            return &Value.True
        else if (mem.eql(u8, str.data, "false"))
            return &Value.False
        else
            return vm.reportErr("cannot cast string to bool");
    }

    const new_val = try vm.gc.alloc();
    new_val.* = switch (type_id) {
        .int => .{
            .int = @import("util.zig").parseInt(str.data) catch
                return vm.reportErr("invalid cast to int"),
        },
        .num => .{
            .num = @import("util.zig").parseNum(str.data) catch
                return vm.reportErr("invalid cast to num"),
        },
        .bool => unreachable,
        .str => unreachable, // already string
        .tuple,
        .map,
        .list,
        => return vm.reportErr("TODO more casts from string"),
        else => unreachable,
    };
    return new_val;
}

pub fn from(val: *Value, vm: *Vm) Vm.Error!*Value {
    const str = try vm.gc.alloc();

    if (val.* == .none) {
        str.* = Value.string("none");
    } else if (val.* == .bool) {
        str.* = if (val.bool)
            Value.string("true")
        else
            Value.string("false");
    } else {
        var b = builder(vm.gc.gpa);
        try val.dump(b.writer(), default_dump_depth);
        str.* = .{ .str = b.finish() };
    }
    return str;
}

pub fn in(str: *const String, val: *const Value) bool {
    if (val.* != .str) return false;
    return mem.indexOf(u8, str.data, val.str.data) != null;
}

pub fn append(str: *String, allocator: *Allocator, data: []const u8) !void {
    var b = try str.toBuilder(allocator);
    try b.append(data);
    str.* = b.finish();
}
