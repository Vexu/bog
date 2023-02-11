const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Value = bog.Value;
const Type = bog.Type;

const String = @This();

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

    pub fn cancel(b: *Builder) void {
        b.inner.deinit();
        b.* = undefined;
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

pub fn builder(allocator: Allocator) Builder {
    return Builder{
        .inner = std.ArrayList(u8).init(allocator),
    };
}

pub fn init(allocator: Allocator, comptime fmt: []const u8, args: anytype) !String {
    var b = builder(allocator);
    errdefer b.cancel();

    try b.writer().print(fmt, args);
    return b.finish();
}

pub fn deinit(str: *String, allocator: Allocator) void {
    if (str.capacity != 0) {
        allocator.free(str.data);
    }
    str.* = undefined;
}

pub fn eql(a: String, b: String) bool {
    return mem.eql(u8, a.data, b.data);
}

pub fn dump(str: String, writer: anytype) !void {
    try writer.print("\"{}\"", .{std.zig.fmtEscapes(str.data)});
}

pub fn get(str: *const String, ctx: Vm.Context, index: *const Value, res: *?*Value) Value.NativeError!void {
    switch (index.*) {
        .int => return ctx.frame.fatal(ctx.vm, "TODO str get int"),
        .range => return ctx.frame.fatal(ctx.vm, "TODO str get with ranges"),
        .str => |*s| {
            if (res.* == null) {
                res.* = try ctx.vm.gc.alloc();
            }

            if (mem.eql(u8, s.data, "len")) {
                res.*.?.* = .{ .int = @intCast(i64, str.data.len) };
            } else inline for (@typeInfo(methods).Struct.decls) |method| {
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
    pub fn append(str: Value.This(*String), ctx: Vm.Context, strs: Value.Variadic([]const u8)) !void {
        var b = builder(ctx.vm.gc.gpa);
        errdefer b.cancel();

        var len = str.t.data.len;
        for (strs.t) |new| len += new.len;
        try b.inner.ensureUnusedCapacity(len);

        b.inner.appendSliceAssumeCapacity(str.t.data);
        for (strs.t) |new| b.inner.appendSliceAssumeCapacity(new);

        str.t.deinit(ctx.vm.gc.gpa);
        str.t.* = b.finish();
    }

    pub fn format(str: Value.This([]const u8), ctx: Vm.Context, args: Value.Variadic(*Value)) !*Value {
        var b = builder(ctx.vm.gc.gpa);
        errdefer b.cancel();

        try b.inner.ensureTotalCapacity(str.t.len);

        const w = b.writer();
        var state: enum {
            start,
            brace,
            format,
        } = .start;
        var arg_i: usize = 0;
        var format_start: usize = 0;
        var options = std.fmt.FormatOptions{};

        var i: usize = 0;
        while (i < str.t.len) : (i += 1) {
            const c = str.t[i];
            switch (state) {
                .start => if (c == '{') {
                    state = .brace;
                } else {
                    try w.writeByte(c);
                },
                .brace => if (c == '{') {
                    try w.writeByte(c);
                } else {
                    if (arg_i >= args.t.len) {
                        return ctx.throw("not enough arguments to format string");
                    }
                    format_start = i;
                    state = .format;
                    options = .{};
                    i -= 1;
                },
                .format => if (c == '}') {
                    const fmt = str.t[format_start..i];
                    var fmt_type: u8 = 0;

                    if (fmt.len == 1) {
                        fmt_type = fmt[0];
                    } else {
                        // TODO properly parse format options
                    }

                    switch (fmt_type) {
                        'x', 'X' => {
                            if (args.t[arg_i].* != .int) {
                                return ctx.throwFmt("'x' takes an integer as an argument, got '{s}'", .{args.t[arg_i].typeName()});
                            }
                            try std.fmt.formatInt(args.t[arg_i].int, 16, @intToEnum(std.fmt.Case, @boolToInt(fmt[0] == 'X')), options, w);
                        },
                        0 => if (args.t[arg_i].* == .str) {
                            try b.append(args.t[arg_i].str.data);
                        } else {
                            try args.t[arg_i].dump(w, default_dump_depth);
                        },
                        else => {
                            return ctx.throw("unknown format specifier");
                        },
                    }

                    state = .start;
                    arg_i += 1;
                },
            }
        }
        if (arg_i != args.t.len) {
            return ctx.throw("unused arguments");
        }

        const ret = try ctx.vm.gc.alloc();
        ret.* = Value{ .str = b.finish() };
        return ret;
    }

    pub fn join(str: Value.This([]const u8), ctx: Vm.Context, strs: Value.Variadic([]const u8)) !*Value {
        if (strs.t.len == 0) {
            const ret = try ctx.vm.gc.alloc();
            ret.* = Value.string("");
            return ret;
        }
        var b = builder(ctx.vm.gc.gpa);
        errdefer b.cancel();

        var len = str.t.len * (strs.t.len - 1);
        for (strs.t) |new| len += new.len;
        try b.inner.ensureUnusedCapacity(len);

        for (strs.t) |arg, i| {
            if (i != 0) {
                b.inner.appendSliceAssumeCapacity(str.t);
            }
            b.inner.appendSliceAssumeCapacity(arg);
        }

        const ret = try ctx.vm.gc.alloc();
        ret.* = Value{ .str = b.finish() };
        return ret;
    }
};

pub fn set(str: *String, ctx: Vm.Context, index: *const Value, new_val: *const Value) Value.NativeError!void {
    _ = str;
    _ = index;
    _ = new_val;
    return ctx.frame.fatal(ctx.vm, "TODO set string");
}

pub fn as(str: *String, ctx: Vm.Context, type_id: Type) Value.NativeError!*Value {
    if (type_id == .null) {
        return Value.Null;
    } else if (type_id == .bool) {
        if (mem.eql(u8, str.data, "true"))
            return Value.True
        else if (mem.eql(u8, str.data, "false"))
            return Value.False
        else
            return ctx.throw("cannot cast string to bool");
    }

    const new_val = try ctx.vm.gc.alloc();
    new_val.* = switch (type_id) {
        .int => .{
            .int = std.fmt.parseInt(i64, str.data, 0) catch |err|
                return ctx.throwFmt("cannot cast string to int: {s}", .{@errorName(err)}),
        },
        .num => .{
            .num = std.fmt.parseFloat(f64, str.data) catch |err|
                return ctx.throwFmt("cannot cast string to num: {s}", .{@errorName(err)}),
        },
        .bool => unreachable,
        .str => unreachable, // already string
        .tuple,
        .map,
        .list,
        => return ctx.frame.fatal(ctx.vm, "TODO more casts from string"),
        else => unreachable,
    };
    return new_val;
}

pub fn from(val: *Value, vm: *Vm) Vm.Error!*Value {
    const str = try vm.gc.alloc();

    if (val == Value.Null) {
        str.* = Value.string("null");
    } else if (val == Value.True) {
        str.* = Value.string("true");
    } else if (val == Value.False) {
        str.* = Value.string("false");
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
