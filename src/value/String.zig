const std = @import("std");
const mem = std.mem;
const bog = @import("../bog.zig");
const Type = bog.Type;
const Value = bog.Value;
const Vm = bog.Vm;


const String = @This();

const default_dump_depth = 4;

inner: extern struct {
    len: u32,
    capacity: u32,
},

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
                    res.* = try Value.zigToBog(ctx.vm, @field(methods, method.name));
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
    pub fn append(str: Value.This(*String), ctx: Vm.Context, data: []const u8) !void {
        var b = builder(ctx.vm.gc.gpa);
        errdefer b.cancel();

        try b.append(str.t.data);
        try b.append(data);

        str.t.deinit(ctx.vm.gc.gpa);
        str.t.* = b.finish();
    }

    pub fn format(str: Value.This([]const u8), ctx: Vm.Context, args: []const *Value) !*Value {
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
                    if (arg_i >= args.len) {
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
                            if (args[arg_i].* != .int) {
                                return ctx.throwFmt("'x' takes an integer as an argument, got '{s}'", .{@tagName(args[arg_i].*)});
                            }
                            try std.fmt.formatInt(args[arg_i].int, 16, @intToEnum(std.fmt.Case, @boolToInt(fmt[0] == 'X')), options, w);
                        },
                        0 => if (args[arg_i].* == .str) {
                            try b.append(args[arg_i].str.data);
                        } else {
                            try args[arg_i].dump(w, default_dump_depth);
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
        if (arg_i != args.len) {
            return ctx.throw("unused arguments");
        }

        const ret = try ctx.vm.gc.alloc();
        ret.* = Value{ .str = b.finish() };
        return ret;
    }

    pub fn join(str: Value.This([]const u8), ctx: Vm.Context, args: []const *Value) !*Value {
        var b = builder(ctx.vm.gc.gpa);
        errdefer b.cancel();
        try b.inner.ensureTotalCapacity(args.len * str.t.len);

        for (args) |arg, i| {
            if (i != 0) {
                try b.append(str.t);
            }
            if (arg.* != .str) {
                return ctx.throwFmt("join only accepts strings, got '{s}'", .{@tagName(arg.*)});
            }
            try b.append(arg.str.data);
        }

        const ret = try ctx.vm.gc.alloc();
        ret.* = Value{ .str = b.finish() };
        return ret;
    }
};

pub fn set(str: *String, ctx: Vm.Context, index: *const Value, new_val: *const Value) Vm.Error!void {
    _ = str;
    _ = index;
    _ = new_val;
    return ctx.frame.fatal(ctx.vm, "TODO set string");
}

pub fn as(str: *String, vm: *Vm, type_id: Type) Vm.Error!*Value {
    if (type_id == .@"null") {
        return Value.Null;
    } else if (type_id == .bool) {
        if (mem.eql(u8, str.data, "true"))
            return Value.True
        else if (mem.eql(u8, str.data, "false"))
            return Value.False
        else
            return vm.errorVal("cannot cast string to bool");
    }

    const new_val = try vm.gc.alloc();
    new_val.* = switch (type_id) {
        .int => .{
            .int = std.fmt.parseInt(i64, str.data, 0) catch |err|
                return vm.errorFmt("cannot cast string to int: {s}", .{@errorName(err)}),
        },
        .num => .{
            .num = std.fmt.parseFloat(f64, str.data) catch |err|
                return vm.errorFmt("cannot cast string to num: {s}", .{@errorName(err)}),
        },
        .bool => unreachable,
        .str => unreachable, // already string
        .tuple,
        .map,
        .list,
        => return vm.errorVal("TODO more casts from string"),
        else => unreachable,
    };
    return new_val;
}

pub fn from(val: *Value, vm: *Vm) Vm.Error!*Value {
    const str = try vm.gc.alloc();

    if (val.* == .@"null") {
        str.* = Value.string("null");
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
