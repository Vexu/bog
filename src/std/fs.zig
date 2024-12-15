const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub fn open(ctx: Vm.Context, path: []const u8) !*Value {
    // TODO take options as parameters
    const res = try ctx.vm.gc.alloc(.native_val);
    res.* = .{ .native_val = .{
        .vtable = Value.NativeVal.VTable.make(File),
        .type_id = Value.NativeVal.typeId(File),
        .ptr = try ctx.vm.gc.gpa.create(File),
    } };
    const file = Value.NativeVal.unwrap(res.native_val.ptr, File);
    file.* = .{
        .base = try std.fs.cwd().openFile(path, .{ .mode = .read_write }),
        .state = .open,
    };
    return res;
}

const File = struct {
    base: std.fs.File,
    state: enum { open, closed },

    pub fn typeName(_: *anyopaque) []const u8 {
        return "File";
    }
    pub fn deinit(a: *anyopaque, gpa: std.mem.Allocator) void {
        gpa.destroy(Value.NativeVal.unwrap(a, File));
    }

    pub fn get(_: *anyopaque, ctx: Vm.Context, index: *const Value, res: *?*Value) Value.NativeError!void {
        switch (index.*) {
            .str => |*s| {
                if (res.* == null) {
                    res.* = try ctx.vm.gc.alloc(.int);
                }

                inline for (@typeInfo(methods).@"struct".decls) |method| {
                    if (std.mem.eql(u8, s.data, method.name)) {
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
        pub fn close(file: Value.This(*File), ctx: Vm.Context) !void {
            if (file.t.state == .closed) return ctx.throw("closing an already closed file");
            file.t.state = .closed;
            file.t.base.close();
        }
        pub fn read(file: Value.This(*File), ctx: Vm.Context) ![]u8 {
            if (file.t.state == .closed) return ctx.throw("reading from a closed file");
            return file.t.base.readToEndAlloc(ctx.vm.gc.gpa, std.math.maxInt(u32));
        }
        pub fn write(file: Value.This(*File), ctx: Vm.Context, str: []const u8) !void {
            if (file.t.state == .closed) return ctx.throw("writing to a closed file");
            try file.t.base.writeAll(str);
        }
    };
};
