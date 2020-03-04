const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub const Registry = struct {
    map: std.StringHashMap(Native),

    // initializes registry and registers all builtin functions
    pub fn init(allocator: *Allocator) Allocator.Error!Registry {
        var reg = Registry{
            .map = std.StringHashMap(Native).init(allocator),
        };
        return reg;
    }

    pub fn deinit(self: *Registry) void {
        self.map.deinit();
    }

    pub fn register(self: *Registry, name: []const u8, comptime func: var) !void {
        std.debug.assert((try self.map.put(name, wrap(func))) == null);
    }
};

pub const Native = struct {
    arg_count: u8,

    func: fn (*Vm, []*Value) Vm.Error!*Value,
};

fn wrap(comptime func: var) Native {
    const Fn = @typeInfo(@TypeOf(func)).Fn;
    if (Fn.is_generic or Fn.is_var_args or Fn.return_type == null)
        @compileError("unsupported function");

    comptime var bog_arg_i: u8 = 0;
    const wrapped_func = struct {
        fn native(vm: *Vm, bog_args: []*Value) Vm.Error!*Value {
            if (Fn.args.len == 0)
                return getRet(vm, func());

            const arg_1 = try getArg(Fn.args[0].arg_type.?, vm, bog_args[bog_arg_i]);
            if (@TypeOf(arg_1) != *Vm) bog_arg_i += 1;
            if (Fn.args.len == 1)
                return getRet(vm, func(arg_1));

            const arg_2 = try getArg(Fn.args[1].arg_type.?, vm, bog_args[bog_arg_i]);
            if (@TypeOf(arg_2) != *Vm) bog_arg_i += 1;
            if (Fn.args.len == 2)
                return getRet(vm, func(arg_1, arg_2));

            @compileError("TODO too many args");
            // var args = .{};
            // inline for (Fn.args) |arg, i| {
            //     const val = bog_args[i];
            //     const T = arg.arg_type.?;
            //     // args = args ++
            // }
            // return getRet(vm, @call(.{}, func, args));
        }
    }.native;

    return Native{
        // TODO this is reset to 0 for some reason
        .arg_count = bog_arg_i,
        .func = wrapped_func,
    };
}

fn getRet(vm: *Vm, val: var) Vm.Error!*Value {
    switch (@TypeOf(val)) {
        void => return &Value.None,
        bool => return if (val) &Value.True else &Value.False,
        *Value => return val,
        []u8 => {
            // assume val was allocated with vm.gc
            const str = try vm.gc.alloc();
            str.* = .{
                .kind = .{
                    .Str = val,
                },
            };
            return str;
        },
        else => switch (@typeInfo(@TypeOf(val))) {
            .ErrorUnion => if (val) |some| {
                return getRet(vm, some);
            } else |e| {
                // wrap error string
                const str = try vm.gc.alloc();
                str.* = .{
                    .kind = .{
                        .Str = @errorName(e),
                    },
                };
                const err = try vm.gc.alloc();
                err.* = .{
                    .kind = .{
                        .Error = str,
                    },
                };
                return err;
            },
            else => @compileError("TODO unsupported type"),
        },
    }
}

fn getArg(comptime T: type, vm: *Vm, val: *Value) Vm.Error!T {
    return switch (T) {
        void => {
            if (val.kind != .None)
                return vm.reportErr("expected none");
        },
        bool => blk: {
            if (val.kind != .Bool)
                return vm.reportErr("expected bool");
            break :blk val.kind.Bool;
        },
        []const u8 => blk: {
            if (val.kind != .Str)
                return vm.reportErr("expected num");
            break :blk val.kind.Str;
        },
        *Vm => vm,
        *Value, *const Value => val,
        else => blk: {
            switch (@typeInfo(T)) {
                .Int => if (val.kind == .Int) {
                    // TODO make this safe
                    break :blk @intCast(T, val.kind.Int);
                } else if (val.kind == .Num) {
                    break :blk @intCast(T, @floatToInt(i64, val.kind.Num));
                } else {
                    return vm.reportErr("expected int");
                },
                .Float => |info| switch (info.bits) {
                    32 => if (val.kind == .Num) {
                        break :blk @floatCast(f32, val.kind.Num);
                    } else if (val.kind == .Int) {
                        break :blk @intToFloat(f32, val.kind.Int);
                    } else {
                        return vm.reportErr("expected num");
                    },
                    64 => if (val.kind == .Num) {
                        break :blk val.kind.Num;
                    } else if (val.kind == .Int) {
                        break :blk @intToFloat(f64, val.kind.Int);
                    } else {
                        return vm.reportErr("expected num");
                    },
                    else => @compileError("unsupported float"),
                },
                else => @compileError("TODO unsupported type"),
            }
        },
    };
}