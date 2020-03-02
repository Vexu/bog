const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub const Native = struct {
    arg_count: u8,

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
};

pub const Native = struct {
    arg_count: u8,

    func: fn (*Vm, []*Value) Vm.Error!*Value,
};

pub fn wrap(comptime func: var) Native {
    const Fn = @typeInfo(@TypeOf(func)).Fn;
    if (Fn.is_generic or Fn.is_var_args or Fn.return_type == null)
        @compileError("unsupported function");

    const wrapped_func = struct {
        fn native(vm: *Vm, bog_args: []*Value) Vm.Error!*Value {
            std.debug.assert(bog_args.len == Fn.args.len);
            var args = .{};
            inline for (Fn.args) |arg, i| {
                const val = bog_args[i];
                const T = arg.arg_type.?;
                args = args ++ switch (T) {
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
                    // TODO unable to evaluate constant expression
                    *Vm, *const Vm => vm,
                    *Value, *const Value => val,
                    else => blk: {
                        switch (@typeInfo(T)) {
                            // .Int => |info| if (info.is_signed) {} else {},
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
            return getRet(vm, @call(.{}, func, args));
        }
    }.native;

    return Native{
        .arg_count = Fn.args.len,
        .func = wrapped_func,
    };
}

fn getRet(vm: *Vm, val: var) Vm.Error!*Value {
    switch (@TypeOf(val)) {
        void => return &Value.None,
        bool => return if (val) &Value.True else &Value.False,
        else => @compileError("TODO unsupported type"),
    }
}
