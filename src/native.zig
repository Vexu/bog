const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub const Registry = struct {
    map: std.StringHashMap(Native),

    // initializes registry and registers all builtin functions
    pub fn init(allocator: *Allocator) Registry {
        return .{
            .map = std.StringHashMap(Native).init(allocator),
        };
    }

    pub fn deinit(self: *Registry) void {
        self.map.deinit();
    }

    pub fn register(self: *Registry, name: []const u8, comptime func: var) !void {
        try self.map.putNoClobber(name, wrap(func));
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
                return Value.zigToBog(vm, func());

            const arg_1 = try bog_args[bog_arg_i].bogToZig(Fn.args[0].arg_type.?, vm);
            if (@TypeOf(arg_1) != *Vm) bog_arg_i += 1;
            if (Fn.args.len == 1)
                return Value.zigToBog(vm, func(arg_1));

            const arg_2 = try bog_args[bog_arg_i].bogToZig(Fn.args[1].arg_type.?, vm);
            if (@TypeOf(arg_2) != *Vm) bog_arg_i += 1;
            if (Fn.args.len == 2)
                return Value.zigToBog(vm, func(arg_1, arg_2));

            // @compileError("TODO too many args");
            // var args = .{};
            // inline for (Fn.args) |arg, i| {
            //     const val = bog_args[i];
            //     const T = arg.arg_type.?;
            //     // args = args ++
            // }
            // return Value.zigToBog(vm, @call(.{}, func, args));
        }
    }.native;

    return Native{
        // TODO this is reset to 0 for some reason
        .arg_count = bog_arg_i,
        .func = wrapped_func,
    };
}
