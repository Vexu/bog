pub const io = @import("std/io.zig");
pub const math = @import("std/math.zig");
pub const os = @import("std/os.zig");
pub const map = @import("std/map.zig");
pub const debug = @import("std/debug.zig");
pub const json = @import("std/json.zig");
pub const gc = struct {
    pub fn collect(vm: *@import("bog.zig").Vm) i64 {
        return @intCast(i64, vm.gc.collect());
    }
};
