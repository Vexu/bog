pub const io = @import("std/io.zig");
pub const fs = @import("std/fs.zig");
pub const math = @import("std/math.zig");
pub const os = @import("std/os.zig");
pub const map = @import("std/map.zig");
pub const debug = @import("std/debug.zig");
pub const json = @import("std/json.zig");
pub const gc = struct {
    pub fn collect(ctx: @import("bog.zig").Vm.Context) i64 {
        return @intCast(ctx.vm.gc.collect());
    }
};
