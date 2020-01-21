const std = @import("std");

pub fn main() void {
    
}

comptime {
    _ = @import("parse.zig");
    _ = @import("tokenizer.zig");
    _ = @import("value.zig");
}
