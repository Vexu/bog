const std = @import("std");
const Allocator = std.mem.Allocator;
const expectEqualSlices = std.testing.expectEqualSlices;

const wcwidth = @import("zig-wcwidth/src/main.zig").wcwidth;

pub fn width(s: []const u8) usize {
    var result: usize = 0;

    var escape_seq = false;
    const view = std.unicode.Utf8View.init(s) catch return 0;
    var iter = view.iterator();
    while (iter.nextCodepoint()) |codepoint| {
        if (escape_seq) {
            if (codepoint == 'm') {
                escape_seq = false;
            }
        } else {
            if (codepoint == '\x1b') {
                escape_seq = true;
            } else {
                const wcw = wcwidth(codepoint);
                if (wcw < 0) return 0;
                result += @intCast(usize, wcw);
            }
        }
    }

    return result;
}
