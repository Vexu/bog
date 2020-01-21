const std = @import("std");
const mem = std.mem;
const testing = std.testing;

pub const Parser = struct {
    it: unicode.Utf8Iterator,
    indent_char: ?u32 = null,
    chars_per_indent: ?u8 = null,
};
