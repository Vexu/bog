const std = @import("std");

pub fn parseInt(in_str: []const u8) !i64 {
    var str = in_str;
    var negate = false;
    if (str.len > 1 and str[0] == '-') {
        negate = true;
        str = str[1..];
    }

    var radix: u8 = if (str.len > 2) switch (str[1]) {
        'x' => @as(u8, 16),
        'b' => 2,
        'o' => 8,
        else => 10,
    } else 10;
    if (radix != 10) str = str[2..];
    var x: i64 = 0;

    for (str) |c| {
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z' => c - 'A' + 10,
            'a'...'z' => c - 'a' + 10,
            '_' => continue,
            else => return error.InvalidCharacter,
        };

        x = try std.math.mul(i64, x, radix);
        // why is this cast needed?
        x += @intCast(i32, digit);
    }

    if (negate)
        x = -x;

    return x;
}

pub fn parseNum(str: []const u8) !f64 {
    var buf: [256]u8 = undefined;

    var i: u32 = 0;
    for (str) |c| {
        if (c == ',') {
            buf[i] = '.';
            i += 1;
        } else if (c != '_') {
            buf[i] = c;
            i += 1;
        }
    }

    return try std.fmt.parseFloat(f64, buf[0..i]);
}
