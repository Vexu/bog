const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub fn parse(ctx: Vm.Context, str: []const u8) !*Value {
    var tokens = std.json.TokenStream.init(str);
    const token = (try tokens.next()) orelse return error.UnexpectedEndOfJson;
    return parseInternal(ctx.vm, token, &tokens);
}

const Error = error{ UnexpectedToken, UnexpectedEndOfJson } || std.mem.Allocator.Error || std.json.TokenStream.Error ||
    std.fmt.ParseIntError;
fn parseInternal(vm: *Vm, token: std.json.Token, tokens: *std.json.TokenStream) Error!*Value {
    switch (token) {
        .ObjectBegin => {
            const res = try vm.gc.alloc(.map);
            res.* = .{ .map = .{} };

            while (true) {
                var tok = (try tokens.next()) orelse return error.UnexpectedEndOfJson;
                switch (tok) {
                    .ObjectEnd => break,
                    else => {},
                }
                const key = try parseInternal(vm, tok, tokens);
                tok = (try tokens.next()) orelse return error.UnexpectedEndOfJson;
                const val = try parseInternal(vm, tok, tokens);
                try res.map.put(vm.gc.gpa, key, val);
            }
            return res;
        },
        .ArrayBegin => {
            const res = try vm.gc.alloc(.list);
            res.* = .{ .list = .{} };

            while (true) {
                const tok = (try tokens.next()) orelse return error.UnexpectedEndOfJson;
                switch (tok) {
                    .ArrayEnd => break,
                    else => {},
                }
                try res.list.inner.append(vm.gc.gpa, try parseInternal(vm, tok, tokens));
            }
            return res;
        },
        .ObjectEnd, .ArrayEnd => return error.UnexpectedToken,
        .String => |info| {
            const source_slice = info.slice(tokens.slice, tokens.i - 1);
            const val = try vm.gc.alloc(.str);
            switch (info.escapes) {
                .None => val.* = Value.string(try vm.gc.gpa.dupe(u8, source_slice)),
                .Some => {
                    const output = try vm.gc.gpa.alloc(u8, info.decodedLength());
                    errdefer vm.gc.gpa.free(output);
                    try std.json.unescapeValidString(output, source_slice);
                    val.* = Value.string(output);
                },
            }
            return val;
        },
        .Number => |info| {
            const val = try vm.gc.alloc(.int);
            if (info.is_integer) {
                val.* = .{ .int = try std.fmt.parseInt(i64, info.slice(tokens.slice, tokens.i - 1), 10) };
            } else {
                val.* = .{ .num = try std.fmt.parseFloat(f64, info.slice(tokens.slice, tokens.i - 1)) };
            }
            return val;
        },
        .True => return Value.True,
        .False => return Value.False,
        .Null => return Value.Null,
    }
}

pub fn stringify(ctx: Vm.Context, val: *Value) !Value.String {
    var b = Value.String.builder(ctx.vm.gc.gpa);
    errdefer b.cancel();
    try std.json.stringify(val, .{}, b.writer());
    return b.finish();
}
