const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig").Parser;

pub fn run(allocator: *Allocator, in_stream: var, out_stream: var) !void {
    var buffer = try ArrayList(u8).initCapacity(allocator, std.mem.page_size);
    defer buffer.deinit();
    var parser = Parser.init(allocator);
    defer parser.deinit();
    parser.tokenizer.repl = true;

    while (true) {
        try out_stream.write("<<< ");
        readLine(&buffer, in_stream) catch |e| switch (e) {
            error.EndOfStream => return out_stream.write(std.cstr.line_sep),
            else => |err| return err,
        };
        try parser.parse(buffer.toSliceConst());

        // TODO
        // executor.exec(parser.builder)
        while (parser.token_it.next()) |tok| {
            try out_stream.print("{}\n", .{tok});
        }
    }
}

fn readLine(buffer: *ArrayList(u8), in_stream: var) !void {
    const start_len = buffer.len;
    while (true) {
        var byte: u8 = in_stream.readByte() catch |e| switch (e) {
            error.EndOfStream => if (start_len == buffer.len) return error.EndOfStream else continue,
            else => |err| return err,
        };
        try buffer.append(byte);

        if (byte == '\n') {
            return;
        }

        // if (buffer.len() == std.mem.page_size) {
        //     return error.StreamTooLong;
        // }
    }
}
