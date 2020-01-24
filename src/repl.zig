const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig").Parser;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Builder = @import("bytecode.zig").Builder;

pub fn run(allocator: *Allocator, in_stream: var, out_stream: var) !void {
    var buffer = try ArrayList(u8).initCapacity(allocator, std.mem.page_size);
    defer buffer.deinit();
    var builder: Builder = undefined;
    try builder.init(allocator);
    defer builder.deinit();
    var tokenizer = Tokenizer.init(allocator, true);
    defer tokenizer.deinit();

    while (true) {
        var begin_index = tokenizer.tokens.len;
        if (begin_index != 0) begin_index -= 1;
        readLine(&buffer, ">>> ", in_stream, out_stream) catch |err| switch (err) {
            error.EndOfStream => return,
            else => |e| return e,
        };
        while (!(try tokenizer.tokenize(buffer.toSliceConst()))) {
            readLine(&buffer, "... ", in_stream, out_stream) catch |err| switch (err) {
                error.EndOfStream => return,
                else => |e| return e,
            };
        }
        try Parser.parse(&builder, &tokenizer.tokens.iterator(begin_index));

        // TODO
        // vm.exec(parser.builder)
        var token_it = tokenizer.tokens.iterator(begin_index);
        while (token_it.next()) |tok| {
            try out_stream.print("{}\n", .{tok});
        }
    }
}

fn readLine(buffer: *ArrayList(u8), prompt: []const u8, in_stream: var, out_stream: var) !void {
    try out_stream.write(prompt);
    const start_len = buffer.len;
    while (true) {
        var byte: u8 = in_stream.readByte() catch |e| switch (e) {
            error.EndOfStream => if (start_len == buffer.len) {
                try out_stream.write(std.cstr.line_sep);
                return error.EndOfStream;
            } else continue,
            else => |err| return err,
        };
        try buffer.append(byte);

        if (byte == '\n') {
            return;
        }

        if (buffer.len - start_len == 1024) {
            return error.StreamTooLong;
        }
    }
}
