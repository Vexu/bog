const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Parser = @import("parser.zig").Parser;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Builder = @import("bytecode.zig").Builder;
const Vm = @import("vm.zig").Vm;

pub fn run(allocator: *Allocator, in_stream: var, out_stream: var) !void {
    var buffer = try ArrayList(u8).initCapacity(allocator, std.mem.page_size);
    defer buffer.deinit();
    var builder: Builder = undefined;
    try builder.init(allocator);
    defer builder.deinit();
    var tokenizer = Tokenizer.init(allocator, true);
    defer tokenizer.deinit();
    var parser = Parser.init(allocator, &builder);
    defer parser.deinit();
    var vm = Vm.init(allocator);
    defer vm.deinit();

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
        parser.parse(&tokenizer.tokens.iterator(begin_index)) catch |err| switch (err) {
            error.ParseError => {
                const RED = "\x1b[31;1m";
                const BOLD = "\x1b[0;1m";
                const RESET = "\x1b[0m";

                var it = parser.errors.iterator(0);
                while (it.next()) |e| {
                    try out_stream.write(RED ++ "error: " ++ BOLD);
                    try e.render(out_stream);
                    try out_stream.write("\n" ++ RESET);
                    // TODO print location
                }
                return;
            },
            else => |e| return e,
        };

        vm.code = builder.cur_func.code.toSliceConst(); // TODO
        try vm.exec();
        // var token_it = tokenizer.tokens.iterator(begin_index);
        // while (token_it.next()) |tok| {
        //     try out_stream.print("{}\n", .{tok});
        // }
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
