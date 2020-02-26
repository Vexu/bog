const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bog = @import("bog.zig");
const Tree = bog.Tree;
const Tokenizer = bog.Tokenizer;
const Compiler = bog.Compiler;
const Vm = bog.Vm;

pub fn run(allocator: *Allocator, in_stream: var, out_stream: var) !void {
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;
    var tree = Tree{
        .source = undefined,
        .arena_allocator = arena_allocator,
        .tokens = bog.Token.List.init(arena),
        .nodes = bog.Node.List.init(arena),
        .errors = bog.Error.List.init(arena),
    };

    var repl = Repl{
        .module = .{
            .name = "<stdin>",
            .code = "",
            .strings = "",
            .start_index = 0,
        },
        .tree = &tree,
        .vm = Vm.init(allocator, true),
        .buffer = try ArrayList(u8).initCapacity(allocator, std.mem.page_size),
        .tokenizer = .{
            .tree = &tree,
            .it = .{
                .bytes = "",
                .i = 0,
            },
            .repl = true,
        },
        .compiler = .{
            .tree = &tree,
            .arena = arena,
            .root_scope = .{
                .base = .{
                    .id = .Fn,
                    .parent = null,
                    .syms = Compiler.Symbol.List.init(allocator),
                },
                .code = Compiler.Code.init(allocator),
            },
            .module_code = Compiler.Code.init(allocator),
            .strings = Compiler.Code.init(allocator),
            .code = undefined,
            .cur_scope = undefined,
        },
    };
    repl.compiler.code = &repl.compiler.root_scope.code;
    repl.compiler.cur_scope = &repl.compiler.root_scope.base;
    defer repl.vm.deinit();
    defer repl.buffer.deinit();

    while (true) {
        repl.handleLine(in_stream, out_stream) catch |err| switch (err) {
            error.EndOfStream => return,
            error.TokenizeError, error.ParseError, error.CompileError => try bog.Error.render(&repl.tree.errors, repl.buffer.toSliceConst(), out_stream),
            error.RuntimeError => {
                repl.vm.ip = repl.module.code.len;
                try bog.Error.render(&repl.tree.errors, repl.buffer.toSliceConst(), out_stream);
            },
            else => |e| return e,
        };
    }
}

const Repl = struct {
    vm: Vm,
    tokenizer: Tokenizer,
    tree: *Tree,
    buffer: ArrayList(u8),
    module: bog.Module,
    compiler: Compiler,

    fn handleLine(repl: *Repl, in_stream: var, out_stream: var) !void {
        var begin_index = repl.tree.tokens.len;
        if (begin_index != 0) begin_index -= 1;
        try repl.readLine(">>> ", in_stream, out_stream);
        while (!(try repl.tokenizer.tokenizeRepl(repl.buffer.toSliceConst()))) {
            try repl.readLine("... ", in_stream, out_stream);
        }
        const node = (try bog.Parser.parseRepl(repl.tree, begin_index)) orelse return;
        repl.vm.ip += try repl.compiler.compileRepl(node, &repl.module);

        const res = try repl.vm.exec(&repl.module);
        if (res) |some| {
            try some.dump(out_stream, 2);
            try out_stream.writeByte('\n');
        }

        // reset arena
        repl.tree.arena_allocator.deinit();
        repl.tree.arena_allocator = std.heap.ArenaAllocator.init(repl.buffer.allocator);
    }

    fn readLine(repl: *Repl, prompt: []const u8, in_stream: var, out_stream: var) !void {
        try out_stream.write(prompt);
        const start_len = repl.buffer.len;
        while (true) {
            var byte: u8 = in_stream.readByte() catch |e| switch (e) {
                error.EndOfStream => if (start_len == repl.buffer.len) {
                    try out_stream.write(std.cstr.line_sep);
                    return error.EndOfStream;
                } else continue,
                else => |err| return err,
            };
            try repl.buffer.append(byte);

            if (byte == '\n') {
                return;
            }

            if (repl.buffer.len - start_len == 1024) {
                return error.StreamTooLong;
            }
        }
    }
};
