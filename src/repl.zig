const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const lang = @import("lang.zig");
const Tree = lang.Tree;
const Tokenizer = lang.Tokenizer;
const Compiler = lang.Compiler;
const Vm = lang.Vm;

pub fn run(allocator: *Allocator, in_stream: var, out_stream: var) !void {
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    defer arena_allocator.deinit();
    const arena = &arena_allocator.allocator;
    var tree = Tree{
        .source = undefined,
        .arena_allocator = arena_allocator,
        .tokens = lang.Token.List.init(arena),
        .nodes = lang.Node.List.init(arena),
        .errors = lang.Error.List.init(arena),
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
                .id = .Fn,
                .parent = null,
                .syms = Compiler.Symbol.List.init(allocator),
            },
            .code = Compiler.Code.init(allocator),
            .cur_scope = undefined,
        },
    };
    repl.compiler.cur_scope = &repl.compiler.root_scope;
    defer repl.vm.deinit();
    defer repl.buffer.deinit();

    // TODO move this
    try repl.vm.gc.stackAlloc(250);

    while (true) {
        repl.handleLine(in_stream, out_stream) catch |err| switch (err) {
            error.EndOfStream => return,
            error.TokenizeError, error.ParseError, error.CompileError => try repl.renderErrors(&repl.tree.errors, in_stream, out_stream),
            error.RuntimeError => try repl.renderErrors(&repl.vm.errors, in_stream, out_stream),
            else => |e| return e,
        };
    }
}

const Repl = struct {
    vm: Vm,
    tokenizer: Tokenizer,
    tree: *Tree,
    buffer: ArrayList(u8),
    module: lang.Module,
    compiler: Compiler,

    fn lineBegin(slice: []const u8, start_index: usize) usize {
        var i = start_index;
        while (i != 0) {
            i -= 1;
            if (slice[i] == '\n') return i + 1;
        }
        return 0;
    }

    fn renderErrors(repl: *Repl, errors: *lang.Error.List, in_stream: var, out_stream: var) !void {
        const RED = "\x1b[31;1m";
        const GREEN = "\x1b[32;1m";
        const BOLD = "\x1b[0;1m";
        const RESET = "\x1b[0m";
        const CYAN = "\x1b[36;1m";

        var it = errors.iterator(0);
        while (it.next()) |e| {
            switch (e.kind) {
                .Error => try out_stream.write(RED ++ "error: " ++ BOLD),
                .Note => try out_stream.write(CYAN ++ "note: " ++ BOLD),
                .Trace => {},
            }
            try out_stream.print("{}\n" ++ RESET, .{e.msg});

            const slice = repl.buffer.toSliceConst();
            const start = lineBegin(slice, e.index);
            const end = std.mem.indexOfScalarPos(u8, slice, e.index, '\n') orelse slice.len;
            try out_stream.write(slice[start..end]);
            try out_stream.write(std.cstr.line_sep);
            try out_stream.writeByteNTimes(' ', e.index - start);
            try out_stream.write(GREEN ++ "^\n" ++ RESET);
        }
        errors.shrink(0);
    }

    fn handleLine(repl: *Repl, in_stream: var, out_stream: var) !void {
        var begin_index = repl.tree.tokens.len;
        if (begin_index != 0) begin_index -= 1;
        try repl.readLine(">>> ", in_stream, out_stream);
        while (!(try repl.tokenizer.tokenizeRepl(repl.buffer.toSliceConst()))) {
            try repl.readLine("... ", in_stream, out_stream);
        }
        const node = (try lang.Parser.parseRepl(repl.tree, begin_index)) orelse return;
        try repl.compiler.compileRepl(node, &repl.module);

        try repl.vm.exec(&repl.module);
        if (repl.vm.result) |some| {
            try some.dump(out_stream, 2);
            try out_stream.writeByte('\n');
            // vm.result.deref();
            repl.vm.result = null;
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
