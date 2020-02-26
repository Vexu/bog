const std = @import("std");

const tokenizer = @import("tokenizer.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;
pub const tokenize = Tokenizer.tokenize;

const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const parse = Parser.parse;

const ast = @import("ast.zig");
pub const Tree = ast.Tree;
pub const Node = ast.Node;

const compiler = @import("compiler.zig");
pub const Compiler = compiler.Compiler;

const value = @import("value.zig");
pub const Value = value.Value;

const vm = @import("vm.zig");
pub const Vm = vm.Vm;

const bytecode = @import("bytecode.zig");
pub const Op = bytecode.Op;
pub const Module = bytecode.Module;
pub const RegRef = bytecode.RegRef;

const native = @import("native.zig");
pub const NativeFn = native.NativeFn;

pub const Error = struct {
    index: u32,
    kind: Kind,
    msg: []const u8,

    const Kind = enum {
        Error,
        Note,
        Trace,
    };

    pub const List = std.SegmentedList(Error, 0);

    pub fn render(errors: *List, source: []const u8, out_stream: var) !void {
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

            const start = lineBegin(source, e.index);
            const end = std.mem.indexOfScalarPos(u8, source, e.index, '\n') orelse source.len;
            try out_stream.write(source[start..end]);
            try out_stream.write(std.cstr.line_sep);
            try out_stream.writeByteNTimes(' ', e.index - start);
            try out_stream.write(GREEN ++ "^\n" ++ RESET);
        }
        errors.shrink(0);
    }

    fn lineBegin(slice: []const u8, start_index: usize) usize {
        var i = start_index;
        while (i != 0) {
            i -= 1;
            if (slice[i] == '\n') return i + 1;
        }
        return 0;
    }
};
