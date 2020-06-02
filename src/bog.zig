const zig_std = @import("std");
const Allocator = zig_std.mem.Allocator;

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
pub const compile = Compiler.compile;

const value = @import("value.zig");
pub const Value = value.Value;
pub const Type = value.Type;

const vm = @import("vm.zig");
pub const Vm = vm.Vm;

pub const Gc = @import("Gc.zig");

const bytecode = @import("bytecode.zig");
pub const Op = bytecode.Op;
pub const Module = bytecode.Module;
pub const RegRef = bytecode.RegRef;
pub const Instruction = bytecode.Instruction;

pub const native = @import("native.zig");

pub const repl = @import("repl.zig");

pub const std = @import("std.zig");

/// file extension of bog text files
pub const extension = ".bog";

/// file extension of bog bytecode files, 'byte bog'
pub const bytecode_extension = ".bbog";

pub const version = zig_std.builtin.Version{
    .major = 0,
    .minor = 0,
    .patch = 1,
};

pub const Errors = struct {
    list: List,

    const Kind = enum {
        err,
        note,
        trace,
    };

    const List = zig_std.SegmentedList(struct {
        msg: []const u8,
        index: u32,
        kind: Kind,
    }, 0);

    pub fn init(alloc: *Allocator) Errors {
        return .{ .list = List.init(alloc) };
    }

    pub fn deinit(self: *Errors) void {
        self.list.deinit();
    }

    pub fn add(self: *Errors, msg: []const u8, index: u32, kind: Kind) !void {
        try self.list.push(.{
            .msg = msg,
            .index = index,
            .kind = kind,
        });
    }

    pub fn render(self: *Errors, source: []const u8, out_stream: var) !void {
        const RED = "\x1b[31;1m";
        const GREEN = "\x1b[32;1m";
        const BOLD = "\x1b[0;1m";
        const RESET = "\x1b[0m";
        const CYAN = "\x1b[36;1m";

        var it = self.list.iterator(0);
        while (it.next()) |e| {
            switch (e.kind) {
                .err => try out_stream.writeAll(RED ++ "error: " ++ BOLD),
                .note => try out_stream.writeAll(CYAN ++ "note: " ++ BOLD),
                .trace => {},
            }
            try out_stream.print("{}\n" ++ RESET, .{e.msg});

            const start = lineBegin(source, e.index);
            const end = zig_std.mem.indexOfScalarPos(u8, source, e.index, '\n') orelse source.len;
            try out_stream.writeAll(source[start..end]);
            try out_stream.writeAll(zig_std.cstr.line_sep);
            try out_stream.writeByteNTimes(' ', e.index - start);
            try out_stream.writeAll(GREEN ++ "^\n" ++ RESET);
        }
        self.list.shrink(0);
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
