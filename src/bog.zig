const zig_std = @import("std");
const Allocator = zig_std.mem.Allocator;

const tokenizer = @import("tokenizer.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;
pub const tokenize = tokenizer.tokenize;

const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const parse = parser.parse;

pub const Tree = @import("Tree.zig");
pub const Node = Tree.Node;

pub const Compiler = @import("Compiler.zig");
pub const compile = Compiler.compile;

const value = @import("value.zig");
pub const Value = value.Value;
pub const Type = value.Type;

pub const Vm = @import("Vm.zig");

pub const Gc = @import("Gc.zig");

pub const Bytecode = @import("Bytecode.zig");

pub const repl = @import("repl.zig");

pub const std = @import("std.zig");

/// file extension of bog text files
pub const extension = ".bog";

pub const version = zig_std.builtin.Version{
    .major = 0,
    .minor = 0,
    .patch = 1,
};

pub const Errors = struct {
    list: List = .{},
    arena: zig_std.heap.ArenaAllocator,

    pub const Kind = enum {
        err,
        note,
        trace,
    };

    const List = zig_std.ArrayListUnmanaged(struct {
        msg: @import("String.zig"),
        line_num: u32,
        col_num: u32,
        kind: Kind,
        path: []const u8,
        line: []const u8,
    });

    pub fn init(alloc: Allocator) Errors {
        return .{ .arena = zig_std.heap.ArenaAllocator.init(alloc) };
    }

    pub fn deinit(self: *Errors) void {
        for (self.list.items) |*err| {
            err.msg.deinit(self.arena.child_allocator);
        }
        self.list.deinit(self.arena.child_allocator);
        self.arena.deinit();
    }

    pub fn add(
        self: *Errors,
        msg: @import("String.zig"),
        source: []const u8,
        path: []const u8,
        byte_offset: u32,
        kind: Kind,
    ) !void {
        var start: u32 = 0;
        // find the start of the line which is either a newline or a splice
        var line_num: u32 = 1;
        var i: u32 = 0;
        while (i < byte_offset) : (i += 1) {
            if (source[i] == '\n') {
                start = i + 1;
                line_num += 1;
            }
        }
        const col_num = byte_offset - start;

        // find the end of the line
        while (i < source.len) : (i += 1) {
            if (source[i] == '\n') break;
        }
        try self.list.append(self.arena.child_allocator, .{
            .msg = msg,
            .line = try self.arena.allocator().dupe(u8, source[start..i]),
            .path = try self.arena.allocator().dupe(u8, path),
            .line_num = line_num,
            .col_num = col_num + 1,
            .kind = kind,
        });
    }

    pub fn render(self: *Errors, writer: anytype) !void {
        // TODO should be an arg
        const tty = zig_std.io.tty.detectConfig(zig_std.io.getStdErr());
        const gpa = self.arena.child_allocator;
        for (self.list.items) |*e| {
            const prefix = if (zig_std.fs.path.dirname(e.path) == null and e.path[0] != '<') "." ++ zig_std.fs.path.sep_str else "";
            switch (e.kind) {
                .err => {
                    try tty.setColor(writer, .white);
                    try writer.print("{s}{s}:{d}:{d}: ", .{ prefix, e.path, e.line_num, e.col_num });
                    try tty.setColor(writer, .red);
                    try writer.writeAll("error: ");
                    try tty.setColor(writer, .white);
                },
                .note => {
                    try tty.setColor(writer, .white);
                    try writer.print("{s}{s}:{d}:{d}: ", .{ prefix, e.path, e.line_num, e.col_num });
                    try tty.setColor(writer, .cyan);
                    try writer.writeAll("note: ");
                    try tty.setColor(writer, .white);
                },
                .trace => {
                    try writer.print("{s}{s}:{d}:{d}: ", .{ prefix, e.path, e.line_num, e.col_num });
                },
            }
            try writer.print("{s}\n", .{e.msg.data});
            try tty.setColor(writer, .reset);

            try writer.print("{s}\n", .{e.line});
            try writer.writeByteNTimes(' ', e.col_num - 1);
            if (e.kind != .trace) try tty.setColor(writer, .green);
            try writer.writeAll("^\n");
            try tty.setColor(writer, .reset);
            e.msg.deinit(gpa);
        }
        self.list.items.len = 0;
        self.arena.deinit();
        self.arena = zig_std.heap.ArenaAllocator.init(gpa);
    }
};
