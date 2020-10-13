const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Errors = bog.Errors;

pub fn run(gpa: *Allocator, reader: anytype, writer: anytype) !void {
    var repl = try Repl.init(gpa);
    defer repl.deinit();

    while (true) {
        repl.handleLine(reader, writer) catch |err| switch (err) {
            error.EndOfStream => return,
            error.TokenizeError, error.ParseError, error.CompileError => try repl.vm.errors.render(repl.buffer.items, writer),
            error.RuntimeError => {
                (try repl.vm.gc.stackRef(0)).* = &bog.Value.None;
                try repl.vm.errors.render(repl.buffer.items, writer);
            },
            else => |e| return e,
        };
    }
}

pub const Repl = struct {
    gpa: *Allocator,
    arena: std.heap.ArenaAllocator,
    vm: Vm,
    buffer: ArrayList(u8),
    strings: std.ArrayList(u8),
    module_code: bog.Compiler.Code,
    string_interner: std.StringHashMap(u32),
    tokenizer: bog.Tokenizer,

    tok_index: bog.Token.Index = 0,
    /// 1 for "ans"
    used_regs: bog.RegRef = 1,

    const tokenize = @import("tokenizer.zig").tokenizeRepl;
    const parse = @import("parser.zig").parseRepl;
    const compile = @import("compiler.zig").compileRepl;

    fn init(gpa: *Allocator) !Repl {
        const buffer = try ArrayList(u8).initCapacity(gpa, std.mem.page_size);
        errdefer buffer.deinit();
        var vm = Vm.init(gpa, .{ .repl = true, .import_files = true });
        errdefer vm.deinit();
        try vm.addStd();
        var syms = bog.Compiler.Symbol.List.init(gpa);
        errdefer syms.deinit();

        // declare 'ans' for the result of the previous input
        try syms.append(.{
            .name = "ans",
            .reg = 0,
            .mutable = false,
        });

        const ans = try vm.gc.stackRef(0);
        ans.* = &bog.Value.None;

        return Repl{
            .gpa = gpa,
            .arena = std.heap.ArenaAllocator.init(gpa),
            .vm = vm,
            .buffer = buffer,
            .tokenizer = .{
                .tokens = bog.Token.List.init(gpa),
                .errors = undefined,
                .it = .{
                    .i = 0,
                    .bytes = "",
                },
                .repl = true,
            },
            .root_scope = .{
                .base = .{
                    .id = .module,
                    .parent = null,
                    .syms = syms,
                },
                .code = bog.Compiler.Code.init(gpa),
            },
            .strings = ArrayList(u8).init(gpa),
            .module_code = bog.Compiler.Code.init(gpa),
            .string_interner = std.StringHashMap(u32).init(gpa),
        };
    }

    fn deinit(repl: *Repl) void {
        repl.vm.deinit();
        repl.arena.deinit();
        repl.buffer.deinit();
        repl.strings.deinit();
        repl.module_code.deinit();
        repl.string_interner.deinit();
        repl.root_scope.code.deinit();
        repl.tokenizer.tokens.deinit();
        repl.root_scope.base.syms.deinit();
    }

    fn handleLine(repl: *Repl, reader: anytype, writer: anytype) !void {
        defer {
            repl.arena.deinit();
            repl.arena = std.heap.ArenaAllocator.init(repl.gpa);
        }
        try repl.readLine(">>> ", reader, writer);
        while (!(try repl.tokenize())) {
            try repl.readLine("... ", reader, writer);
        }
        const node = (try repl.parse()) orelse return;
        var mod = try repl.compile(node);
        repl.vm.ip = mod.entry;

        const res = try repl.vm.exec(&mod);
        (try repl.vm.gc.stackRef(0)).* = res;
        if (res.* == .none) return;
        try res.dump(writer, 2);
        try writer.writeByte('\n');
    }

    fn readLine(repl: *Repl, prompt: []const u8, reader: anytype, writer: anytype) !void {
        try writer.writeAll(prompt);
        const start_len = repl.buffer.items.len;
        while (true) {
            var byte: u8 = reader.readByte() catch |e| switch (e) {
                error.EndOfStream => if (start_len == repl.buffer.items.len) {
                    try writer.writeAll(std.cstr.line_sep);
                    return error.EndOfStream;
                } else continue,
                else => |err| return err,
            };
            try repl.buffer.append(byte);

            if (byte == '\n') {
                return;
            }

            if (repl.buffer.items.len - start_len == 1024) {
                return error.StreamTooLong;
            }
        }
    }
};
