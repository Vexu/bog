const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bog = @import("bog.zig");
const Vm = bog.Vm;
const Errors = bog.Errors;

pub fn run(gpa: Allocator, reader: anytype, writer: anytype) !void {
    var vm = Vm.init(gpa, .{ .repl = true, .import_files = true });
    defer vm.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var module = bog.Compiler.Fn{
        .code = bog.Compiler.Code.init(gpa),
        .captures = undefined,
    };
    defer module.code.deinit();

    var repl = try Repl.init(gpa, &vm, &module, &arena);
    defer repl.deinit();

    while (true) {
        defer {
            arena.deinit();
            arena = std.heap.ArenaAllocator.init(gpa);
        }
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
    vm: *Vm,
    buffer: ArrayList(u8),
    tokenizer: bog.Tokenizer,
    compiler: bog.Compiler,
    tok_index: bog.Token.Index = 0,

    const tokenize = @import("tokenizer.zig").tokenizeRepl;
    const parse = @import("parser.zig").parseRepl;
    const compile = @import("compiler.zig").compileRepl;

    fn init(gpa: Allocator, vm: *Vm, mod: *bog.Compiler.Fn, arena: *std.heap.ArenaAllocator) !Repl {
        const buffer = try ArrayList(u8).initCapacity(gpa, std.mem.page_size);
        errdefer buffer.deinit();
        try vm.addStd();

        var scopes = std.ArrayList(bog.Compiler.Scope).init(gpa);
        errdefer scopes.deinit();

        try scopes.append(.{ .module = mod });
        try scopes.append(.{
            .symbol = .{
                .name = "ans",
                .reg = mod.regAlloc() catch unreachable,
                .mut = false,
            },
        });

        return Repl{
            .compiler = .{
                .source = "",
                .tokens = &[0]bog.Token{},
                .errors = &vm.errors,
                .gpa = gpa,
                .scopes = scopes,
                .func = mod,
                .arena = arena.allocator(),
                .module_code = bog.Compiler.Code.init(gpa),
                .strings = std.ArrayList(u8).init(gpa),
                .string_interner = std.StringHashMap(u32).init(gpa),
            },
            .vm = vm,
            .buffer = buffer,
            .tokenizer = .{
                .tokens = bog.Token.List.init(gpa),
                .errors = &vm.errors,
                .it = .{
                    .i = 0,
                    .bytes = "",
                },
                .repl = true,
            },
        };
    }

    fn deinit(repl: *Repl) void {
        repl.buffer.deinit();
        repl.tokenizer.tokens.deinit();
        repl.compiler.deinit();
    }

    fn handleLine(repl: *Repl, reader: anytype, writer: anytype) !void {
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
