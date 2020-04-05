const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Node = bog.Node;
const TypeId = bog.Value.TypeId;

// TODO give these numbers once they are more stable
pub const Op = enum(u8) {
    /// A <- B
    Move,

    /// A = COPY(B)
    Copy,

    /// DISCARD(A)
    Discard,

    /// A = CAPTURE(arg1)
    LoadCapture,

    /// CAPTURE(A, arg1) = B
    StoreCapture,

    /// A = PRIMITIVE(arg1)
    /// 0 = ()
    /// 1 = false
    /// 2 = true
    ConstPrimitive,

    /// A = INT(arg1)
    ConstInt8,
    ConstInt32,
    ConstInt64,

    /// A = STRING(arg1)
    ConstString,

    /// A = NUM(arg1)
    ConstNum,

    /// A = B // C
    DivFloor,

    /// A = B / C
    Div,

    /// A = B * C
    Mul,

    /// A = B ** C
    Pow,

    /// A = B % C
    Mod,

    /// A = B + C
    Add,

    /// A = B - C
    Sub,

    /// A = B << C
    LShift,

    /// A = B << C
    RShift,

    /// A = B & C
    BitAnd,

    /// A = B | C
    BitOr,

    /// A = B ^ C
    BitXor,

    /// A = B == C
    Equal,

    /// A = B != C
    NotEqual,

    /// A = B < C
    LessThan,

    /// A = B <= C
    LessThanEqual,

    /// A = B > C
    GreaterThan,

    /// A = B >= C
    GreaterThanEqual,

    /// A = B in C
    In,

    /// A = B and C
    BoolAnd,

    /// A = B or C
    BoolOr,

    /// A = not B
    BoolNot,

    /// A = ~B
    BitNot,

    /// A = -B
    Negate,

    /// IF (B==error) RET B ELSE A = B
    Try,

    /// A = B[C]
    Get,

    /// A[B] = C
    Set,

    /// A = error(B)
    BuildError,

    /// A = (B, B + 1, ... B + N)
    BuildTuple,

    /// A = [B, B + 1, ... B + N]
    BuildList,

    /// A = Fn(arg_count, captures, offset)
    BuildFn,

    /// A = NATIVE(arg1)
    BuildNative,

    /// A = {B: B + 1, B + 2: ... N - 1: N}
    BuildMap,

    /// ip = arg1
    Jump,

    /// if (A) ip = arg1
    JumpTrue,

    /// if (not A) ip = arg1
    JumpFalse,

    /// if (not A is error) ip = arg1
    JumpNotError,

    /// if (A is none) ip = arg1
    JumpNone,

    /// A = B.iterator()
    IterInit,

    /// A = B.next()
    IterNext,

    /// error(A) = B
    UnwrapError,

    /// A = IMPORT(arg1)
    Import,

    /// A = B is TYPEID
    Is,

    /// A = B as TYPEID
    As,

    /// A = B(C, C + 1, ... C + N)
    Call,

    /// 0 = A
    Return,

    /// 0 = ()
    ReturnNone,

    /// TODO better debug info
    LineInfo,

    _,
};

// TODO optimize size of this
pub const RegRef = u16;

pub const Module = struct {
    name: []const u8,
    code: []const u8,
    strings: []const u8,
    entry: u32,
    // debug_info,

    pub fn deinit(module: *Module, alloc: *std.mem.Allocator) void {
        alloc.free(module.name);
        alloc.free(module.code);
        alloc.free(module.strings);
        alloc.destroy(module);
    }

    pub const magic = "\x7fbog";
    pub const header_version = 1;
    pub const bog_version = @bitCast(u32, packed struct {
        _pad: u8 = 0,
        major: u8 = @truncate(u8, bog.version.major),
        minor: u8 = @truncate(u8, bog.version.minor),
        patch: u8 = @truncate(u8, bog.version.patch),
    }{});

    /// all integer values are little-endian
    pub const Header = packed struct {
        magic: [4]u8,
        header_version: u32,
        bog_version: u32,
        strings: u32,
        code: u32,
        entry: u32,
    };

    pub const ReadError = error{
        InvalidMagic,
        InvalidHeader,
        UnsupportedVersion,
    };

    pub fn read(src: []const u8) ReadError!Module {
        if (!mem.startsWith(u8, src, magic))
            return error.InvalidMagic;
        if (src.len < @sizeOf(Header))
            return error.InvalidHeader;

        const header = if (@import("builtin").endian == .Little)
            @ptrCast(*const Header, src.ptr).*
        else
            Header{
                .magic = @ptrCast(*const [4]u8, src.ptr).*,
                .header_version = mem.readIntLittle(u32, @ptrCast(*const [4]u8, src.ptr + 4)),
                .bog_version = mem.readIntLittle(u32, @ptrCast(*const [4]u8, src.ptr + 8)),
                .strings = mem.readIntLittle(u32, @ptrCast(*const [4]u8, src.ptr + 12)),
                .code = mem.readIntLittle(u32, @ptrCast(*const [4]u8, src.ptr + 16)),
                .entry = mem.readIntLittle(u32, @ptrCast(*const [4]u8, src.ptr + 20)),
            };

        // strings must come before code
        if (header.strings > header.code)
            return error.InvalidHeader;

        if (src.len < header.code)
            return error.InvalidHeader;

        return Module{
            .name = "",
            .strings = src[header.strings..header.code],
            .code = src[header.code..],
            // entry is offset to to the beginning of code
            .entry = header.entry - header.code,
        };
    }

    pub fn write(module: Module, stream: var) @TypeOf(stream).Error!void {
        try stream.writeAll(magic);
        try stream.writeIntLittle(u32, header_version);
        try stream.writeIntLittle(u32, bog_version);

        // strings come immediately after header
        const strings_offset = @intCast(u32, @sizeOf(Header));
        try stream.writeIntLittle(u32, strings_offset);

        // code comes immediately after strings
        const code_offset = strings_offset + @intCast(u32, module.strings.len);
        try stream.writeIntLittle(u32, code_offset);

        // entry is offset to the beginning of the code
        const entry_offset = code_offset + module.entry;
        try stream.writeIntLittle(u32, entry_offset);

        // write strings
        try stream.writeAll(module.strings);

        // write code
        try stream.writeAll(module.code);
    }

    pub fn dump(module: Module, allocator: *Allocator, stream: var) (@TypeOf(stream).Error || Allocator.Error)!void {
        var arena_allocator = std.heap.ArenaAllocator.init(allocator);
        defer arena_allocator.deinit();
        const arena = &arena_allocator.allocator;

        var jumps = try module.mapJumpTargets(arena);
        var ip: usize = 0;
        while (ip < module.code.len) {
            if (ip == module.entry) {
                try stream.writeAll("\nentry:");
            }
            if (jumps.getValue(ip)) |label| {
                try stream.print("\n{}:", .{label});
            }
            const this_offset = ip;
            const op = module.getArg(Op, &ip);
            if (op != .LineInfo) {
                try stream.print("\n {: <5} {} ", .{ this_offset, @tagName(op) });
            }
            switch (op) {
                // A i8
                .ConstPrimitive, .ConstInt8 => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(i8, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A u32
                .JumpTrue, .JumpFalse, .JumpNotError, .JumpNone => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(u32, &ip);
                    const label = jumps.getValue(ip + arg_2) orelse unreachable;
                    try stream.print(" #{} {}\n", .{ arg_1, label });
                },

                // i32
                .Jump => {
                    const arg_1 = module.getArg(i32, &ip);
                    const label = jumps.getValue(@intCast(usize, @intCast(isize, ip) + arg_1)) orelse unreachable;
                    try stream.print(" {}\n", .{label});
                },

                // A = Fn(arg_count, captures, offset)
                .BuildFn => {
                    const res = module.getArg(RegRef, &ip);
                    const arg_count = module.getArg(u8, &ip);
                    const captures = module.getArg(u8, &ip);
                    const offset = module.getArg(u32, &ip);
                    const label = jumps.getValue(offset) orelse unreachable;
                    try stream.print(" #{} {}({})[{}]\n", .{ res, label, arg_count, captures });
                },

                // A i32
                .ConstInt32 => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(i32, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A i64
                .ConstInt64 => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(i64, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A STRING(arg1)
                .Import, .BuildNative, .ConstString => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const offset = module.getArg(u32, &ip);

                    const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
                    const slice = module.strings[offset + @sizeOf(u32) ..][0..len];
                    try stream.print(" #{} \"{}\"\n", .{ arg_1, slice });
                },

                // A f64
                .ConstNum => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(f64, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A B C
                .DivFloor,
                .Div,
                .Mul,
                .Pow,
                .Mod,
                .Add,
                .Sub,
                .LShift,
                .RShift,
                .BitAnd,
                .BitOr,
                .BitXor,
                .Equal,
                .NotEqual,
                .LessThan,
                .LessThanEqual,
                .GreaterThan,
                .GreaterThanEqual,
                .In,
                .BoolAnd,
                .BoolOr,
                .Get,
                .Set,
                => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(RegRef, &ip);
                    try stream.print(" #{} #{} #{}\n", .{ arg_1, arg_2, arg_3 });
                },

                // A B
                .Move,
                .Copy,
                .BoolNot,
                .BitNot,
                .Negate,
                .Try,
                .BuildError,
                .UnwrapError,
                .IterInit,
                .IterNext,
                => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    try stream.print(" #{} #{}\n", .{ arg_1, arg_2 });
                },

                // A B u8
                .StoreCapture => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(u8, &ip);
                    try stream.print(" #{}[{}] = #{}\n", .{ arg_1, arg_3, arg_2 });
                },

                // A u8
                .LoadCapture => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(u8, &ip);
                    try stream.print(" #{} = [{}]\n", .{ arg_1, arg_2 });
                },

                // A = (B, B + 1, ... B + N)
                .BuildTuple,
                .BuildList,
                .BuildMap,
                => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(u16, &ip);
                    try stream.print(" #{} #{} arg_count:{}\n", .{ arg_1, arg_2, arg_3 });
                },

                // A = B(C, C + 1, ... C + N)
                .Call => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(RegRef, &ip);
                    const arg_4 = module.getArg(u16, &ip);
                    try stream.print(" #{} #{} #{} arg_count:{}\n", .{ arg_1, arg_2, arg_3, arg_4 });
                },

                // u32
                .LineInfo => {
                    const arg_1 = module.getArg(u32, &ip);
                },

                // A B TYPEID
                .Is, .As => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(TypeId, &ip);
                    try stream.print(" #{} #{} id:{}\n", .{ arg_1, arg_2, @tagName(arg_3) });
                },

                // A
                .Discard, .Return => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    try stream.print(" #{}\n", .{arg_1});
                },

                .ReturnNone => try stream.writeByte('\n'),
                _ => unreachable,
            }
        }
    }

    pub const JumpMap = std.AutoHashMap(usize, []const u8);

    pub fn mapJumpTargets(module: Module, arena: *Allocator) Allocator.Error!JumpMap {
        var map = JumpMap.init(arena);
        var ip: usize = 0;
        var mangle: u32 = 0;
        while (ip < module.code.len) {
            const op = module.getArg(Op, &ip);
            switch (op) {
                // A i8
                .LoadCapture, .ConstPrimitive, .ConstInt8 => ip += @sizeOf(RegRef) + @sizeOf(i8),

                // A u32
                .JumpTrue, .JumpFalse, .JumpNotError, .JumpNone, .Jump => {
                    var jump_target: usize = undefined;
                    if (op == .Jump) {
                        const arg = module.getArg(i32, &ip);
                        jump_target = @intCast(usize, @intCast(isize, ip) + arg);
                    } else {
                        ip += @sizeOf(RegRef);
                        const arg = module.getArg(u32, &ip);
                        jump_target = ip + arg;
                    }
                    if (map.getValue(jump_target)) |_| continue;

                    _ = try map.put(jump_target, try std.fmt.allocPrint(arena, "{}_{}", .{ @tagName(op), mangle }));
                    mangle += 1;
                },

                // A = Fn(arg_count, captures, offset)
                .BuildFn => {
                    ip += @sizeOf(RegRef) + @sizeOf(u8) * 2;
                    const offset = module.getArg(u32, &ip);

                    _ = try map.put(offset, try std.fmt.allocPrint(arena, "function_{}", .{mangle}));
                    mangle += 1;
                },

                // A i32
                .ConstInt32 => ip += @sizeOf(RegRef) + @sizeOf(i32),

                // A i64
                .ConstInt64 => ip += @sizeOf(RegRef) + @sizeOf(i64),

                // A STRING(arg1)
                .Import, .BuildNative, .ConstString => ip += @sizeOf(RegRef) + @sizeOf(u32),

                // A f64
                .ConstNum => ip += @sizeOf(RegRef) + @sizeOf(f64),

                // A B C
                .DivFloor,
                .Div,
                .Mul,
                .Pow,
                .Mod,
                .Add,
                .Sub,
                .LShift,
                .RShift,
                .BitAnd,
                .BitOr,
                .BitXor,
                .Equal,
                .NotEqual,
                .LessThan,
                .LessThanEqual,
                .GreaterThan,
                .GreaterThanEqual,
                .In,
                .BoolAnd,
                .BoolOr,
                .Get,
                .Set,
                => ip += @sizeOf(RegRef) * 3,

                // A B
                .Move,
                .Copy,
                .BoolNot,
                .BitNot,
                .Negate,
                .Try,
                .BuildError,
                .UnwrapError,
                .IterInit,
                .IterNext,
                => ip += @sizeOf(RegRef) * 2,

                // A B u8
                .StoreCapture => ip += @sizeOf(RegRef) * 2 + @sizeOf(u8),

                // A = (B, B + 1, ... B + N)
                .BuildTuple, .BuildList, .BuildMap => ip += @sizeOf(RegRef) * 2 + @sizeOf(u16),

                // A = B(C, C + 1, ... C + N)
                .Call => ip += @sizeOf(RegRef) * 3 + @sizeOf(u16),

                // u32
                .LineInfo => ip += @sizeOf(u32),

                // A B TYPEID
                .Is, .As => ip += @sizeOf(RegRef) * 2 + @sizeOf(TypeId),

                // A
                .Discard, .Return => ip += @sizeOf(RegRef),

                .ReturnNone => {},
                _ => unreachable,
            }
        }
        return map;
    }

    fn getArg(module: Module, comptime T: type, ip: *usize) T {
        const val = @ptrCast(*align(1) const T, module.code[ip.*..].ptr).*;
        ip.* += @sizeOf(T);
        return val;
    }
};
