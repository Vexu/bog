const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Node = bog.Node;
const Type = bog.Type;

// TODO give these numbers once they are more stable
pub const Op = enum(u8) {
    move_double,
    copy_double,

    discard_single,

    /// res = CAPTURE(arg)
    load_capture_double,

    /// CAPTURE(res, lhs) = rhs
    store_capture_triple,

    load_this_single,

    const_primitive,

    /// A = INT(arg1)
    const_int,

    /// A = NUM(arg1, arg2)
    const_num,
    const_string_off,
    import_off,

    div_floor_triple,
    div_triple,
    mul_triple,
    pow_triple,
    mod_triple,
    add_triple,
    sub_triple,

    l_shift_triple,
    r_shift_triple,
    bit_and_triple,
    bit_or_triple,
    bit_xor_triple,
    bit_not_double,

    equal_triple,
    not_equal_triple,
    less_than_triple,
    less_than_equal_triple,
    greater_than_triple,
    greater_than_equal_triple,
    in_triple,

    bool_and_triple,
    bool_or_triple,
    bool_not_double,

    negate_double,
    iter_init_double,
    iter_next_double,
    unwrap_error_double,
    /// IF (B==error) RET B ELSE A = B
    try_double,

    /// res = lhs[rhs]
    get_triple,

    /// res[lhs] = rhs
    set_triple,
    build_error_double,
    build_tuple_off,
    build_list_off,
    build_map_off,
    build_native_off,
    build_func,

    /// ip = arg1
    jump,

    /// if (A) ip = arg1
    jump_true,

    /// if (not A) ip = arg1
    jump_false,

    /// if (not A is error) ip = arg1
    jump_not_error,

    /// if (A is none) ip = arg1
    jump_none,

    is_type_id,
    as_type_id,

    call,

    /// RETURN(arg)
    return_single,

    /// RETURN(())
    return_none,

    /// TODO better debug info
    line_info,

    _,
};

/// All integers are little endian
pub const Instruction = packed union {
    bare: u32,
    bare_signed: i32,
    op: packed struct {
        op: Op,
        __pad1: u8 = 0,
        __pad2: u8 = 0,
        __pad3: u8 = 0,
    },
    single: packed struct {
        op: Op,
        arg: RegRef,
        __pad: u16 = 0,
    },
    double: packed struct {
        op: Op,
        res: RegRef,
        arg: RegRef,
        __pad: u8 = 0,
    },
    triple: packed struct {
        op: Op,
        res: RegRef,
        lhs: RegRef,
        rhs: RegRef,
    },
    type_id: packed struct {
        op: Op,
        res: RegRef,
        arg: RegRef,
        type_id: Type,
    },
    off: packed struct {
        op: Op,
        res: RegRef,
        off: u16,

        pub inline fn isArg(self: @This()) bool {
            return self.off == 0xFFFF;
        }
    },
    primitive: packed struct {
        op: Op = .const_primitive,
        res: u8,
        kind: packed enum(u8) {
            none = 0,
            True = 1,
            False = 2,
            _,
        },
        __pad: u8 = 0,
    },
    int: packed struct {
        op: Op = .const_int,
        res: RegRef,
        /// if true arg is given as two instructions
        long: bool,
        arg: i15,
    },
    func: packed struct {
        op: Op = .build_func,
        res: RegRef,
        arg_count: u8,
        capture_count: u8,

        // followed by an offset
    },
    jump: packed struct {
        op: Op = .jump,
        kind: packed enum(u8) {
            immediate = 0,
            arg = 1,
            _,
        },
        off: i16,
    },
    jump_arg: packed struct {
        op: Op,
        arg: RegRef,
        off: u16,

        pub inline fn isArg(self: @This()) bool {
            return self.off == 0xFFFF;
        }
    },
    call: packed struct {
        /// A = B(C, C + 1, ... C + N)
        op: Op = .call,
        res: RegRef,
        func: RegRef,
        first: RegRef,

        // followed by a bare instruction with arg count
        // TODO max 32 args, reduce waste of space
    },

    comptime {
        std.debug.assert(@sizeOf(Instruction) == @sizeOf(u32));
    }
};

pub const RegRef = u8;

/// A self contained Bog module with its code and strings.
pub const Module = struct {
    name: []const u8,
    code: []const Instruction,
    strings: []const u8,
    entry: u32,
    // debug_info,

    pub fn deinit(module: *Module, alloc: *std.mem.Allocator) void {
        alloc.free(module.name);
        alloc.free(module.code);
        alloc.free(module.strings);
        alloc.destroy(module);
    }

    /// The magic number for a Bog bytecode file.
    pub const magic = "\x7fbog";

    /// Current bytecode version.
    pub const bytecode_version = 2;

    /// The header of a Bog bytecode file.
    /// All integer values are little-endian.
    pub const Header = packed struct {
        /// A magic number, must be `\x7fbog`.
        magic: [4]u8,

        /// Version of this header.
        version: u32,

        /// Offset to the string table.
        strings: u32,

        /// Offset to the bytecode.
        code: u32,

        /// Offset to the module entry point.
        entry: u32,
    };

    pub const ReadError = error{
        /// Source did not start with a correct magic number.
        InvalidMagic,

        /// Header was malformed.
        InvalidHeader,

        /// This version of Bog cannot execute this bytecode.
        UnsupportedVersion,

        /// Code sections length is not a multiple of 4.
        MalformedCode,
    };

    /// Reads a module from memory.
    pub fn read(src: []const u8) ReadError!Module {
        if (!mem.startsWith(u8, src, magic))
            return error.InvalidMagic;
        if (src.len < @sizeOf(Header))
            return error.InvalidHeader;

        const header = if (@import("builtin").endian == .Little)
            @ptrCast(*const Header, src.ptr).*
        else
            Header{
                .magic = src[0..4].*,
                .version = mem.readIntLittle(u32, src[4..8]),
                .strings = mem.readIntLittle(u32, src[8..12]),
                .code = mem.readIntLittle(u32, src[12..16]),
                .entry = mem.readIntLittle(u32, src[16..20]),
            };

        if (header.version != bytecode_version)
            return error.UnsupportedVersion;

        // strings must come before code
        if (header.strings > header.code)
            return error.InvalidHeader;

        if (src.len < header.code)
            return error.InvalidHeader;

        const code = src[header.code..];
        if (code.len % @sizeOf(Instruction) != 0)
            return error.InvalidHeader;

        return Module{
            .name = "",
            .strings = src[header.strings..header.code],
            .code = mem.bytesAsSlice(Instruction, code),
            // entry is offset to to the beginning of code
            .entry = header.entry - header.code,
        };
    }

    /// Writes a module to a stream.
    pub fn write(module: Module, stream: var) @TypeOf(stream).Error!void {
        try stream.writeAll(magic);
        try stream.writeIntLittle(u32, bytecode_version);

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
        try stream.writeAll(mem.sliceAsBytes(module.code));
    }

    /// Pretty prints debug info about the module.
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
                    const arg_3 = module.getArg(Type, &ip);
                    try stream.print(" #{} #{} id:{}\n", .{ arg_1, arg_2, @tagName(arg_3) });
                },

                // A
                .Discard, .Return, .LoadThis => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    try stream.print(" #{}\n", .{arg_1});
                },

                .ReturnNone => try stream.writeByte('\n'),
                _ => unreachable,
            }
        }
    }

    const JumpMap = std.AutoHashMap(usize, []const u8);

    fn mapJumpTargets(module: Module, arena: *Allocator) Allocator.Error!JumpMap {
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
                .Is, .As => ip += @sizeOf(RegRef) * 2 + @sizeOf(Type),

                // A
                .Discard, .Return, .LoadThis => ip += @sizeOf(RegRef),

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
