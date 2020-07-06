const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Node = bog.Node;
const Type = bog.Type;

/// Id's of all the bytecode instructions
pub const Op = enum(u8) {
    move_double = 0x10,
    copy_double = 0x11,

    discard_single = 0x12,

    /// res = CAPTURE(arg)
    load_capture_double = 0x13,

    /// CAPTURE(res, lhs) = rhs
    store_capture_triple = 0x14,

    load_this_single = 0x15,

    const_primitive = 0x20,

    /// A = INT(arg1)
    const_int = 0x21,

    /// A = NUM(arg1, arg2)
    const_num = 0x22,
    const_string_off = 0x23,
    import_off = 0x24,

    div_floor_triple = 0x30,
    div_triple = 0x31,
    mul_triple = 0x32,
    pow_triple = 0x33,
    mod_triple = 0x34,
    add_triple = 0x35,
    sub_triple = 0x36,

    l_shift_triple = 0x37,
    r_shift_triple = 0x38,
    bit_and_triple = 0x39,
    bit_or_triple = 0x3A,
    bit_xor_triple = 0x3B,
    bit_not_double = 0x3C,

    equal_triple = 0x40,
    not_equal_triple = 0x41,
    less_than_triple = 0x42,
    less_than_equal_triple = 0x43,
    greater_than_triple = 0x44,
    greater_than_equal_triple = 0x45,
    in_triple = 0x46,

    is_type_id = 0x47,
    as_type_id = 0x48,

    bool_and_triple = 0x4A,
    bool_or_triple = 0x4B,
    bool_not_double = 0x4C,

    negate_double = 0x4D,
    /// IF (B==error) RET B ELSE A = B
    try_double = 0x4E,

    unwrap_error_double = 0x50,
    unwrap_tagged = 0x51,
    /// res = lhs[rhs]
    get_triple = 0x52,
    /// res[lhs] = rhs
    set_triple = 0x53,

    build_error_double = 0xB0,
    build_tuple_off = 0xB1,
    build_list_off = 0xB2,
    build_map_off = 0xB3,
    build_func = 0xB4,
    build_tagged = 0xB5,

    /// ip = arg1
    jump = 0xA0,
    /// if (A) ip = arg1
    jump_true = 0xA1,
    /// if (not A) ip = arg1
    jump_false = 0xA2,
    /// if (not A is error) ip = arg1
    jump_not_error = 0xA3,
    /// if (A is none) ip = arg1
    jump_none = 0xA4,
    iter_init_double = 0xA5,
    /// This is fused with a jump_none as an optimization
    iter_next_double = 0xA6,

    call = 0x90,
    /// RETURN(arg)
    return_single = 0x91,
    /// RETURN(())
    return_none = 0x92,

    // unstable instructions

    /// TODO better debug info
    line_info,

    append_double,

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
    tagged: packed struct {
        op: Op = .build_tagged,
        res: RegRef,
        arg: RegRef,
        kind: packed enum(u8) {
            none = 0,
            some = 1,
            _,
        },
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
        op: Op,
        arg: RegRef,
        __pad: u16 = 0,

        // followed by an offset
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
    pub const bytecode_version = 5;
    pub const last_compatible = 5;

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

        if (header.version < last_compatible)
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
            if (jumps.get(ip)) |label| {
                try stream.print("\n{}:", .{label});
            }
            const inst = module.code[ip];
            ip += 1;
            if (inst.op.op != .line_info) {
                try stream.print("\n {: <5} {: <20} ", .{ ip, @tagName(inst.op.op) });
            }
            switch (inst.op.op) {
                .const_primitive => {
                    try stream.print("{} <- ({})\n", .{ inst.primitive.res, @tagName(inst.primitive.kind) });
                },
                .const_int => {
                    const val = if (inst.int.long) blk: {
                        ip += 2;
                        break :blk module.code[ip - 2].bare_signed;
                    } else inst.int.arg;
                    try stream.print("{} <- ({})\n", .{ inst.int.res, val });
                },
                .const_num => {
                    try stream.print("{} <- ({d})\n", .{ inst.single.arg, @ptrCast(*align(@alignOf(Instruction)) const f64, &module.code[ip]).* });
                    ip += 2;
                },
                .const_string_off, .import_off => {
                    const offset = if (inst.off.isArg()) blk: {
                        ip += 1;
                        break :blk module.code[ip - 1].bare;
                    } else inst.off.off;

                    const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
                    const slice = module.strings[offset + @sizeOf(u32) ..][0..len];
                    try stream.print("{} <- \"{}\"\n", .{ inst.off.res, slice });
                },

                .build_tagged => {
                    const offset = module.code[ip].bare;
                    ip += 1;

                    const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
                    const slice = module.strings[offset + @sizeOf(u32) ..][0..len];
                    if (inst.tagged.kind == .none)
                        try stream.print("{} <- @{}\n", .{ inst.off.res, slice })
                    else
                        try stream.print("{} <- @{}({})\n", .{ inst.off.res, slice, inst.tagged.arg });
                },

                .jump, .jump_false, .jump_true, .jump_none, .jump_not_error => {
                    const jump_target = if (inst.jump.op == .jump)
                        @intCast(usize, @intCast(isize, ip) + module.code[ip].bare_signed)
                    else
                        ip + module.code[ip].bare;
                    ip += 1;
                    const label = jumps.get(jump_target) orelse unreachable;

                    if (inst.jump.op == .jump)
                        try stream.print("to {}\n", .{label})
                    else
                        try stream.print("to {}, cond {}\n", .{ label, inst.jump.arg });
                },

                .build_func => {
                    const offset = module.code[ip].bare;
                    ip += 1;
                    const label = jumps.get(offset) orelse unreachable;
                    try stream.print("{} <- {}({})[{}]\n", .{ inst.func.res, label, inst.func.arg_count, inst.func.capture_count });
                },

                .div_floor_triple,
                .div_triple,
                .mul_triple,
                .pow_triple,
                .mod_triple,
                .add_triple,
                .sub_triple,
                .l_shift_triple,
                .r_shift_triple,
                .bit_and_triple,
                .bit_or_triple,
                .bit_xor_triple,
                .equal_triple,
                .not_equal_triple,
                .less_than_triple,
                .less_than_equal_triple,
                .greater_than_triple,
                .greater_than_equal_triple,
                .in_triple,
                .bool_and_triple,
                .bool_or_triple,
                => {
                    try stream.print("{} <- {} {} {}\n", .{ inst.triple.res, inst.triple.lhs, opToStr(inst.triple.op), inst.triple.rhs });
                },
                .get_triple => {
                    try stream.print("{} <- {}[{}]\n", .{ inst.triple.res, inst.triple.lhs, inst.triple.rhs });
                },
                .set_triple => {
                    try stream.print("{}[{}] <- {}\n", .{ inst.triple.res, inst.triple.lhs, inst.triple.rhs });
                },

                .iter_next_double => {
                    const jump_target = ip + module.code[ip].bare;
                    ip += 1;
                    const label = jumps.get(jump_target) orelse unreachable;
                    try stream.print("{} <- {}, jump_none to {}\n", .{ inst.double.res, inst.double.arg, label });
                },
                .build_error_double, .unwrap_error_double, .iter_init_double, .move_double, .copy_double, .append_double => {
                    try stream.print("{} <- {}\n", .{ inst.double.res, inst.double.arg });
                },
                .unwrap_tagged => {
                    const offset = module.code[ip].bare;
                    ip += 1;

                    const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
                    const slice = module.strings[offset + @sizeOf(u32) ..][0..len];
                    try stream.print("{} <- @{}({})\n", .{ inst.double.res, slice, inst.double.arg });
                },
                .bool_not_double, .bit_not_double, .negate_double, .try_double => {
                    try stream.print("{} <- {} {}\n", .{ inst.double.res, opToStr(inst.double.op), inst.double.arg });
                },

                .store_capture_triple => {
                    try stream.print("{}[{}] <- {}\n", .{ inst.triple.res, inst.triple.rhs, inst.triple.lhs });
                },
                .load_capture_double => {
                    try stream.print("{} <- [{}]\n", .{ inst.double.res, inst.double.arg });
                },
                .build_tuple_off, .build_list_off, .build_map_off => {
                    const size = if (inst.off.isArg()) blk: {
                        ip += 1;
                        break :blk module.code[ip - 1].bare;
                    } else inst.off.off;
                    try stream.print("{} <- size:{}\n", .{ inst.off.res, size });
                },

                .call => {
                    var first = inst.call.first;
                    const last = module.code[ip].bare + first;
                    try stream.print("{} <- {}(", .{ inst.call.res, inst.call.func });
                    var comma = false;
                    while (first < last) : (first += 1) {
                        if (comma) {
                            try stream.writeAll(", ");
                        } else comma = true;
                        try stream.print("{}", .{first});
                    }
                    try stream.writeAll(")\n");
                    ip += 1;
                },

                .line_info => ip += 1,
                .is_type_id, .as_type_id => {
                    try stream.print("{} <- {} {} {}\n", .{ inst.type_id.res, inst.type_id.arg, opToStr(inst.op.op), @tagName(inst.type_id.type_id) });
                },
                .discard_single, .return_single, .load_this_single => {
                    try stream.print("{}\n", .{inst.single.arg});
                },
                .return_none => try stream.writeByte('\n'),
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
            const inst = module.code[ip];
            ip += 1;
            switch (inst.op.op) {
                .jump, .jump_false, .jump_true, .jump_none, .jump_not_error, .iter_next_double => {
                    const jump_target = if (inst.jump.op == .jump)
                        @intCast(usize, @intCast(isize, ip) + module.code[ip].bare_signed)
                    else
                        ip + module.code[ip].bare;
                    ip += 1;
                    if (map.get(jump_target)) |_| continue;

                    _ = try map.put(jump_target, try std.fmt.allocPrint(arena, "{}_{}", .{ @tagName(inst.op.op), mangle }));
                    mangle += 1;
                },

                .build_func => {
                    _ = try map.put(module.code[ip].bare, try std.fmt.allocPrint(arena, "function_{}", .{mangle}));
                    mangle += 1;
                },

                .const_int => if (inst.int.long) {
                    ip += 2;
                },
                .const_num => ip += 2,

                .import_off,
                .const_string_off,
                .build_tuple_off,
                .build_list_off,
                .build_map_off,
                => if (inst.off.isArg()) {
                    ip += 1;
                },

                .build_tagged, .line_info, .call => ip += 1,
                else => {},
            }
        }
        return map;
    }

    fn opToStr(op: Op) []const u8 {
        return switch (op) {
            .div_floor_triple => "//",
            .div_triple => "/",
            .mul_triple => "*",
            .pow_triple => "**",
            .mod_triple => "%",
            .add_triple => "+",
            .sub_triple, .negate_double => "-",
            .l_shift_triple => "<<",
            .r_shift_triple => ">>",
            .bit_and_triple => "&",
            .bit_or_triple => "|",
            .bit_xor_triple => "^",
            .equal_triple => "==",
            .not_equal_triple => "!=",
            .less_than_triple => "<",
            .less_than_equal_triple => "<=",
            .greater_than_triple => ">",
            .greater_than_equal_triple => ">=",
            .in_triple => "in",
            .bool_and_triple => "and",
            .bool_or_triple => "or",

            .bool_not_double => "not",
            .bit_not_double => "~",
            .try_double => "try",

            .is_type_id => "is",
            .as_type_id => "as",
            else => unreachable,
        };
    }
};
