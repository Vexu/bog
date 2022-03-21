const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Node = bog.Node;
const Type = bog.Type;

const Bytecode = @This();

name: []const u8,
code: Inst.List.Slice,
extra: []const u32,

main: []const Ref,

strings: []const u8,
debug_info: DebugInfo,

pub const Ref = u32;

/// All integers are little endian
pub const Inst = struct {
    op: Op,
    data: Data,

    pub const List = std.MultiArrayList(Inst);

    pub const Op = enum(u8) {
        // literal construction

        /// null, true, false
        primitive,
        /// integer literal
        int,
        /// number literal
        num,
        // string literal
        str,

        // aggregate construction

        // use Data.aggregate
        build_tuple,
        build_list,
        build_map,
        // use Data.un
        build_error,
        build_tagged,
        // uses Data.func
        build_func,
        // uses Data.range
        build_range,
        // uses Data.range_step
        build_range_step,

        // import, uses Data.str
        import,

        // res = copy(operand)
        copy_un,
        // lhs = copy(rhs)
        copy,
        // lhs = rhs
        move,
        // res = CAPTURE(operand)
        load_global,
        // res = CAPTURE(operand)
        load_capture,
        // CAPTURE(lhs) = rhs
        store_capture,

        // binary operators

        // numeric
        div_floor,
        div,
        mul,
        pow,
        mod,
        add,
        sub,

        // bitwise
        l_shift,
        r_shift,
        bit_and,
        bit_or,
        bit_xor,

        // comparisons
        equal,
        not_equal,
        less_than,
        less_than_equal,
        greater_than,
        greater_than_equal,

        /// lhs in rhs
        in,

        /// container(lhs).append(rhs)
        append,

        // simple cast
        as,
        // simple type check
        is,

        // unary operations
        negate,
        bool_not,
        bit_not,

        // uses Data.un, error(A) => A
        unwrap_error,
        // uses Data.str, @tag(A) => A
        unwrap_tagged,

        // use Data.bin
        get,
        // returns null if no
        get_or_null,
        set,

        // uses Data.jump
        jump,
        // use Data.jump_condition
        jump_if_true,
        jump_if_false,
        jump_if_not_error,
        jump_if_null,
        jump_if_error,

        // use Data.un
        iter_init,
        iter_next,

        // uses Data.call
        call,
        // uses Data.bin, lhs(rhs)
        call_one,
        // uses Data.un, operand()
        call_zero,

        // use Data.un
        ret,
        ret_null,

        _,
    };

    pub const Data = union {
        none: void,
        primitive: enum {
            @"null",
            @"true",
            @"false",
        },
        int: i64,
        num: f64,
        str: struct {
            offset: u32,
            len: u32,
        },
        aggregate: struct {
            extra: u32,
            len: u32,
        },
        func: struct {
            // extra[0] = Data.FnInfo
            extra: u32,
            len: u32,
        },
        range: struct {
            start: Ref,
            end: Ref,
        },
        range_step: struct {
            start: Ref,
            /// end = extra[extra]
            /// step = extra[extra + 1]
            extra: u32,
        },
        bin: struct {
            lhs: Ref,
            rhs: Ref,
        },
        bin_ty: struct {
            operand: Ref,
            ty: Type,
        },
        un: Ref,
        jump: u32,
        jump_condition: struct {
            operand: Ref,
            offset: u32,
        },
        call: struct {
            callee: Ref,
            extra: u24,
            count: u8,
        },

        pub const FnInfo = packed struct {
            args: u8,
            captures: u24,
        };
    };

    comptime {
        if (!std.debug.runtime_safety) std.debug.assert(@sizeOf(Data) == @sizeOf(u64));
    }
};

pub const DebugInfo = struct {
    file_path: []const u8,
    line_parts: []const LinePart,

    pub const LinePart = struct {
        offset: u8,
        line: u8,
        col: u8,
    };

    pub const LineCol = struct {
        line: u32,
        col: u32,
    };

    pub fn getLineForOffset(d: DebugInfo, offset: u32) LineCol {
        var cur: LineCol = .{ .line = 0, .col = 0 };
        var cur_offset: u32 = 0;
        for (d.line_parts) |part| {
            if (cur_offset >= offset) break;
            cur.line += part.line;
            if (part.col == std.math.maxInt(u8)) {
                cur.col = 1;
            } else {
                cur.col += part.col;
            }
        }
        return cur;
    }
};
