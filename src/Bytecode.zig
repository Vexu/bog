const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Node = bog.Node;
const Type = bog.Type;

const Bytecode = @This();

name: []const u8,
code: Inst.List.Slice,
extra: []const Ref,

main: []const Ref,

strings: []const u8,
debug_info: DebugInfo,

pub const max_params = 32;

pub const Index = usize;
pub const Ref = enum(u32) {
    _,
    pub fn format(ref: Ref, _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeByte('%');
        try std.fmt.formatInt(@enumToInt(ref), 10, .lower, options, writer);
    }
};

pub inline fn indexToRef(i: Index) Ref {
    return @intToEnum(Ref, i + max_params);
}
pub inline fn refToIndex(r: Ref) Index {
    return @enumToInt(r) - max_params;
}

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

        // use Data.extra
        build_tuple,
        build_list,
        build_map,
        // use Data.un
        build_error,
        build_error_null,
        /// uses Data.extra with
        /// extra[0] == operand
        /// extra[1] == str.offset
        build_tagged,
        /// uses Data.str
        build_tagged_null,
        /// uses Data.func with
        // extra[0] == Data.FnInfo
        build_func,
        /// uses Data.bin
        build_range,
        /// uses Data.range
        build_range_step,

        // import, uses Data.str
        import,

        // discards Data.un, complains if it's an error
        discard,

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
        // CAPTURE(lhs func).append(rhs)
        store_capture,
        // res = THIS
        load_this,

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
        unwrap_tagged_or_null,

        // uses Data.bin
        // returns null if lhs is not tuple/list or if its len is not equal to @enumToInt(rhs)
        list_len,
        tuple_len,
        // same as above but return error on false
        assert_list_len,
        assert_tuple_len,

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
        jump_if_null,
        jump_if_error,
        /// if operand is not an error jumps,
        /// otherwise unwraps the error
        unwrap_error_or_jump,

        // use Data.un
        iter_init,
        // use Data.jump_condition
        iter_next,

        /// uses Data.extra with 
        /// extra[0] == callee
        call,
        /// uses Data.bin, lhs(rhs)
        call_one,
        /// uses Data.un, operand()
        call_zero,

        // use Data.un
        ret,
        ret_null,
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
        extra: struct {
            extra: u32,
            len: u32,
        },
        range: struct {
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

        pub const FnInfo = packed struct {
            captures: u24,
            args: u8,
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

pub fn dump(b: *Bytecode, body: []const Ref) void {
    b.dumpExtra(body, 0);
}

fn dumpExtra(b: *Bytecode, body: []const Ref, level: u32) void {
    const ops = b.code.items(.op);
    const data = b.code.items(.data);
    for (body) |ref, inst| {
        const i = refToIndex(ref);
        std.debug.print("{d:[3]} {} = {s} ", .{ inst, ref, @tagName(ops[i]), level });
        switch (ops[i]) {
            .primitive => std.debug.print("{s}\n", .{@tagName(data[i].primitive)}),
            .int => std.debug.print("{d}\n", .{data[i].int}),
            .num => std.debug.print("{d}\n", .{data[i].num}),
            .import, .str, .unwrap_tagged, .unwrap_tagged_or_null => {
                const str = b.strings[data[i].str.offset..][0..data[i].str.len];
                std.debug.print("{s}\n", .{str});
            },
            .build_tuple => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                std.debug.print("(", .{});
                dumpList(extra);
                std.debug.print(")\n", .{});
            },
            .build_list => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                std.debug.print("[", .{});
                dumpList(extra);
                std.debug.print("]\n", .{});
            },
            .build_map => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                std.debug.print("{{", .{});
                var extra_i: u32 = 0;
                while (extra_i < extra.len) : (extra_i += 2) {
                    if (extra_i != 0) std.debug.print(", ", .{});
                    std.debug.print("{} = {}", .{ extra[extra_i], extra[extra_i + 1] });
                }
                std.debug.print("}}\n", .{});
            },
            .build_func => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                const fn_info = @bitCast(Inst.Data.FnInfo, extra[0]);
                const fn_body = extra[1..];
                std.debug.print("args: {d}, captures: {d}\n", .{ fn_info.args, fn_info.captures });
                b.dumpExtra(fn_body, level + 2);
            },
            .build_tagged_null => {
                const str = b.strings[data[i].str.offset..][0..data[i].str.len];
                std.debug.print("@{s} = null\n", .{str});
            },
            .build_tagged => {
                const operand = b.extra[data[i].extra.extra];
                const str_offset = @enumToInt(b.extra[data[i].extra.extra + 1]);
                const str = b.strings[str_offset..][0..data[i].extra.len];
                std.debug.print("@{s} = {}\n", .{ str, operand });
            },
            .build_range => std.debug.print("{}:{}\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .build_range_step => {
                const start = data[i].range.start;
                const end = b.extra[data[i].range.extra];
                const step = b.extra[data[i].range.extra + 1];
                std.debug.print("{}:{}:{}\n", .{ start, end, step });
            },
            .tuple_len,
            .list_len,
            .assert_list_len,
            .assert_tuple_len,
            => {
                const operand = data[i].bin.lhs;
                const len = @enumToInt(data[i].bin.rhs);
                std.debug.print("{} {d}\n", .{ operand, len });
            },
            .load_global => std.debug.print("GLOBAL({})\n", .{data[i].un}),
            .load_capture => std.debug.print("CAPTURE({d})\n", .{@enumToInt(data[i].un)}),
            .store_capture => std.debug.print("CAPTURE({}) = {}\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .copy,
            .move,
            .get,
            .get_or_null,
            .set,
            .div_floor,
            .div,
            .mul,
            .pow,
            .mod,
            .add,
            .sub,
            .l_shift,
            .r_shift,
            .bit_and,
            .bit_or,
            .bit_xor,
            .equal,
            .not_equal,
            .less_than,
            .less_than_equal,
            .greater_than,
            .greater_than_equal,
            .in,
            => std.debug.print("{} {}\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .append => std.debug.print("{}.append({})\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .as, .is => std.debug.print("{} {s}\n", .{ data[i].bin_ty.operand, @tagName(data[i].bin_ty.ty) }),
            .ret,
            .negate,
            .bool_not,
            .bit_not,
            .unwrap_error,
            .iter_init,
            .discard,
            .build_error,
            .copy_un,
            => std.debug.print("{}\n", .{data[i].un}),
            .jump => std.debug.print("{d}\n", .{data[i].jump}),
            .jump_if_true,
            .jump_if_false,
            .unwrap_error_or_jump,
            .jump_if_null,
            .jump_if_error,
            .iter_next,
            => std.debug.print("{d} cond {}\n", .{ data[i].jump_condition.offset, data[i].jump_condition.operand }),
            .call => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                std.debug.print("{}(", .{extra[0]});
                dumpList(extra[1..]);
                std.debug.print(")\n", .{});
            },
            .call_one => std.debug.print("{}({})\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .call_zero => std.debug.print("{}()\n", .{data[i].un}),
            .ret_null, .build_error_null, .load_this => std.debug.print("\n", .{}),
        }
    }
}

fn dumpList(list: []const Ref) void {
    for (list) |item, i| {
        if (i != 0) std.debug.print(", ", .{});
        std.debug.print("{}", .{item});
    }
}
