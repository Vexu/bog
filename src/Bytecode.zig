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

        // use Data.extra
        build_tuple,
        build_list,
        build_map,
        // use Data.un
        build_error,
        build_error_null,
        build_tagged,
        build_tagged_null,
        /// uses Data.func with
        // extra[0] == Data.FnInfo
        build_func,
        // uses Data.range
        build_range,
        // uses Data.range_step
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
        extra: struct {
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

pub fn dump(b: *Bytecode, body: []const Ref) void {
    const ops = b.code.items(.op);
    const data = b.code.items(.data);
    for (body) |i, inst| {
        std.debug.print("{d} %{d} = {s} ", .{ inst, i, @tagName(ops[i]) });
        switch (ops[i]) {
            .primitive => std.debug.print("{s}\n", .{@tagName(data[i].primitive)}),
            .int => std.debug.print("{d}\n", .{data[i].int}),
            .num => std.debug.print("{d}\n", .{data[i].num}),
            .import, .str, .unwrap_tagged => {
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
                    std.debug.print("%{d} = %{d}", .{ extra[extra_i], extra[extra_i + 1] });
                }
                std.debug.print("}}\n", .{});
            },
            .build_tagged,
            .build_tagged_null,
            .build_func,
            .build_range,
            .build_range_step,
            => std.debug.print("TODO\n", .{}),
            .load_global => std.debug.print("GLOBAL({d})\n", .{data[i].un}),
            .load_capture => std.debug.print("CAPTURE({d})\n", .{data[i].un}),
            .store_capture => std.debug.print("CAPTURE({d}) = %{d}\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
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
            => std.debug.print("%{d} %{d}\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .append => std.debug.print("%{d}.append(%{d})\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .as, .is => std.debug.print("%{d} {s}\n", .{ data[i].bin_ty.operand, @tagName(data[i].bin_ty.ty) }),
            .ret,
            .negate,
            .bool_not,
            .bit_not,
            .unwrap_error,
            .iter_init,
            .discard,
            .build_error,
            .copy_un,
            => std.debug.print("%{d}\n", .{data[i].un}),
            .jump => std.debug.print("{d}\n", .{data[i].jump}),
            .jump_if_true,
            .jump_if_false,
            .jump_if_not_error,
            .jump_if_null,
            .jump_if_error,
            .iter_next,
            => std.debug.print("{d} cond %{d}\n", .{ data[i].jump_condition.offset, data[i].jump_condition.operand }),
            .call => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                std.debug.print("%{d}(", .{extra[0]});
                dumpList(extra[1..]);
                std.debug.print(")\n", .{});
            },
            .call_one => std.debug.print("%{d}(%{d})\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .call_zero => std.debug.print("%{d}()\n", .{data[i].un}),
            .ret_null, .build_error_null => std.debug.print("\n", .{}),
            _ => unreachable,
        }
    }
}

fn dumpList(list: []const Ref) void {
    for (list) |item, i| {
        if (i != 0) std.debug.print(", ", .{});
        std.debug.print("%{d}", .{item});
    }
}
