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

pub const Ref = enum(u32) {
    _,
    pub fn format(ref: Ref, _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeByte('%');
        try std.fmt.formatInt(@enumToInt(ref), 10, .lower, options, writer);
    }
};

pub inline fn indexToRef(i: u64) Ref {
    return @intToEnum(Ref, i + max_params);
}
pub inline fn refToIndex(r: Ref) u32 {
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
        rem,
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
        check_len,
        // same as above but return error on false
        assert_len,

        // use Data.bin
        get,
        // returns null if no
        get_or_null,
        // uses Data.range with
        // start == container
        // extra[0] == index
        // extra[1] == value
        set,

        /// uses Data.jump_condition
        /// Operand is where the error value should be stored
        /// and offset is where the VM should jump to handle the error.
        push_err_handler,
        pop_err_handler,

        // uses Data.jump
        jump,
        // use Data.jump_condition
        jump_if_true,
        jump_if_false,
        jump_if_null,
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
        throw,
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
    lines: []const Line,

    pub const Line = struct {
        index: u32,
        line: u32,
    };

    pub fn getLineForIndex(d: DebugInfo, index: u32) u32 {
        return for (d.lines) |line| {
            if (line.index >= index) break line.line;
        } else if (d.lines.len != 0)
            d.lines[d.lines.len - 1].line
        else
            0;
    }
};

pub fn dump(b: *Bytecode, body: []const Ref) void {
    const ops = b.code.items(.op);
    const data = b.code.items(.data);
    var prev_line: u32 = 0;
    for (body) |ref, inst| {
        const i = refToIndex(ref);
        const line = b.debug_info.getLineForIndex(i);
        if (line != prev_line) {
            std.debug.print("line {d}:\n", .{line});
            prev_line = line;
        }
        std.debug.print("{d:4} {} = {s} ", .{ inst, ref, @tagName(ops[i]) });
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
                std.debug.print("\n\nfn(args: {d}, captures: {d}) {{\n", .{ fn_info.args, fn_info.captures });
                b.dump(fn_body);
                std.debug.print("}}\n\nline {d}:\n", .{line});
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
            .set => {
                const container = data[i].range.start;
                const index = b.extra[data[i].range.extra];
                const val = b.extra[data[i].range.extra + 1];
                std.debug.print("{}[{}] = {}\n", .{ container, index, val });
            },
            .check_len,
            .assert_len,
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
            .div_floor,
            .div,
            .mul,
            .pow,
            .rem,
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
            .throw,
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
            .iter_next,
            .push_err_handler,
            => std.debug.print(
                "{d} cond {}\n",
                .{ data[i].jump_condition.offset, data[i].jump_condition.operand },
            ),
            .call => {
                const extra = b.extra[data[i].extra.extra..][0..data[i].extra.len];
                std.debug.print("{}(", .{extra[0]});
                dumpList(extra[1..]);
                std.debug.print(")\n", .{});
            },
            .call_one => std.debug.print("{}({})\n", .{ data[i].bin.lhs, data[i].bin.rhs }),
            .call_zero => std.debug.print("{}()\n", .{data[i].un}),
            .ret_null,
            .build_error_null,
            .load_this,
            .pop_err_handler,
            => std.debug.print("\n", .{}),
        }
    }
}

fn dumpList(list: []const Ref) void {
    for (list) |item, i| {
        if (i != 0) std.debug.print(", ", .{});
        std.debug.print("{}", .{item});
    }
}
