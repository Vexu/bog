const std = @import("std");
const Allocator = std.mem.Allocator;

const bog = @import("bog.zig");
const Token = bog.Token;

pub const Tree = struct {
    tokens: Token.List,
    nodes: Node.List,
    extra: []Node.Index,
    root_decls: []Node.Index,

    /// not owned by the tree
    source: []const u8,

    pub fn deinit(tree: *Tree, gpa: *Allocator) void {
        tree.tokens.deinit(gpa);
        tree.nodes.deinit(gpa);
        gpa.free(tree.extra);
        gpa.free(tree.root_decls);
    }

    pub const render = @import("render.zig").render;
};

pub const Node = struct {
    id: Id,
    token: Token.Index,
    data: union {
        un: Index,
        bin: struct {
            lhs: Index,
            rhs: Index,
        },
        range: struct {
            start: u32,
            end: u32,
        },
        cond: struct {
            cond: Index,
            extra: u32,
        },
        format: struct {
            fmt_start: u32,
            args_start: u32,

            fn str(f: @This(), extra: []Index) []Token.Index {
                return @bitCast([]Token.Index, extra[f.fmt_start..f.args_start]);
            }

            fn exprs(f: @This(), extra: []Index) []Index {
                // there are N -1 expressions for N string parts
                return extra[f.args_start..][0 .. f.args_start - f.fmt_start - 1];
            }
        },
    },

    pub const List = std.MultiArrayList(Node);

    pub const Index = enum(u32) { none = 0, _ };

    pub const Id = enum(u8) {
        // declarations

        /// export un
        export_decl,
        /// import extra[start] extra[start+1..end]
        import_decl,
        /// import lhs rhs, rhs may be omitted
        import_decl_one,
        /// let lhs = rhs
        let_decl,
        /// type token = un
        type_decl,
        /// fn token(extra[start..end-2]): extra[end-1] extra[end-2]
        fn_decl,
        /// fn token(extra[start..end-1]) extra[end-1]
        fn_decl_ret,
        /// fn token(): lhs rhs
        fn_decl_zero,
        /// fn token(lhs) rhs
        fn_decl_one,

        // destructuring

        /// token: un, un may be omitted
        ident_dest,
        /// mut token: un, un may be omitted
        mut_ident_dest,
        /// _
        discard_dest,
        /// token un
        enum_dest,

        // types

        /// any
        any_type,
        /// never
        never_type,
        /// none
        none_type,
        /// int
        int_type,
        /// num
        num_type,
        /// bool
        bool_type,
        /// str
        str_type,
        /// range[start] | range[start + 1] | ... | range[end]
        union_type,
        /// lhs | rhs
        union_type_two,
        /// enum 
        enum_type,

        // statements

        /// NL lhs NL rhs NL, lhs and rhs may be omitted
        block_stmt,
        /// NL extra[start..end] NL
        block_stmt_two,

        // assignment expressions

        /// lhs = rhs
        assign,
        /// lhs += rhs
        add_assign,
        /// lhs -= rhs
        sub_assign,
        /// lhs *= rhs
        mul_assign,
        /// lhs **= rhs
        pow_assign,
        /// lhs /= rhs
        div_assign,
        /// lhs //= rhs
        div_floor_assign,
        /// lhs %= rhs
        mod_assign,
        /// lhs <<= rhs
        l_shift_assign,
        /// lhs >>= rhs
        r_shift_assign,
        /// lhs &= rhs
        bit_and_assign,
        /// lhs |= rhs
        bit_or_assign,
        /// lhs ^= rhs
        bit_x_or_assign,

        // expressions

        /// return un, un may be omitted
        return_expr,
        /// break
        break_expr,
        /// continue
        continue_expr,
        /// throw un
        throw_expr,
        /// fn(extra[start..end-2]): extra[end-1] extra[end-2]
        lambda_expr,
        /// fn(extra[start..end-1]) extra[end-1]
        lambda_expr_ret,
        /// fn(): lhs rhs
        lambda_expr_zero,
        /// fn(lhs) rhs
        lambda_expr_one,
        /// ( data.un )
        paren_expr,
        /// ( extra[start..end])
        tuple_expr,
        /// (lhr, rhs) rhs may be omitted
        tuple_expr_two,
        /// [extra[start..end]]
        list_expr,
        /// [lhs, rhs] both may be omitted
        list_expr_two,
        /// { extra[start..end] }
        map_expr,
        /// { lhs, rhs } both may be omitted
        map_expr_two,
        /// lhs = rhs
        map_item_expr,
        /// token
        decl_ref_expr,
        /// token
        string_expr,
        /// token
        int_expr,
        /// token
        num_expr,
        /// true
        true_expr,
        /// false
        false_expr,
        /// this
        this_expr,
        /// null
        null_expr,
        /// error un, un may be omitted
        error_expr,
        /// @token un, un may be omitted
        enum_expr,
        /// lhs[rhs]
        array_access_expr,
        /// lhs.token
        member_access_expr,
        /// extra[start](extra[start+1]..extra[end])
        call_expr,
        /// lhs(rhs) rhs may be omitted
        call_expr_one,
        /// if lhs rhs
        if_expr,
        /// if cond extra[0] else extra[1]
        if_else_expr,
        /// if let extra[0] = cond extra[1]
        if_let_expr,
        /// if let extra[0] = cond extra[1] else extra[2]
        if_let_else_expr,
        /// while lhs rhs
        while_expr,
        /// while let extra[0] cond extra[1]
        while_let_expr,
        /// for lhs rhs
        for_expr,
        /// for let extra[0] cond extra[1]
        for_let_expr,
        /// match extra[start] extra[start+1..end]
        match_expr,
        /// match lhs rhs
        match_expr_one,
        /// let lhs => rhs
        match_case_let,
        /// _ => un
        match_case_catch_all,
        /// extra[start..end -1] => extra[end]
        match_case,
        /// lhs => rhs
        match_case_one,
        /// try extra[start] extra[start+1..end]
        try_expr,
        /// try lhs rhs, rhs may be omitted
        try_one_expr,
        /// catch lhs rhs, lhs may be omitted
        catch_expr,
        /// catch let lhs rhs
        catch_let_expr,
        /// f" format "
        format_expr,
        /// cond : extra[0] : extra[1]
        range_expr,
        /// : lhs : rhs
        range_expr_start,
        /// lhs : : rhs
        range_expr_end,
        /// lhs : rhs :
        range_expr_step,

        // unary expressions

        /// not un
        bool_not_expr,
        /// ~un
        bit_not_expr,
        /// -un
        negate_expr,
        /// +un
        plus_expr,

        // binary expressions

        /// lhs or rhs
        bool_or_expr,
        /// lhs and rhs
        bool_and_expr,
        /// lhs < rhs
        less_than_expr,
        /// lhs <= rhs
        less_than_equal_expr,
        /// lhs > rhs
        greater_than_expr,
        /// lhs >= rhs
        greater_than_equal_expr,
        /// lhs == rhs
        equal_expr,
        /// lhs != rhs
        not_equal_expr,
        /// lhs in rhs
        in_expr,
        /// lhs is rhs
        is_expr,
        /// lhs as rhs
        as_expr,
        /// lhs & rhs
        bit_and_expr,
        /// lhs | rhs
        bit_or_expr,
        /// lhs ^ rhs
        bit_xor_expr,
        /// lhs << rhs
        l_shift_expr,
        /// lhs >> rhs
        r_shift_expr,
        /// lhs + rhs
        add_expr,
        /// lhs - rhs
        sub_expr,
        /// lhs * rhs
        mul_expr,
        /// lhs / rhs
        div_expr,
        /// lhs // rhs
        div_floor_expr,
        /// lhs % rhs
        mod_expr,
        /// lhs ** rhs
        pow_expr,
    };
};
