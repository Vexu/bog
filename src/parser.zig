const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Token = bog.Token;
const Tree = bog.Tree;
const Node = bog.Node;

const null_node: Node.Index = 0;

/// root : (stmt NL)* EOF
pub fn parse(gpa: Allocator, source: []const u8, path: []const u8, errors: *bog.Errors) (Parser.Error || bog.Tokenizer.Error)!Tree {
    var tokens = try bog.tokenize(gpa, source, path, errors);
    errdefer tokens.deinit(gpa);

    var parser: Parser = .{
        .errors = errors,
        .source = source,
        .path = path,
        .tok_ids = tokens.items(.id),
        .tok_starts = tokens.items(.start),
        .extra = std.ArrayList(Node.Index).init(gpa),
        .node_buf = std.ArrayList(Node.Index).init(gpa),
    };
    defer parser.node_buf.deinit();
    defer parser.extra.deinit();
    errdefer parser.nodes.deinit(gpa);

    while (true) switch (parser.tok_ids[parser.tok_i]) {
        .nl => parser.tok_i += 1,
        else => break,
    };

    try parser.nodes.append(gpa, undefined); // index 0 is reserved for null

    while (true) {
        _ = try parser.eatIndent(0);
        if (parser.eatToken(.eof, .keep_nl)) |_| break;
        try parser.node_buf.append(try parser.stmt(0));
        _ = parser.eatToken(.nl, .keep_nl) orelse {
            _ = try parser.expectToken(.eof, .keep_nl);
            break;
        };
    }

    return .{
        .root_nodes = try parser.node_buf.toOwnedSlice(),
        .tokens = tokens,
        .extra = try parser.extra.toOwnedSlice(),
        .nodes = parser.nodes,
        .source = source,
        .path = path,
    };
}

pub fn parseRepl(repl: *@import("repl.zig").Repl) Parser.Error!?Node.Index {
    repl.parser.tok_ids = repl.tokenizer.tokens.items(.id);
    repl.parser.tok_starts = repl.tokenizer.tokens.items(.start);
    repl.parser.source = repl.tokenizer.it.bytes;
    const start_i = repl.parser.tok_i;
    errdefer repl.parser.tok_i = start_i;
    defer repl.parser.tok_i -= 1; // go before EOF

    repl.parser.skipNl();
    if (repl.parser.eatToken(.eof, .skip_nl)) |_| return null;
    const ret = try repl.parser.stmt(0);
    repl.parser.skipNl();
    _ = try repl.parser.expectToken(.eof, .skip_nl);

    repl.tree = .{
        .root_nodes = repl.parser.node_buf.items,
        .tokens = repl.tokenizer.tokens,
        .extra = repl.parser.extra.items,
        .nodes = repl.parser.nodes,
        .source = repl.tokenizer.it.bytes,
        .path = "<stdin>",
    };
    return ret;
}

pub const Parser = struct {
    tok_ids: []const Token.Id,
    tok_i: u32 = 0,
    tok_starts: []const u32,
    nodes: Node.List = .{},
    extra: std.ArrayList(Node.Index),
    node_buf: std.ArrayList(Node.Index),
    errors: *bog.Errors,
    source: []const u8,
    path: []const u8,
    repl: bool = false,

    pub const Error = error{ ParseError, NeedInput } || Allocator.Error;

    const SkipNl = enum { skip_nl, keep_nl };

    fn addUn(p: *Parser, id: Node.Id, token: Token.Index, op: Node.Index) !Node.Index {
        const index = p.nodes.len;
        try p.nodes.append(p.extra.allocator, .{
            .id = id,
            .token = token,
            .data = .{ .un = op },
        });
        return @intCast(index);
    }

    fn addBin(p: *Parser, id: Node.Id, token: Token.Index, lhs: Node.Index, rhs: Node.Index) !Node.Index {
        const index = p.nodes.len;
        try p.nodes.append(p.extra.allocator, .{
            .id = id,
            .token = token,
            .data = .{ .bin = .{ .lhs = lhs, .rhs = rhs } },
        });
        return @intCast(index);
    }

    fn addList(p: *Parser, id: Node.Id, token: Token.Index, nodes: []const Node.Index) Allocator.Error!Node.Index {
        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(nodes);

        const index = p.nodes.len;
        try p.nodes.append(p.extra.allocator, .{
            .id = id,
            .token = token,
            .data = .{ .range = .{ .start = start, .end = @intCast(p.extra.items.len) } },
        });
        return @intCast(index);
    }

    fn addCond(p: *Parser, id: Node.Id, token: Token.Index, cond: Node.Index, nodes: []const Node.Index) Allocator.Error!Node.Index {
        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(nodes);

        const index = p.nodes.len;
        try p.nodes.append(p.extra.allocator, .{
            .id = id,
            .token = token,
            .data = .{ .cond = .{ .cond = cond, .extra = start } },
        });
        return @intCast(index);
    }

    /// decl : "let" primary_expr "=" block_or_expr
    fn decl(p: *Parser, level: u8) Error!?Node.Index {
        const let_tok = p.eatToken(.keyword_let, .skip_nl) orelse return null;
        const dest = try p.primaryExpr(.skip_nl, level);
        _ = try p.expectToken(.equal, .keep_nl);
        const init = try p.blockOrExpr(.keep_nl, level);
        return try p.addBin(.decl, let_tok, dest, init);
    }

    /// fn: "fn" "(" (primary_expr ",")* primary_expr? ")" block_or_expr
    fn func(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const fn_tok = p.eatToken(.keyword_fn, .skip_nl) orelse return null;
        _ = try p.expectToken(.l_paren, .skip_nl);

        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        var end = false;
        while (true) {
            if (p.eatToken(.r_paren, skip_nl)) |_| {
                break;
            } else if (end) {
                _ = try p.expectToken(.r_paren, skip_nl);
                break;
            }
            try p.node_buf.append(try primaryExpr(p, .skip_nl, level));
            if (p.eatToken(.ellipsis, .skip_nl) != null or
                p.eatToken(.comma, .skip_nl) == null)
                end = true;
        }
        const body = try p.blockOrExpr(skip_nl, level);
        try p.node_buf.append(body);

        const params = p.node_buf.items[node_buf_top..];
        return switch (params.len) {
            0 => unreachable, // body is always added to the list
            1 => try p.addBin(.fn_expr_one, fn_tok, null_node, body),
            2 => try p.addBin(.fn_expr_one, fn_tok, params[0], body),
            else => try p.addList(.fn_expr, fn_tok, params),
        };
    }

    /// stmt
    ///     : decl
    ///     | assign_expr
    fn stmt(p: *Parser, level: u8) Error!Node.Index {
        if (try p.decl(level)) |node| return node;
        return p.assignExpr(.keep_nl, level);
    }

    /// assign_expr : expr (("=" | "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") block_or_expr)?
    fn assignExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const lhs = try p.expr(skip_nl, level);
        const tok = p.tok_i;
        switch (p.tok_ids[tok]) {
            .equal,
            .plus_equal,
            .minus_equal,
            .asterisk_equal,
            .asterisk_asterisk_equal,
            .slash_equal,
            .slash_slash_equal,
            .percent_equal,
            .l_arr_arr_equal,
            .r_arr_arr_equal,
            .ampersand_equal,
            .pipe_equal,
            .caret_equal,
            => {
                p.tok_i += 1;
                return try p.addBin(switch (p.tok_ids[tok]) {
                    .equal => .assign,
                    .plus_equal => .add_assign,
                    .minus_equal => .sub_assign,
                    .asterisk_equal => .mul_assign,
                    .asterisk_asterisk_equal => .pow_assign,
                    .slash_equal => .div_assign,
                    .slash_slash_equal => .div_floor_assign,
                    .percent_equal => .rem_assign,
                    .l_arr_arr_equal => .l_shift_assign,
                    .r_arr_arr_equal => .r_shift_assign,
                    .ampersand_equal => .bit_and_assign,
                    .pipe_equal => .bit_or_assign,
                    .caret_equal => .bit_xor_assign,
                    else => unreachable,
                }, tok, lhs, try p.blockOrExpr(skip_nl, level));
            },
            else => return lhs,
        }
    }

    /// block_or_expr : block | assign_expr
    fn blockOrExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        if (skip_nl == .keep_nl) if (try p.block(level)) |node| return node;
        p.skipNl();
        return try p.assignExpr(skip_nl, level);
    }

    /// block : NL (stmt NL)+
    fn block(p: *Parser, level: u8) Error!?Node.Index {
        const nl = p.eatToken(.nl, .keep_nl) orelse return null;
        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        const new_level = indent: {
            const indent = p.eatIndentExtra();
            if (indent == null or indent.? <= level)
                return p.reportErr("expected indentation", p.tok_i);
            p.tok_i += 1;

            break :indent indent.?;
        };

        var last_nl = p.tok_i;
        while (true) {
            try p.node_buf.append(try p.stmt(new_level));
            last_nl = p.eatToken(.nl, .keep_nl) orelse {
                last_nl = try p.expectToken(.eof, .keep_nl);
                break;
            };
            if (!try p.eatIndent(new_level)) break;
        }

        // reset to previous new line since all statements are expected to end in a newline
        p.tok_i = last_nl;

        const stmts = p.node_buf.items[node_buf_top..];
        switch (stmts.len) {
            0 => unreachable,
            1 => return try p.addBin(.block_stmt_two, nl, stmts[0], null_node),
            2 => return try p.addBin(.block_stmt_two, nl, stmts[0], stmts[1]),
            else => return try p.addList(.block_stmt, nl, stmts),
        }
    }

    /// expr
    ///     : jump_expr
    ///     | lambda
    ///     | bool_expr
    fn expr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        if (try p.jumpExpr(skip_nl, level)) |node| return node;
        if (try p.func(skip_nl, level)) |node| return node;
        return p.boolExpr(skip_nl, level);
    }

    /// jump_expr
    ///     : "return" block_or_expr?
    ///     | "break"
    ///     | "continue"
    ///     | "throw" block_or_expr
    ///     | "suspend"
    ///     | "resume" block_or_expr
    fn jumpExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.tok_i;
        const id: Node.Id = switch (p.tok_ids[tok]) {
            .keyword_return => .return_expr,
            .keyword_break => .break_expr,
            .keyword_continue => .continue_expr,
            .keyword_throw => .throw_expr,
            .keyword_suspend => .suspend_expr,
            .keyword_resume => .resume_expr,
            else => return null,
        };
        p.tok_i += 1;

        const op = if (id == .throw_expr or id == .resume_expr)
            try p.blockOrExpr(skip_nl, level)
        else if (id == .return_expr and switch (p.tok_ids[p.tok_i]) {
            .eof, .nl, .r_paren, .r_brace, .r_bracket, .keyword_else, .keyword_catch, .comma, .colon => false,
            else => true,
        })
            try p.blockOrExpr(skip_nl, level)
        else
            null_node;

        return try p.addUn(id, tok, op);
    }

    /// bool_expr
    ///     : "not" comparison_expr
    ///     | comparison_expr ("or" comparison_expr)*
    ///     | comparison_expr ("and" comparison_expr)*
    fn boolExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        if (p.eatToken(.keyword_not, skip_nl)) |tok| {
            p.skipNl();
            return p.addUn(.bool_not_expr, tok, try p.comparisonExpr(skip_nl, level));
        }
        var lhs = try p.comparisonExpr(skip_nl, level);

        if (p.eatTokenNoNl(.keyword_or)) |t| {
            var tok = t;
            while (true) {
                p.skipNl();
                lhs = try p.addBin(.bool_or_expr, tok, lhs, try p.comparisonExpr(skip_nl, level));
                if (p.eatTokenNoNl(.keyword_or)) |tt| tok = tt else break;
            }
        } else {
            while (p.eatTokenNoNl(.keyword_and)) |tok| {
                p.skipNl();
                lhs = try p.addBin(.bool_and_expr, tok, lhs, try p.comparisonExpr(skip_nl, level));
            }
        }
        return lhs;
    }

    /// comparison_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr)?
    ///     | range_expr ("is" type_name)?
    fn comparisonExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const lhs = try p.rangeExpr(skip_nl, level);

        // we can safely skip any newlines here
        const start = p.tok_i;
        p.skipNl();
        const tok = p.nextToken(.skip_nl);
        const id = p.tok_ids[tok];
        switch (id) {
            .l_arr => return p.addBin(.less_than_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .l_arr_equal => return p.addBin(.less_than_equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .r_arr => return p.addBin(.greater_than_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .r_arr_equal => return p.addBin(.greater_than_equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .equal_equal => return p.addBin(.equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .bang_equal => return p.addBin(.not_equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .keyword_in => return p.addBin(.in_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .keyword_is => return p.addUn(.is_expr, try p.typeName(skip_nl), lhs),
            else => {
                p.tok_i = start;
                return lhs;
            },
        }
    }

    /// type_name : "null" | "int" | "num" | "bool" | "str" | "tuple" | "map" | "list" | "error" | "range" | "fn"
    fn typeName(p: *Parser, skip_nl: SkipNl) Error!Token.Index {
        return p.eatToken(.keyword_error, skip_nl) orelse
            p.eatToken(.keyword_fn, skip_nl) orelse
            p.eatToken(.keyword_null, skip_nl) orelse
            p.eatToken(.identifier, skip_nl) orelse
            p.reportErr("expected type name", p.tok_i);
    }

    /// range_expr : bit_expr (":" bit_expr? (":" bit_expr)?)?
    fn rangeExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const start = try p.bitExpr(skip_nl, level);
        const colon_1 = p.eatToken(.colon, skip_nl) orelse return start;

        var end: Node.Index = null_node;
        var colon_2 = p.eatToken(.colon, skip_nl);
        if (colon_2 == null) {
            switch (p.tok_ids[p.tok_i]) {
                .eof, .nl, .r_paren, .r_brace, .r_bracket, .keyword_else, .keyword_catch, .comma, .colon => {},
                else => end = try p.bitExpr(skip_nl, level),
            }
            colon_2 = p.eatToken(.colon, skip_nl);
        }

        const step = if (colon_2 != null) try p.bitExpr(skip_nl, level) else null_node;

        if (end != null_node and step != null_node) {
            return p.addCond(.range_expr, colon_1, start, &.{ end, step });
        } else if (end == null_node) {
            return p.addBin(.range_expr_step, colon_1, start, step);
        } else {
            return p.addBin(.range_expr_end, colon_1, start, end);
        }
    }

    /// bit_expr : shift_expr (("&" shift_expr)* | ("|" shift_expr)* | ("^" shift_expr)*
    fn bitExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var lhs = try p.shiftExpr(skip_nl, level);

        if (p.eatTokenNoNl(.ampersand)) |t| {
            var tok = t;
            while (true) {
                p.skipNl();
                lhs = try p.addBin(.bit_and_expr, tok, lhs, try p.shiftExpr(skip_nl, level));
                if (p.eatTokenNoNl(.ampersand)) |tt| tok = tt else break;
            }
        } else if (p.eatTokenNoNl(.pipe)) |t| {
            var tok = t;
            while (true) {
                p.skipNl();
                lhs = try p.addBin(.bit_or_expr, tok, lhs, try p.shiftExpr(skip_nl, level));
                if (p.eatTokenNoNl(.pipe)) |tt| tok = tt else break;
            }
        } else if (p.eatTokenNoNl(.caret)) |t| {
            var tok = t;
            while (true) {
                p.skipNl();
                lhs = try p.addBin(.bit_xor_expr, tok, lhs, try p.shiftExpr(skip_nl, level));
                if (p.eatTokenNoNl(.caret)) |tt| tok = tt else break;
            }
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr)
    fn shiftExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const lhs = try p.addExpr(skip_nl, level);

        // we can safely skip any newlines here
        const start = p.tok_i;
        p.skipNl();
        const tok = p.nextToken(.skip_nl);
        switch (p.tok_ids[tok]) {
            .l_arr_arr => {
                p.skipNl();
                return p.addBin(.l_shift_expr, tok, lhs, try p.addExpr(skip_nl, level));
            },
            .r_arr_arr => {
                p.skipNl();
                return p.addBin(.r_shift_expr, tok, lhs, try p.addExpr(skip_nl, level));
            },
            else => {
                p.tok_i = start;
                return lhs;
            },
        }
    }

    /// add_expr : mul_expr (("-" | "+") mul_expr)*
    fn addExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var lhs = try p.mulExpr(skip_nl, level);

        while (true) {
            const tok = p.nextToken(skip_nl);
            switch (p.tok_ids[tok]) {
                .minus => {
                    p.skipNl();
                    lhs = try p.addBin(.sub_expr, tok, lhs, try p.mulExpr(skip_nl, level));
                },
                .plus => {
                    p.skipNl();
                    lhs = try p.addBin(.add_expr, tok, lhs, try p.mulExpr(skip_nl, level));
                },
                else => {
                    p.tok_i = tok;
                    return lhs;
                },
            }
        }
    }

    /// mul_expr : cast_expr (("*" | "/" | "//" | "%") cast_expr)*
    fn mulExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var lhs = try p.castExpr(skip_nl, level);

        while (true) {
            // we can safely skip any newlines here
            const start = p.tok_i;
            p.skipNl();
            const tok = p.nextToken(.skip_nl);
            switch (p.tok_ids[tok]) {
                .asterisk => {
                    p.skipNl();
                    lhs = try p.addBin(.mul_expr, tok, lhs, try p.castExpr(skip_nl, level));
                },
                .slash => {
                    p.skipNl();
                    lhs = try p.addBin(.div_expr, tok, lhs, try p.castExpr(skip_nl, level));
                },
                .slash_slash => {
                    p.skipNl();
                    lhs = try p.addBin(.div_floor_expr, tok, lhs, try p.castExpr(skip_nl, level));
                },
                .percent => {
                    p.skipNl();
                    lhs = try p.addBin(.rem_expr, tok, lhs, try p.castExpr(skip_nl, level));
                },
                else => {
                    p.tok_i = start;
                    return lhs;
                },
            }
        }

        return lhs;
    }

    /// cast_expr : prefix_expr ("as" type_name)?
    fn castExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var lhs = try p.prefixExpr(skip_nl, level);

        if (p.eatTokenNoNl(.keyword_as)) |_| {
            p.skipNl();
            lhs = try p.addUn(.as_expr, try p.typeName(skip_nl), lhs);
        }

        return lhs;
    }

    /// prefix_expr : ("-" | "~" | "await")? power_expr
    fn prefixExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const tok = p.nextToken(skip_nl);
        switch (p.tok_ids[tok]) {
            .minus => {
                p.skipNl();
                return p.addUn(.negate_expr, tok, try p.powerExpr(skip_nl, level));
            },
            .tilde => {
                p.skipNl();
                return p.addUn(.bit_not_expr, tok, try p.powerExpr(skip_nl, level));
            },
            .keyword_await => {
                p.skipNl();
                return p.addUn(.await_expr, tok, try p.powerExpr(skip_nl, level));
            },
            else => {
                p.tok_i = tok;
                return try p.powerExpr(skip_nl, level);
            },
        }
    }

    /// power_expr : suffix_expr ("**" power_expr)?
    fn powerExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const primary = try p.suffixExpr(skip_nl, level);
        if (p.eatTokenNoNl(.asterisk_asterisk)) |tok| {
            p.skipNl();
            return p.addBin(.pow_expr, tok, primary, try p.powerExpr(skip_nl, level));
        }
        return primary;
    }

    /// suffix_expr
    ///     : primary_expr suffix_op*
    ///     | "async" primary_expr suffix_op* call_args
    fn suffixExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const is_async = p.eatToken(.keyword_async, .skip_nl) != null;
        var lhs = try p.primaryExpr(skip_nl, level);
        while (true) {
            if (try p.suffixOp(lhs, skip_nl, level)) |some| {
                lhs = some;
            } else if (try p.callArgs(is_async, lhs, skip_nl, level)) |some| {
                if (is_async) return some;
                lhs = some;
            } else break;
        }
        if (is_async) return p.reportErr("expected a function call", p.tok_i);
        return lhs;
    }
    /// suffix_op
    ///     : "[" expr "]"
    ///     | call_args
    ///     | "." IDENTIFIER
    fn suffixOp(p: *Parser, lhs: Node.Index, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        if (p.eatToken(.l_bracket, skip_nl)) |tok| {
            p.skipNl();
            const res = try p.addBin(.array_access_expr, tok, lhs, try p.expr(.skip_nl, level));
            _ = try p.expectToken(.r_bracket, .keep_nl);
            return res;
        } else if (p.eatTokenNoNl(.period)) |_| {
            p.skipNl();
            const ident = try p.expectToken(.identifier, skip_nl);
            return try p.addUn(.member_access_expr, ident, lhs);
        } else {
            return null;
        }
    }

    /// call_args : "(" (spread_expr ",")* spread_expr? ")"
    fn callArgs(p: *Parser, is_async: bool, lhs: Node.Index, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.l_paren, .skip_nl) orelse return null;
        const args = try p.listParser(skip_nl, level, spreadExpr, .r_paren, lhs);
        const ops = [2][2]Node.Id{ .{ .call_expr_one, .call_expr }, .{ .async_call_expr_one, .async_call_expr } };
        return switch (args.len) {
            0 => unreachable, // we pass lhs as first
            1 => try p.addBin(ops[@intFromBool(is_async)][0], tok, lhs, null_node),
            2 => try p.addBin(ops[@intFromBool(is_async)][0], tok, lhs, args[1]),
            else => try p.addList(ops[@intFromBool(is_async)][1], tok, args),
        };
    }

    /// spread_expr : "..."? expr
    fn spreadExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const ellipsis = p.eatToken(.ellipsis, skip_nl);
        const operand = try p.expr(skip_nl, level);
        if (ellipsis) |some| {
            return p.addUn(.spread_expr, some, operand);
        } else {
            return operand;
        }
    }

    /// primary_expr
    ///     : IDENTIFIER
    ///     | STRING
    ///     | format_string
    ///     | NUMBER
    ///     | "true"
    ///     | "false"
    ///     | "null"
    ///     | initializer
    ///     | "error" initializer?
    ///     | "@" IDENTIFIER initializer?
    ///     | "import" block_or_expr
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    ///     | try
    fn primaryExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const tok = p.nextToken(skip_nl);
        switch (p.tok_ids[tok]) {
            .keyword_mut => {
                p.skipNl();
                const ident = try p.expectToken(.identifier, skip_nl);
                return p.addUn(.mut_ident_expr, ident, null_node);
            },
            .underscore => return p.addUn(.discard_expr, tok, null_node),
            .identifier => return p.addUn(.ident_expr, tok, null_node),
            .string => return p.addUn(.string_expr, tok, null_node),
            .number => return p.addUn(.num_expr, tok, null_node),
            .integer => return p.addUn(.int_expr, tok, null_node),
            .keyword_true => return p.addUn(.true_expr, tok, null_node),
            .keyword_false => return p.addUn(.false_expr, tok, null_node),
            .keyword_null => return p.addUn(.null_expr, tok, null_node),
            .keyword_this => return p.addUn(.this_expr, tok, null_node),
            .keyword_error => {
                const init = (try p.initializer(skip_nl, level)) orelse null_node;
                return p.addUn(.error_expr, tok, init);
            },
            .at => {
                p.skipNl();
                const ident = try p.expectToken(.identifier, skip_nl);
                const init = (try p.initializer(skip_nl, level)) orelse null_node;
                return p.addUn(.enum_expr, ident, init);
            },
            .keyword_import => {
                const str = try p.blockOrExpr(skip_nl, level);
                return p.addUn(.import_expr, tok, str);
            },
            else => {
                p.tok_i = tok;
            },
        }
        if (try p.formatString(skip_nl, level)) |res| return res;
        if (try p.initializer(skip_nl, level)) |res| return res;
        if (try p.ifExpr(skip_nl, level)) |res| return res;
        if (try p.whileExpr(skip_nl, level)) |res| return res;
        if (try p.forExpr(skip_nl, level)) |res| return res;
        if (try p.matchExpr(level)) |res| return res;
        if (try p.tryExpr(skip_nl, level)) |res| return res;
        return p.reportErr("expected an identifier, a literal '(', '{{', '[', error, try, if, while, for or match", p.tok_i);
    }

    /// format_string : FORMAT_START expr "="? (FORMAT expr "="?)* FORMAT_END
    fn formatString(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const first = p.eatToken(.format_start, .skip_nl) orelse return null;

        var toks = std.ArrayList(Token.Index).init(p.extra.allocator);
        defer toks.deinit();
        try toks.append(first);

        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        while (true) {
            try p.node_buf.append(try p.expr(.skip_nl, level));
            _ = p.eatToken(.equal, .skip_nl);

            if (p.eatToken(.format, .skip_nl)) |tok| {
                try toks.append(tok);
            } else break;
        }
        try toks.append(try p.expectToken(.format_end, skip_nl));

        const fmt_start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(@ptrCast(toks.items));
        const args_start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(p.node_buf.items[node_buf_top..]);

        const index = p.nodes.len;
        try p.nodes.append(p.extra.allocator, .{
            .id = .format_expr,
            .token = first,
            .data = .{ .format = .{ .fmt_start = fmt_start, .args_start = args_start } },
        });
        return @intCast(index);
    }

    /// initializer
    ///     : "(" block_or_expr ")"
    ///     | "(" (expr ",")+ expr? ")"
    ///     | "{" (expr "=" expr ",")* (expr "=" expr)? "}"
    ///     | "[" (expr ",")* expr? "]"
    fn initializer(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        if (p.eatToken(.l_brace, .skip_nl)) |tok| {
            const elems = try p.listParser(skip_nl, level, mapItem, .r_brace, null);
            switch (elems.len) {
                0 => return try p.addBin(.map_expr_two, tok, null_node, null_node),
                1 => return try p.addBin(.map_expr_two, tok, elems[0], null_node),
                2 => return try p.addBin(.map_expr_two, tok, elems[0], elems[1]),
                else => return try p.addList(.map_expr, tok, elems),
            }
        } else if (p.eatToken(.l_bracket, .skip_nl)) |tok| {
            const elems = try p.listParser(skip_nl, level, spreadExpr, .r_bracket, null);
            switch (elems.len) {
                0 => return try p.addBin(.list_expr_two, tok, null_node, null_node),
                1 => return try p.addBin(.list_expr_two, tok, elems[0], null_node),
                2 => return try p.addBin(.list_expr_two, tok, elems[0], elems[1]),
                else => return try p.addList(.list_expr, tok, elems),
            }
        } else if (p.eatToken(.l_paren, .keep_nl)) |tok| {
            if (try p.block(level)) |b| {
                p.skipNl();
                _ = try p.expectToken(.r_paren, skip_nl);
                return try p.addUn(.paren_expr, tok, b);
            }
            p.skipNl();
            const first = try p.spreadExpr(.skip_nl, level);
            if (p.nodes.items(.id)[first] != .spread_expr and p.eatToken(.r_paren, skip_nl) != null) {
                return try p.addUn(.paren_expr, tok, first);
            }
            _ = try p.expectToken(.comma, .skip_nl);

            const elems = try p.listParser(skip_nl, level, spreadExpr, .r_paren, first);
            switch (elems.len) {
                0 => return try p.addBin(.tuple_expr_two, tok, null_node, null_node),
                1 => return try p.addBin(.tuple_expr_two, tok, elems[0], null_node),
                2 => return try p.addBin(.tuple_expr_two, tok, elems[0], elems[1]),
                else => return try p.addList(.tuple_expr, tok, elems),
            }
        } else return null;
    }

    /// expr ("=" expr)?
    fn mapItem(p: *Parser, _: SkipNl, level: u8) Error!Node.Index {
        var tok = p.tok_i;
        var key: Node.Index = null_node;
        var value = try p.expr(.skip_nl, level);
        if (p.eatToken(.equal, .skip_nl)) |eq| {
            tok = eq;
            key = value;
            value = try p.expr(.skip_nl, level);
        }
        return p.addBin(.map_item_expr, tok, key, value);
    }

    /// (PARSE_FN ",")* PARSE_FN? TERM
    fn listParser(
        p: *Parser,
        skip_nl: SkipNl,
        level: u8,
        comptime parseFn: fn (*Parser, SkipNl, u8) Error!Node.Index,
        term_id: Token.Id,
        first: ?Node.Index,
    ) Error![]Node.Index {
        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;
        if (first) |some| try p.node_buf.append(some);

        var end = false;
        while (true) {
            if (p.eatToken(term_id, skip_nl)) |_| {
                break;
            } else if (end) {
                _ = try p.expectToken(term_id, skip_nl);
                break;
            }
            try p.node_buf.append(try parseFn(p, .skip_nl, level));
            if (p.eatToken(.comma, .skip_nl) == null) end = true;
        }
        return p.node_buf.items[node_buf_top..];
    }

    /// if : "if" ("let" primary_expr "=")? expr block_or_expr ("else" block_or_expr)?
    fn ifExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_if, .skip_nl) orelse return null;
        const let = p.eatToken(.keyword_let, .skip_nl);
        const dest = if (let) |_| try p.primaryExpr(.skip_nl, level) else null_node;
        if (let) |_| _ = try p.expectToken(.equal, .skip_nl);
        const cond = try p.expr(.keep_nl, level);
        const then_body = try p.blockOrExpr(skip_nl, level);
        const @"else" = p.eatTokenNoNl(.keyword_else);
        const else_body = if (@"else") |_| try p.blockOrExpr(skip_nl, level) else null_node;

        if (dest == null_node and else_body == null_node) {
            return try p.addBin(.if_expr, tok, cond, then_body);
        } else if (dest == null_node) {
            return try p.addCond(.if_else_expr, tok, cond, &.{ then_body, else_body });
        } else if (else_body == null_node) {
            return try p.addCond(.if_let_expr, tok, cond, &.{ dest, then_body });
        } else {
            return try p.addCond(.if_let_else_expr, tok, cond, &.{ dest, then_body, else_body });
        }
    }

    /// while : "while" ("let" primary_expr "=")? expr block_or_expr
    fn whileExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_while, .skip_nl) orelse return null;
        const let = p.eatToken(.keyword_let, .skip_nl);
        const dest = if (let) |_| try p.primaryExpr(.skip_nl, level) else null_node;
        if (let) |_| _ = try p.expectToken(.equal, .skip_nl);
        const cond = try p.expr(.keep_nl, level);
        const body = try p.blockOrExpr(skip_nl, level);

        if (dest == null_node) {
            return try p.addBin(.while_expr, tok, cond, body);
        } else {
            return try p.addCond(.while_let_expr, tok, cond, &.{ dest, body });
        }
    }

    /// for : "for" ("let" primary_expr "in")? expr block_or_expr
    fn forExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_for, .skip_nl) orelse return null;
        const let = p.eatToken(.keyword_let, .skip_nl);
        const dest = if (let) |_| try p.primaryExpr(.skip_nl, level) else null_node;
        if (let) |_| _ = try p.expectToken(.keyword_in, .skip_nl);
        const cond = try p.expr(.keep_nl, level);
        const body = try p.blockOrExpr(skip_nl, level);

        if (dest == null_node) {
            return try p.addBin(.for_expr, tok, cond, body);
        } else {
            return try p.addCond(.for_let_expr, tok, cond, &.{ dest, body });
        }
    }

    /// match : "match" expr (NL match_case)+ NL
    fn matchExpr(p: *Parser, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_match, .skip_nl) orelse return null;
        const cond = try p.expr(.keep_nl, level);
        _ = try p.expectToken(.nl, .keep_nl);

        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        // in case we need it in match_expr
        try p.node_buf.append(cond);

        const new_level = indent: {
            const indent = p.eatIndentExtra();
            if (indent == null or indent.? <= level)
                return p.reportErr("expected indentation", p.tok_i);
            p.tok_i += 1;
            break :indent indent.?;
        };

        var last_nl = p.tok_i;
        while (true) {
            try p.node_buf.append(try p.matchCase(new_level));
            last_nl = p.eatToken(.nl, .keep_nl) orelse {
                last_nl = try p.expectToken(.eof, .keep_nl);
                break;
            };
            if (!try p.eatIndent(new_level)) break;
        }

        // reset to previous new line since all statements are expected to end in a newline
        p.tok_i = last_nl;

        const cases = p.node_buf.items[node_buf_top..];
        switch (cases.len) {
            0 => unreachable, // must have at least one case
            1 => unreachable, // cases[0] == cond
            2 => return try p.addBin(.match_expr_one, tok, cond, cases[1]),
            else => return try p.addList(.match_expr, tok, cases),
        }
    }

    /// match_case
    ///    : "let" primary_expr "=>" block_or_expr
    ///    | expr ("," expr)* ","? "=>" block_or_expr
    fn matchCase(p: *Parser, level: u8) Error!Node.Index {
        if (p.eatToken(.keyword_let, .skip_nl)) |_| {
            const dest = try p.primaryExpr(.skip_nl, level);
            const arr = try p.expectToken(.equal_rarr, .keep_nl);
            const body = try p.blockOrExpr(.keep_nl, level);
            return p.addBin(.match_case_let, arr, dest, body);
        } else if (p.eatToken(.underscore, .skip_nl)) |_| {
            const arr = try p.expectToken(.equal_rarr, .keep_nl);
            return p.addUn(.match_case_catch_all, arr, try p.blockOrExpr(.keep_nl, level));
        } else {
            const start = p.tok_i;
            const node_buf_top = p.node_buf.items.len;
            defer p.node_buf.items.len = node_buf_top;

            var items = try p.listParser(.skip_nl, level, expr, .equal_rarr, null);
            const arr = p.tok_i - 1;

            p.node_buf.items.len += items.len;
            try p.node_buf.append(try p.blockOrExpr(.keep_nl, level));

            items = p.node_buf.items[node_buf_top..];
            switch (items.len) {
                0 => unreachable,
                1 => return p.reportErr("expected at least one item in match case", start),
                2 => return p.addBin(.match_case_one, arr, items[0], items[1]),
                else => return p.addList(.match_case, arr, items),
            }
        }
    }

    /// try : "try" block_or_expr ("catch" ("let" primary_expr | expr)? block_or_expr)*
    fn tryExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_try, .keep_nl) orelse return null;
        const body = try p.blockOrExpr(.keep_nl, level);

        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;
        // in case we need it in try_expr
        try p.node_buf.append(body);

        while (try p.catchExpr(skip_nl, level)) |catch_expr| {
            try p.node_buf.append(catch_expr);
        }

        const catches = p.node_buf.items[node_buf_top..];
        switch (catches.len) {
            0 => unreachable,
            1 => return try p.addBin(.try_expr_one, tok, body, null_node),
            2 => return try p.addBin(.try_expr_one, tok, body, catches[1]),
            else => return try p.addList(.try_expr, tok, catches),
        }
    }

    /// "catch" ("let" primary_expr | expr)? block_or_expr
    fn catchExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatTokenNoNl(.keyword_catch) orelse return null;

        if (p.eatToken(.keyword_let, .skip_nl)) |_| {
            return try p.addBin(.catch_let_expr, tok, try p.primaryExpr(.keep_nl, level), try p.blockOrExpr(skip_nl, level));
        } else if (p.tok_ids[p.tok_i] == .nl) {
            // catch
            //    foo
            return try p.addBin(.catch_expr, tok, null_node, try p.blockOrExpr(skip_nl, level));
        } else {
            const capture_or_body = try p.expr(.keep_nl, level);
            const start_i = p.tok_i;
            if (p.eatToken(.nl, .keep_nl) != null and p.eatIndentExtra() orelse level > level) {
                // catch 1
                //    foo
                p.tok_i = start_i;
                return try p.addBin(.catch_expr, tok, capture_or_body, try p.blockOrExpr(skip_nl, level));
            } else {
                // catch 1
                return try p.addBin(.catch_expr, tok, null_node, capture_or_body);
            }
        }
    }

    fn reportErr(p: *Parser, msg: []const u8, tok: Token.Index) Error {
        if (p.repl and p.tok_ids[p.tok_i] == .eof) return error.NeedInput;
        try p.errors.add(.{ .data = msg }, p.source, p.path, p.tok_starts[tok], .err);
        return error.ParseError;
    }

    /// skips nl begins and ends
    fn skipNl(p: *Parser) void {
        _ = p.eatToken(.nl, .skip_nl);
    }

    fn eatIndent(p: *Parser, level: u8) !bool {
        const indent = p.eatIndentExtra() orelse return false;
        if (indent > level) return p.reportErr("unexpected indentation", p.tok_i);
        if (indent != level) return false;
        p.tok_i += 1;
        return true;
    }

    fn eatIndentExtra(p: *Parser) ?u8 {
        return switch (p.tok_ids[p.tok_i]) {
            .indent_1 => 1,
            .indent_2 => 2,
            .indent_3 => 3,
            .indent_4 => 4,
            .indent_5 => 5,
            .indent_6 => 6,
            .indent_7 => 7,
            .indent_8 => 8,
            .indent_9 => 9,
            .indent_10 => 10,
            .indent_11 => 11,
            .indent_12 => 12,
            .indent_13 => 13,
            .indent_14 => 14,
            .indent_15 => 15,
            .indent_16 => 16,
            .indent_17 => 17,
            .indent_18 => 18,
            .indent_19 => 19,
            .indent_20 => 20,
            .indent_21 => 21,
            .indent_22 => 22,
            .indent_23 => 23,
            .indent_24 => 24,
            .indent_25 => 25,
            .indent_26 => 26,
            .indent_27 => 27,
            .indent_28 => 28,
            .indent_29 => 29,
            .indent_30 => 30,
            .indent_31 => 31,
            .indent_32 => 32,
            else => return null,
        };
    }

    fn nextToken(p: *Parser, skip_nl: SkipNl) Token.Index {
        const result = p.tok_i;
        p.tok_i += 1;

        if (p.tok_i >= p.tok_ids.len) return result;
        while (true) {
            switch (p.tok_ids[p.tok_i]) {
                // skip nl and indent if they are not meaningful
                // zig fmt: off
                .indent_1, .indent_2, .indent_3, .indent_4,
                .indent_5, .indent_6, .indent_7, .indent_8,
                .indent_9, .indent_10, .indent_11, .indent_12,
                .indent_13, .indent_14, .indent_15, .indent_16,
                .indent_17, .indent_18, .indent_19, .indent_20,
                .indent_21, .indent_22, .indent_23, .indent_24,
                .indent_25, .indent_26, .indent_27, .indent_28,
                .indent_29, .indent_30, .indent_31, .indent_32,
                // zig fmt: on
                => if (skip_nl == .keep_nl) break,
                .nl => if (skip_nl == .keep_nl and p.tok_i + 1 <= p.tok_ids.len) break,
                else => break,
            }
            p.tok_i += 1;
        }
        return result;
    }

    fn eatToken(p: *Parser, id: Token.Id, skip_nl: SkipNl) ?Token.Index {
        return if (p.tok_ids[p.tok_i] == id) p.nextToken(skip_nl) else null;
    }

    fn eatTokenNoNl(p: *Parser, id: Token.Id) ?Token.Index {
        const start = p.tok_i;
        p.skipNl();
        if (p.eatToken(id, .keep_nl)) |tok| return tok else {
            p.tok_i = start;
            return null;
        }
    }

    fn expectToken(p: *Parser, id: Token.Id, skip_nl: SkipNl) !Token.Index {
        if (p.eatToken(id, skip_nl)) |tok| return tok;
        try p.errors.add(try @import("String.zig").init(
            p.errors.arena.child_allocator,
            "expected '{s}', found '{s}'",
            .{ Token.string(id), Token.string(p.tok_ids[p.tok_i]) },
        ), p.source, p.path, p.tok_starts[p.tok_i], .err);
        return error.ParseError;
    }
};
