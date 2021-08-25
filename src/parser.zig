const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Token = bog.Token;
const Tree = bog.Tree;
const Node = bog.Node;

/// file : (decl_or_export NL)* EOF
pub fn parse(gpa: *Allocator, source: []const u8, errors: *bog.Errors) (Parser.Error || bog.Tokenizer.Error)!*Tree {
    const tokens = try bog.tokenize(gpa, source, errors);
    errdefer gpa.free(tokens);

    var parser = Parser{
        .gpa = gpa,
        .errors = errors,
        .tok_ids = tokens.items(.id),
    };
    var nodes = Node.List.init(gpa);
    defer nodes.deinit();

    while (true) switch (parser.tok_ids[parser.tok_i]) {
        .nl => parser.tok_i += 1,
        else => break,
    };

    while (true) {
        _ = try parser.eatIndent(0);
        if (parser.eatToken(.eof, false)) |_| break;
        try nodes.append(try parser.declOrExport());
        _ = parser.eatToken(.nl, false) orelse {
            _ = try parser.expectToken(.eof, false);
            break;
        };
    }

    return Tree{
        .tokens = tokens,
        .extra = extra,
        .nodes = nodes,
        .source = source,
        .gpa = gpa,
    };
}

pub fn parseRepl(repl: *@import("repl.zig").Repl) Parser.Error!?Node.Index {
    var parser = Parser{
        .gpa = repl.compiler.gpa,
        .errors = &repl.vm.errors,
        .tok_ids = rep.tokenizer.tokens.items(.id),
        .tok_i = repl.tok_i,
    };
    defer repl.tok_i = @intCast(u32, parser.tok_ids.len - 1);

    if (parser.eatToken(.eof, true)) |_| return null;
    const ret = try parser.decl(0);
    _ = try parser.expectToken(.nl, true);
    _ = try parser.expectToken(.eof, true);
    return ret;
}

pub const Parser = struct {
    tok_ids: []Token.Id,
    tok_i: u32,
    nodes: Node.List,
    extra: std.ArrayList(Node.Index),
    node_buf: std.ArrayList(Node.Index),

    pub const Error = error{ParseError} || Allocator.Error;

    const SkipNl = enum { skip_nl, keep_nl };

    fn addUn(p: *Parser, id: Node.Id, token: Token.Index, op: Node.Index) !Node.Index {
        const index = p.nodes.len;
        try p.nodes.append(.{
            .id = id,
            .token = token,
            .data = .{ .un = op },
        });
        return @intToEnum(Node.Index, index);
    }

    fn addBin(p: *Parser, id: Node.Id, token: Token.Index, lhs: Node.Index, rhs: Node.Index) !Node.Index {
        const index = p.nodes.len;
        try p.nodes.append(.{
            .id = id,
            .token = token,
            .data = .{ .bin = .{ .lhs = lhs, .rhs = rhs } },
        });
        return @intToEnum(Node.Index, index);
    }

    fn addList(p: *Parser, id: Node.Id, token: Token.Index, nodes: []const Node.Index) Allocator.Error!Node.Data {
        const start = @intCast(u32, p.extra.items.len);
        try p.extra.appendSlice(nodes);

        const index = p.nodes.len;
        try p.nodes.append(.{
            .id = id,
            .token = token,
            .data = .{ .range = .{ .start = start, .end = @intCast(u32, p.extra.items.len) } },
        });
        return @intToEnum(Node.Index, index);
    }

    fn addCond(p: *Parser, id: Node.Id, token: Token.Index, cond: Node.Index, nodes: []const Node.Index) Allocator.Error!Node.Data {
        const start = @intCast(u32, p.extra.items.len);
        try p.extra.appendSlice(nodes);

        const index = p.nodes.len;
        try p.nodes.append(.{
            .id = id,
            .token = token,
            .data = .{ .cond = .{ .cond = cond, .extra = start } },
        });
        return @intToEnum(Node.Index, index);
    }

    /// decl_or_export
    ///     : "export" decl
    ///     | decl
    fn declOrExport(p: *Parser) Error!Node.Index {
        if (p.eatToken(.keyword_export, .skip_nl)) |e_tok| {
            const inner = try p.decl(0);
            return p.addNode(.export_decl, e_tok, .{ .un = inner });
        }
        return p.decl(0);
    }

    /// decl
    ///     : import STRING import_suffix?
    ///     | "let" destructuring "=" block_or_expr
    ///     | "type" IDENTIFIER "=" type
    ///     | "fn" IDENTIFIER "(" (destructuring ",")* destructuring? ")" (":" type)? block_or_expr
    fn decl(p: *Parser, level: u8) Error!?Node.Index {
        switch (p.tok_ids[p.tok_i]) {
            .keyword_import => {
                @panic("TODO import decl");
            },
            .keyword_let => {
                const let_tok = p.tok_i;
                p.tok_i += 1;
                const dest = try p.destructuring(level);
                _ = try p.expectToken(.equal, .keep_nl);
                const init = try p.blockOrExpr(.keep_nl, level);
                return try p.addBin(.let_decl, let_tok, dest, init);
            },
            .keyword_type => {
                p.tok_i += 1;
                const name = try p.expectToken(.identifier, .skip_nl);
                _ = try p.expectToken(.equal, .keep_nl);
                const init = try p.Type(.skip_nl, level);
                return try p.addUn(.type_decl, name, init);
            },
            .keyword_fn => {
                @panic("TODO fn decl");
            },
            else => return null,
        }
    }

    /// destructuring
    ///     : "mut"? IDENTIFIER (":" type)?
    ///     | "_"
    ///     | IDENTIFIER compound_destructuring
    ///     | "error" compound_destructuring
    ///     | compound_destructuring
    ///     | destructuring? ":" (destructuring? (":" destructuring)?)?
    fn destructuring(p: *Parser, level: u8) Error!Node.Index {
        _ = p;
        _ = level;
        @panic("TODO destructuring");
    }

    /// compound_destructuring
    ///     : "(" destructuring ")"
    ///     | "(" destructuring ("," destructuring)* ","? ")"
    ///     | "[" destructuring ("," destructuring)* ","? "]"
    ///     | "{" destructuring ("," destructuring)* ","? "}"
    fn compoundDestructuring(p: *Parser, level: u8) Error!Node.Index {
        _ = p;
        _ = level;
        @panic("TODO compoundDestructuring");
    }

    // type
    //     : "any"
    //     | "never"
    //     | "none"
    //     | "int"
    //     | "num"
    //     | "bool"
    //     | "str"
    //     | "range"
    //     | type ("|" type)+
    //     | "fn" "(" type ("," type)* ","? ")" (":" type)?
    //     | "error" compound_type
    //     | "enum" IDENTIFIER compound_type ("," IDENTIFIER compound_type)*
    fn Type(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        _ = p;
        _ = skip_nl;
        _ = level;
        @panic("TODO type");
    }

    /// compound_type
    ///     : "(" type ")"
    ///     | "(" type ("," type)* "," ")"
    ///     | "[" type "]"
    ///     | "{" type "=" type "}"
    fn compoundType(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        _ = p;
        _ = skip_nl;
        _ = level;
        @panic("TODO compoundType");
    }

    /// stmt
    ///     : decl
    ///     | assign_expr
    fn stmt(p: *Parser, level: u8) Error!Node.Index {
        if (try p.decl(level)) |node| return node;
        return p.assignExpr(.skip_nl, level);
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
                const rhs = try p.blockOrExpr(skip_nl, level);

                return try p.addNode(switch (p.tok_ids[tok]) {
                    .equal => .assign,
                    .plus_equal => .add_assign,
                    .minus_equal => .sub_assign,
                    .asterisk_equal => .mul_assign,
                    .asterisk_asterisk_equal => .pow_assign,
                    .slash_equal => .div_assign,
                    .slash_slash_equal => .div_floor_assign,
                    .percent_equal => .mod_assign,
                    .l_arr_arr_equal => .l_shift_assign,
                    .r_arr_arr_equal => .r_shift_assign,
                    .ampersand_equal => .bit_and_assign,
                    .pipe_equal => .bit_or_assign,
                    .caret_equal => .bit_x_or_assign,
                    else => unreachable,
                }, tok, .{ .bin = .{ .lhs = lhs, .rhs = rhs } });
            },
            else => return lhs,
        }
    }

    /// block_or_expr : block | expr
    fn blockOrExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        if (skip_nl == .keep_nl) if (try p.block(level)) |node| return node;
        p.skipNl();
        return try p.expr(skip_nl, level);
    }

    /// block : NL (stmt NL)+
    fn block(p: *Parser, level: u8) Error!?Node.Index {
        const nl = p.eatToken(.nl, false) orelse return null;
        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        const new_level = indent: {
            const indent = p.eatIndentExtra();
            if (indent == null or indent.? <= level)
                return p.reportErr("expected indentation", p.tok_i);

            break :indent indent.?;
        };

        var last_nl = p.tok_i;
        while (true) {
            try p.node_buf.append(try p.stmt(new_level));
            last_nl = p.eatToken(.nl, false) orelse {
                last_nl = try p.expectToken(.eof, false);
                break;
            };
            if (!try p.eatIndent(new_level)) break;
        }

        // reset to previous new line since all statements are expected to end in a newline
        p.tok_index = last_nl;

        const stmts = p.node_buf.items[node_buf_top..];
        switch (stmts.len) {
            0 => return p.addBin(.block_stmt_two, nl, .none, .none),
            1 => return p.addBin(.block_stmt_two, nl, stmts[0], .none),
            2 => return p.addBin(.block_stmt_two, nl, stmts[0], stmts[1]),
            else => return p.addList(.block_stmt, nl, stmt),
        }
    }

    /// expr
    ///     : jump_expr
    ///     | lambda
    ///     | bool_expr
    fn expr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        if (try p.jumpExpr(skip_nl, level)) |node| return node;
        if (try p.lambda(skip_nl, level)) |node| return node;
        return p.boolExpr(skip_nl, level);
    }

    /// jump_expr : "return" block_or_expr? | "break" | "continue" | "throw" block_or_expr
    fn jumpExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.tok_i;
        const id: Node.Id = switch (p.tok_ids[tok]) {
            .keyword_return => .return_expr,
            .keyword_break => .break_expr,
            .keyword_continue => .continue_expr,
            .keyword_throw => .throw_expr,
            else => return null,
        };
        p.tok_i += 1;

        const op = if (op == .throw_expr)
            try p.blockOrExpr(skip_nl, level)
        else if (op == .keyword_return and switch (p.tok_ids[p.tok_i].id) {
            .eof, .nl, .r_paren, .r_brace, .r_bracket, .keyword_else, .keyword_catch, .comma, .colon => false,
            else => true,
        })
            try p.blockOrExpr(skip_nl, level)
        else
            .none;

        return p.addUn(id, tok, op);
    }

    /// lambda : "fn" "(" (bool_expr ",")* bool_expr? ")" block_or_expr
    fn lambda(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        _ = p;
        _ = skip_nl;
        _ = level;
        @panic("TODO lambda");
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
                lhs = try p.addBin(.bool_andexpr, tok, lhs, try p.comparisonExpr(skip_nl, level));
            }
        }
        return lhs;
    }

    /// comparison_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr)?
    ///     | range_expr ("is" type)?
    fn comparisonExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const lhs = try p.rangeExpr(skip_nl, level);

        // we can safely skip any newlines here
        const start = p.tok_i;
        p.skipNl();
        const tok = p.nextToken(true);
        const id = p.tok_ids[tok];
        switch (id) {
            .l_arr => return p.addBin(.less_than_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .l_arr_equal => return p.addBin(.less_than_equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .r_arr => return p.addBin(.greater_than_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .r_arr_equal => return p.addBin(.greater_than_equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .equal_equal => return p.addBin(.equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .bang_equal => return p.addBin(.not_equal_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .keyword_in => return p.addBin(.in_expr, tok, lhs, try p.rangeExpr(skip_nl, level)),
            .keyword_is => return p.addBin(.is_expr, tok, lhs, try p.Type(skip_nl, level)),
            else => {
                p.tok_i = start;
                return lhs;
            },
        }
    }

    /// range_expr : bit_expr? (":" bit_expr? (":" bit_expr)?)?
    fn rangeExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var start: Node.Index = .none;
        var colon_1 = p.eatToken(.colon, skip_nl);

        if (colon_1 == null) {
            start = try p.bitExpr(skip_nl, level);
            colon_1 = p.eatToken(.colon, skip_nl);
            // not a range
            if (colon_1 == null) return start;
        }

        var end: Node.Index = .none;
        var colon_2 = p.eatToken(.colon, skip_nl);
        if (colon_2 == null) {
            switch (p.tok_ids[p.tok_i]) {
                .eof, .nl, .r_paren, .r_brace, .r_bracket, .keyword_else, .keyword_catch, .comma, .colon => {},
                else => end = try p.bitExpr(skip_nl, level),
            }
            colon_2 = p.eatToken(.colon, skip_nl);
        }

        var step: Node.Index = .none;
        if (colon_2 != null) {
            step = try p.bitExpr(skip_nl, level);
        }

        if (start != .none and end != .none and step != .none) {
            return p.addCond(.range_expr, colon_1.?, start, &.{ end, step });
        } else if (start == .none) {
            return p.addBin(.range_start, colon_1.?, end, step);
        } else if (end == .none) {
            return p.addBin(.range_end, colon_1.?, start, step);
        } else {
            return p.addBin(.range_step, colon_1.?, start, end);
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
                lhs = try p.addBin(.bit_or, tok, lhs, try p.shiftExpr(skip_nl, level));
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
        const tok = p.nextToken(true);
        switch (p.tok_ids[tok]) {
            .l_arr_arr => {
                p.skipNl();
                return p.addBin(.l_shift, tok, lhs, try p.addExpr(skip_nl, level));
            },
            .r_arr_arr => {
                p.skipNl();
                return p.addBin(.r_shift, tok, lhs, try p.addExpr(skip_nl, level));
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
                    lhs = try p.addNode(.sub_expr, tok, lhs, try p.mulExpr(skip_nl, level));
                },
                .plus => {
                    p.skipNl();
                    lhs = try p.addNode(.add_expr, tok, lhs, try p.mulExpr(skip_nl, level));
                },
                else => {
                    p.tok_index = tok;
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
            const tok = p.nextToken(true);
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
                    lhs = try p.addBin(.mod_expr, tok, lhs, try p.castExpr(skip_nl, level));
                },
                else => {
                    p.tok_i = start;
                    return lhs;
                },
            }
        }

        return lhs;
    }

    /// cast_expr : prefix_expr ("as" type)?
    fn castExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var lhs = try p.prefixExpr(skip_nl, level);

        if (p.eatTokenNoNl(.keyword_as)) |tok| {
            p.skipNl();
            lhs = try p.addBin(.as_expr, tok, lhs, try p.Type(skip_nl, level));
        }

        return lhs;
    }

    /// prefix_expr : ("-" | "+" | "~")? power_expr
    fn prefixExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const tok = p.nextToken(skip_nl);
        switch (p.tok_ids[tok]) {
            .minus => {
                p.skipNl();
                return p.addBin(.negate_expr, tok, lhs, try p.powerExpr(skip_nl, level));
            },
            .plus => {
                p.skipNl();
                return p.addBin(.plus_expr, tok, lhs, try p.powerExpr(skip_nl, level));
            },
            .tilde => {
                p.skipNl();
                return p.addBin(.bit_not_expr, tok, lhs, try p.powerExpr(skip_nl, level));
            },
            else => {
                p.tok_i = tok;
                return try p.powerExpr(skip_nl, level);
            },
        }
    }

    /// power_expr : primary_expr suffix_expr* ("**" power_expr)?
    fn powerExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var primary = try p.primaryExpr(skip_nl, level);
        primary = try p.suffixExpr(primary, skip_nl, level);
        if (p.eatTokenNoNl(.asterisk_asterisk)) |tok| {
            p.skipNl();
            primary = try p.addBin(.pow_expr, tok, primary, try p.powerExpr(skip_nl, level));
        }
        return primary;
    }

    /// suffix_expr
    ///     : "[" expr "]"
    ///     | "(" (expr ",")* expr? ")"
    ///     | "." IDENTIFIER
    fn suffixExpr(p: *Parser, primary: Node.Index, skip_nl: SkipNl, level: u8) Error!Node.Index {
        var lhs = primary;
        while (true) {
            if (p.eatToken(.l_bracket, skip_nl)) |tok| {
                p.skipNl();
                lhs = try p.addBin(.array_access_expr, tok, lhs, try p.expr(.skip_nl, level));
                _ = try p.expectToken(.RBracket, false);
            } else if (p.eatToken(.l_paren, true)) |tok| {
                const args = try p.listParser(skip_nl, level, expr, .RParen, lhs);
                switch (args.len) {
                    0 => unreachable, // we pass lhs as first
                    1 => lhs = try p.addUn(.call_expr_one, tok, lhs, .none),
                    2 => lhs = try p.addUn(.call_expr_one, tok, lhs, args[1]),
                    else => lhs = try p.addList(.call_expr, tok, args),
                }
            } else if (p.eatTokenNoNl(.period)) |_| {
                p.skipNl();
                const ident = try p.expectToken(.identifier, skip_nl);
                lhs = try p.addUn(.member_access_expr, ident, lhs);
            } else {
                return lhs;
            }
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
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    ///     | try
    fn primaryExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!Node.Index {
        const tok = p.nextToken(false);
        switch (p.tok_ids[tok]) {
            .identifier => {
                if (skip_nl) p.skipNl();
                return p.addUn(.decl_ref_expr, tok, .none);
            },
            .string => {
                if (skip_nl) p.skipNl();
                return p.addUn(.string_expr, tok, .none);
            },
            .number => {
                if (skip_nl) p.skipNl();
                return p.addUn(.num_expr, tok, .none);
            },
            .integer => {
                if (skip_nl) p.skipNl();
                return p.addUn(.int_expr, tok, .none);
            },
            .keyword_true => {
                if (skip_nl) p.skipNl();
                return p.addUn(.true_expr, tok, .none);
            },
            .keyword_false => {
                if (skip_nl) p.skipNl();
                return p.addUn(.false_expr, tok, .none);
            },
            .keyword_null => {
                if (skip_nl) p.skipNl();
                return p.addUn(.null_expr, tok, .none);
            },
            .Keyword_this => {
                if (skip_nl) p.skipNl();
                return p.addUn(.this_expr, tok, .none);
            },
            .keyword_error => {
                if (skip_nl) p.skipNl();
                return p.addUn(.error_expr, tok, try p.initializer(skip_nl, level));
            },
            .At => {
                p.skipNl();
                const ident = try p.expectToken(.identifier, skip_nl);
                return p.addUn(.error_expr, ident, try p.initializer(skip_nl, level));
            },
            else => {
                p.tok_index = tok;
            },
        }
        if (try p.formatString(skip_nl, level)) |res| return res;
        if (try p.initializer(skip_nl, level)) |res| return res;
        if (try p.ifExpr(skip_nl, level)) |res| return res;
        if (try p.whileExpr(skip_nl, level)) |res| return res;
        if (try p.forExpr(skip_nl, level)) |res| return res;
        if (try p.matchExpr(skip_nl, level)) |res| return res;
        if (try p.tryExpr(skip_nl, level)) |res| return res;
        return p.reportErr("expected Identifier, String, Number, true, false, '(', '{{', '[', error, try, import, if, while, for or match", p.tokens[p.tok_index]);
    }

    /// format_string : FORMAT_START expr (FORMAT expr)* FORMAT_END
    fn formatString(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const first = p.eatToken(.format_start, true) orelse return null;

        var toks = std.ArrayList(TokenIndex).init(p.gpa);
        defer toks.deinit();
        try toks.append(first);

        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        while (true) {
            try p.node_buf.append(try p.expr(true, true, level));

            if (p.eatToken(.format, true)) |tok| {
                try toks.append(tok);
            } else break;
        }
        try toks.append(try p.expectToken(.format_end, skip_nl));

        const fmt_start = @intCast(u32, p.extra.items.len);
        try p.extra.appendSlice(@bitCast([]Node.Index, toks.items));
        const args_start = @intCast(u32, p.extra.items.len);
        try p.extra.appendSlice(p.node_buf[node_buf_top..]);

        const index = p.nodes.len;
        try p.nodes.append(.{
            .id = .format_expr,
            .token = first,
            .data = .{ .format = .{ .fmt_start = fmt_start, .args_start = args_start } },
        });
        return @intToEnum(Node.Index, index);
    }

    /// initializer
    ///     : "(" block_or_expr ")"
    ///     | "(" (expr ",")+ expr? ")"
    ///     | "{" (expr "=" expr ",")* (expr "=" expr)? "}"
    ///     | "[" (expr ",")* expr? "]"
    fn initializer(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        if (p.eatToken(.l_brace, true)) |tok| {
            const elems = try p.listParser(skip_nl, level, mapItem, .r_brace, null);
            switch (elems.len) {
                0 => return p.addBin(.map_expr_two, tok, .none, .none),
                1 => return p.addBin(.map_expr_two, tok, elems[0], .none),
                2 => return p.addBin(.map_expr_two, tok, elems[0], elems[1]),
                else => return p.addList(.map_expr, tok, elems),
            }
        } else if (p.eatToken(.l_bracket, true)) |tok| {
            const elems = try p.listParser(skip_nl, level, expr, .r_bracket, null);
            switch (elems.len) {
                0 => return p.addBin(.list_expr_two, tok, .none, .none),
                1 => return p.addBin(.list_expr_two, tok, elems[0], .none),
                2 => return p.addBin(.list_expr_two, tok, elems[0], elems[1]),
                else => return p.addList(.list_expr, tok, elems),
            }
        } else if (p.eatToken(.l_paren, false)) |tok| {
            if (try p.block(level)) |b| {
                p.skipNl();
                return p.addUn(.paren_expr, tok, b);
            }
            p.skipNl();
            const first = try p.expr(.skip_nl, level);
            if (p.eatToken(.RParen, skip_nl)) |_| {
                return p.addUn(.paren_expr, tok, b);
            }
            _ = try p.expectToken(.comma, true);

            const elems = try p.listParser(skip_nl, level, expr, .RParen, first);
            switch (elems.len) {
                0 => return p.addBin(.tuple_expr_two, tok, .none, .none),
                1 => return p.addBin(.tuple_expr_two, tok, elems[0], .none),
                2 => return p.addBin(.tuple_expr_two, tok, elems[0], elems[1]),
                else => return p.addList(.tuple_expr, tok, elems),
            }
        } else return null;
    }

    /// expr ("=" expr)?
    fn mapItem(p: *Parser, _: SkipNl, level: u8) Error!Node.Index {
        var tok = p.tok_i;
        var key: Node.Index = .none;
        var value = try p.expr(.skip_nl, level);
        if (p.eatToken(.equal, true)) |eq| {
            tok = eq;
            key = value;
            value = try p.expr(.skip_nl, level);
        }
        try p.addBin(.map_item_expr, tok, key, value);
    }

    /// (PARSE_FN ",")* PARSE_FN? TERM
    fn listParser(
        p: *Parser,
        skip_nl: SkipNl,
        level: u8,
        parseFn: fn (*Parser, SkipNl, u8) Error!Node.Index,
        term_id: Token.Id,
        first: ?Node.Index,
    ) Error![]Node.Index {
        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;
        if (first) |some| try p.node_buf.append(some);

        var end = false;
        while (true) {
            if (p.eatToken(term_id, skip_nl)) |term_tok| {
                term = term_tok;
                break;
            } else if (end) {
                term = try p.expectToken(term_id, skip_nl);
                break;
            }
            try p.node_buf.append(try parseFn(p, .skip_nl, level));
            if (p.eatToken(.comma, true) == null) end = true;
        }
        return p.node_buf[node_buf_top..];
    }

    /// if : "if" ("let" destructuring "=")? expr block_or_expr ("else" block_or_expr)?
    fn ifExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_if, true) orelse return null;
        const let = p.eatToken(.keyword_let, true);
        const dest = if (let) |_| try p.destructuring(level) else .none;
        if (let) |_| _ = try p.expectToken(.equal, true);
        const cond = try p.expr(.keep_nl, level);
        const then_body = try p.blockOrExpr(skip_nl, level);
        const @"else" = p.eatTokenNoNl(.keyword_else);
        const else_body = if (@"else") |_| try p.blockOrExpr(skip_nl, level) else .none;

        if (dest == .none and else_body == .none) {
            return p.addBin(.if_expr, tok, cond, then_body);
        } else if (dest == .none) {
            return p.addCond(.if_else_expr, tok, cond, &.{ then_body, else_body });
        } else if (else_body == .none) {
            return p.addCond(.if_let_expr, tok, cond, &.{ dest, then_body });
        } else {
            return p.addCond(.if_let_else_expr, tok, cond, &.{ dest, then_body, else_body });
        }
    }

    /// while : "while" ("let" destructuring "=")? expr block_or_expr
    fn whileExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_while, .skip_nl) orelse return null;
        const let = p.eatToken(.keyword_let, .skip_nl);
        const dest = if (let) |_| try p.destructuring(level) else .none;
        if (let) |_| _ = try p.expectToken(.equal, true);
        const cond = try p.expr(.keep_nl, level);
        const body = try p.blockOrExpr(skip_nl, level);

        if (dest == .none) {
            return p.addBin(.while_expr, tok, cond, body);
        } else {
            return p.addCond(.while_let_expr, tok, cond, &.{ dest, body });
        }
    }

    /// for : "for" ("let" destructuring "in")? expr block_or_expr
    fn forExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_for, .skip_nl) orelse return null;
        const let = p.eatToken(.keyword_let, .skip_nl);
        const dest = if (let) |_| try p.destructuring(level) else .none;
        if (let) |_| _ = try p.expectToken(.keyword_in, true);
        const cond = try p.expr(.keep_nl, level);
        const body = try p.blockOrExpr(skip_nl, level);

        if (dest == .none) {
            return p.addBin(.for_expr, tok, cond, body);
        } else {
            return p.addCond(.for_let_expr, tok, cond, &.{ dest, body });
        }
    }

    /// match : "match" expr (NL match_case)+ NL
    fn matchExpr(p: *Parser, _: bool, level: u8) Error!?Node.Index {
        const tok = p.eatToken(.keyword_match, .skip_nl) orelse return null;
        const cond = try p.expr(.keep_nl, level);
        _ = try p.expectToken(.nl, false);

        const node_buf_top = p.node_buf.items.len;
        defer p.node_buf.items.len = node_buf_top;

        // in case we need it in match_expr
        try p.node_buf.append(cond);

        const new_level = indent: {
            const indent = p.eatIndentExtra();
            if (indent == null or indent.? <= level)
                return p.reportErr("expected indentation", p.tok_i);

            break :indent indent.?;
        };

        var last_nl = p.tok_index;
        while (true) {
            try p.node_buf.append(try p.matchCase(new_level));
            last_nl = p.eatToken(.nl, false) orelse {
                last_nl = try p.expectToken(.eof, false);
                break;
            };
            if (!try p.eatIndent(new_level)) break;
        }

        // reset to previous new line since all statements are expected to end in a newline
        p.tok_i = last_nl;

        const cases = p.node_buf[node_buf_top..];
        switch (cases.len) {
            0 => unreachable, // must have at least one case
            1 => unreachable, // cases[0] == cond
            2 => try p.addBin(.match_expr_one, tok, cond, cases[1]),
            else => try p.addList(.match_expr, tok, cases),
        }
    }

    /// match_case
    ///    : "let" destructuring "=>" block_or_expr
    ///    | expr ("," expr)* ","? "=>" block_or_expr
    fn matchCase(p: *Parser, level: u8) Error!Node.Index {
        if (p.eatToken(.keyword_let, .skip_nl)) |_| {
            const dest = try p.primaryExpr(.skip_nl, level);
            const arr = try p.expectToken(.equal_rarr, .keep_nl);
            const body = try p.blockOrExpr(.keep_nl, level);
            return p.addBin(.match_case_let, arr, dest, body);
        } else if (p.eatToken(.underscore, .skip_nl)) |_| {
            const arr = try p.expectToken(.EqualRarr, .keep_nl);
            return p.addUn(.match_case_catch_all, arr, try p.blockOrExpr(.keep_nl, level));
        } else {
            const start = p.tok_i;
            const node_buf_top = p.node_buf.items.len;
            defer p.node_buf.items.len = node_buf_top;

            const items = try p.listParser(.skip_nl, level, expr, .equal_rarr, null);
            // TODO should be =>
            const arr = start;

            p.node_buf.items.len += items.len;
            try p.node_buf.append(try p.blockOrExpr(.keep_nl, level));

            switch (items.len) {
                0 => unreachable,
                1 => return p.reportErr("expected at least one item in match case", start),
                2 => return p.addBin(.match_case_one, arr, items[0], items[1]),
                else => return p.addList(.match_case, arr, items),
            }
        }
    }

    /// try : "try" block_or_expr ("catch" ("let" destructuring | expr)? block_or_expr)*
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
            1 => return p.addBin(.try_one_expr, tok, body, .none),
            2 => return p.addBin(.try_one_expr, tok, body, catches[1]),
            else => return p.addList(.try_expr, tok, catches),
        }
    }

    /// "catch" ("let" destructuring | expr)? block_or_expr
    fn catchExpr(p: *Parser, skip_nl: SkipNl, level: u8) Error!?Node.Index {
        const tok = p.eatTokenNoNl(.keyword_catch) orelse return null;

        if (p.eatToken(.keyword_let, .skip_nl)) |_| {
            return p.addBin(.catch_let_expr, tok, try p.destructuring(level), try p.blockOrExpr(skip_nl, level));
        } else {
            return p.addBin(.catch_let_expr, tok, try p.expr(.keep_nl, level), try p.blockOrExpr(skip_nl, level));
        }
    }

    fn reportErr(p: *Parser, msg: []const u8, tok: Token.Index) Error {
        try p.errors.add(.{ .data = msg }, p.tok_start[tok], .err);
        return error.ParseError;
    }

    /// skips nl begins and ends
    fn skipNl(p: *Parser) void {
        _ = p.eatToken(.nl, .skip_nl);
    }

    fn eatIndent(p: *Parser, level: u8) !bool {
        const indent = p.eatIndentExtra() orelse return false;
        if (indent > level) return p.reportErr("unexpected indentation", p.tokens[p.tok_i]);
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
                .indent_1,
                .indent_2,
                .indent_3,
                .indent_4,
                .indent_5,
                .indent_6,
                .indent_7,
                .indent_8,
                .indent_9,
                .indent_10,
                .indent_11,
                .indent_12,
                .indent_13,
                .indent_14,
                .indent_15,
                .indent_16,
                .indent_17,
                .indent_18,
                .indent_19,
                .indent_20,
                .indent_21,
                .indent_22,
                .indent_23,
                .indent_24,
                .indent_25,
                .indent_26,
                .indent_27,
                .indent_28,
                .indent_29,
                .indent_30,
                .indent_31,
                .indent_32,
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
        // try p.errors.add(try bog.Value.String.init(
        //     p.gpa,
        //     "expected '{s}', found '{s}'",
        //     .{ Token.string(id), Token.string(tok.id) },
        // ), p.tokens[p.tok_i], .err);
        return error.ParseError;
    }
};
