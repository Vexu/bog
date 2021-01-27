const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Token = bog.Token;
const TokenIndex = Token.Index;
const TokenId = @TagType(Token.Id);
const Tree = bog.Tree;
const Node = bog.Node;
const NodeList = std.ArrayList(*Node);

/// root : (stmt NL)* EOF
pub fn parse(gpa: *Allocator, source: []const u8, errors: *bog.Errors) (Parser.Error || bog.Tokenizer.Error)!*Tree {
    const tokens = try bog.tokenize(gpa, source, errors);
    errdefer gpa.free(tokens);

    var arena = std.heap.ArenaAllocator.init(gpa);
    errdefer arena.deinit();

    var parser = Parser{
        .arena = &arena.allocator,
        .gpa = gpa,
        .errors = errors,
        .tokens = tokens,
    };
    var nodes = NodeList.init(gpa);
    defer nodes.deinit();

    while (true) switch (parser.tokens[parser.tok_index].id) {
        .Comment, .Nl => parser.tok_index += 1,
        else => break,
    };

    while (true) {
        _ = try parser.eatIndent(0);
        if (parser.eatToken(.Eof, false)) |_| break;
        try nodes.append(try parser.stmt(0));
        _ = parser.eatToken(.Nl, false) orelse {
            _ = try parser.expectToken(.Eof, false);
            break;
        };
    }

    const tree = try parser.arena.create(Tree);
    tree.* = .{
        .tokens = tokens,
        .nodes = try parser.arena.dupe(*Node, nodes.items),
        .source = source,
        .arena = arena.state,
        .gpa = gpa,
    };
    return tree;
}

pub fn parseRepl(repl: *@import("repl.zig").Repl) Parser.Error!?*Node {
    var parser = Parser{
        .arena = repl.compiler.arena,
        .gpa = repl.compiler.gpa,
        .errors = &repl.vm.errors,
        .tokens = repl.tokenizer.tokens.items,
        .tok_index = repl.tok_index,
    };
    defer repl.tok_index = @intCast(u32, parser.tokens.len - 1);

    if (parser.eatToken(.Eof, true)) |_| return null;
    const ret = try parser.stmt(0);
    _ = try parser.expectToken(.Nl, true);
    _ = try parser.expectToken(.Eof, true);
    return ret;
}

pub const Parser = struct {
    arena: *Allocator,
    gpa: *Allocator,
    errors: *bog.Errors,
    tokens: []const Token,
    tok_index: TokenIndex = 0,

    pub const Error = error{ParseError} || Allocator.Error;

    /// stmt
    ///     : decl "=" block_or_expr
    ///     | expr
    fn stmt(parser: *Parser, level: u16) Error!*Node {
        if (try parser.decl(level)) |node| return node;
        return parser.expr(false, true, level);
    }

    /// decl : ("let" | "const") bool_expr
    fn decl(parser: *Parser, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true) orelse return null;
        const capture = try parser.boolExpr(true, true, level);
        const eq_tok = try parser.expectToken(.Equal, false);
        const value = try parser.blockOrExpr(false, true, level);
        const node = try parser.arena.create(Node.Decl);
        node.* = .{
            .let_const = tok,
            .capture = capture,
            .eq_tok = eq_tok,
            .value = value,
        };
        return &node.base;
    }

    /// expr
    ///     : fn
    ///     | jump_expr
    ///     | bool_expr assign?
    fn expr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        if (try parser.func(skip_nl, allow_range, level)) |node| return node;
        if (try parser.jumpExpr(skip_nl, allow_range, level)) |node| return node;
        return parser.assign(try parser.boolExpr(skip_nl, allow_range, level), skip_nl, allow_range, level);
    }

    /// block_or_expr : block | expr
    fn blockOrExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        if (!skip_nl) if (try parser.block(level)) |node| return node;
        parser.skipNl();
        return try parser.expr(skip_nl, allow_range, level);
    }

    /// fn : "fn" "(" (bool_expr ",")* bool_expr? ")" block_or_expr
    fn func(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_fn, true) orelse return null;

        _ = try parser.expectToken(.LParen, true);
        const params = try parser.listParser(skip_nl, level, boolExpr, .RParen, null);
        const node = try parser.arena.create(Node.Fn);
        node.* = .{
            .fn_tok = tok,
            .params = params.nodes,
            .r_paren = params.term,
            .body = try parser.blockOrExpr(skip_nl, allow_range, level),
        };
        return &node.base;
    }

    /// jump_expr : "return" block_or_expr? | "break" | "continue"
    fn jumpExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.nextToken(false);
        const id = parser.tokens[tok].id;
        switch (id) {
            .Keyword_return, .Keyword_break, .Keyword_continue => {},
            else => {
                parser.tok_index = tok;
                return null;
            },
        }
        const res = if (id == .Keyword_return and switch (parser.tokens[parser.tok_index].id) {
            .Eof, .Nl, .RParen, .RBrace, .RBracket, .Keyword_else, .Comma, .Colon => false,
            else => true,
        }) try parser.blockOrExpr(skip_nl, allow_range, level) else null;
        const node = try parser.arena.create(Node.Jump);
        node.* = .{
            .tok = tok,
            .op = switch (id) {
                .Keyword_return => .{ .Return = res },
                .Keyword_break => .Break,
                .Keyword_continue => .Continue,
                else => unreachable,
            },
        };
        return &node.base;
    }

    /// bool_expr
    ///     : "not" comparison_expr
    ///     | comparison_expr ("or" comparison_expr)*
    ///     | comparison_expr ("and" comparison_expr)*
    fn boolExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        if (parser.eatToken(.Keyword_not, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Prefix);
            node.* = .{
                .op = .bool_not,
                .rhs = try parser.comparisonExpr(skip_nl, allow_range, level),
                .tok = tok,
            };
            return &node.base;
        }
        var lhs = try parser.comparisonExpr(skip_nl, allow_range, level);

        if (parser.eatTokenNoNl(.Keyword_or)) |t| {
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .bool_or,
                    .rhs = try parser.comparisonExpr(skip_nl, allow_range, level),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Keyword_or)) |tt| tok = tt else break;
            }
        } else {
            while (parser.eatTokenNoNl(.Keyword_and)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .bool_and,
                    .rhs = try parser.comparisonExpr(skip_nl, allow_range, level),
                };
                lhs = &node.base;
            }
        }
        return lhs;
    }

    /// comparison_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr)?
    ///     | range_expr ("is" type_name)?
    fn comparisonExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        const lhs = try parser.rangeExpr(skip_nl, allow_range, level);

        // we can safely skip any newlines here
        const start = parser.tok_index;
        parser.skipNl();
        const tok = parser.nextToken(true);
        const id = parser.tokens[tok].id;
        switch (id) {
            .LArr, .LArrEqual, .RArr, .RArrEqual, .EqualEqual, .BangEqual, .Keyword_in => {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = switch (id) {
                        .LArr => .less_than,
                        .LArrEqual => .less_than_equal,
                        .RArr => .greater_than,
                        .RArrEqual => .greater_than_equal,
                        .EqualEqual => .equal,
                        .BangEqual => .not_equal,
                        .Keyword_in => .in,
                        else => unreachable,
                    },
                    .rhs = try parser.rangeExpr(skip_nl, allow_range, level),
                };
                return &node.base;
            },
            .Keyword_is => {
                parser.skipNl();
                const node = try parser.arena.create(Node.TypeInfix);
                node.* = .{
                    .lhs = lhs,
                    .op = .is,
                    .tok = tok,
                    .type_tok = try parser.typeName(skip_nl),
                };
                return &node.base;
            },
            else => {
                parser.tok_index = start;
                return lhs;
            },
        }
    }

    /// type_name : "none" | "int" | "num" | "bool" | "str" | "tuple" | "map" | "list" | "error" | "range" | "fn"
    fn typeName(parser: *Parser, skip_nl: bool) Error!TokenIndex {
        return parser.eatToken(.Keyword_error, skip_nl) orelse
            parser.eatToken(.Keyword_fn, skip_nl) orelse
            parser.eatToken(.Identifier, skip_nl) orelse
            parser.reportErr("expected type name", parser.tokens[parser.tok_index]);
    }

    /// range_expr : bit_expr? (":" bit_expr? (":" bit_expr?)?)?
    fn rangeExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        if (!allow_range) {
            return parser.bitExpr(skip_nl, allow_range, level);
        }
        // This grammar would be much easier to implement with infinite lookahead
        // maybe that would be worth doing some time.
        var start: ?*Node = null;
        var colon_1 = parser.eatToken(.Colon, skip_nl);

        if (colon_1 == null) {
            start = try parser.bitExpr(skip_nl, false, level);
            colon_1 = parser.eatToken(.Colon, skip_nl);
            // not a range
            if (colon_1 == null) return start.?;
        }

        var end: ?*Node = null;
        var colon_2 = parser.eatToken(.Colon, skip_nl);
        if (colon_2 == null) {
            switch (parser.tokens[parser.tok_index].id) {
                .Eof, .Nl, .RParen, .RBrace, .RBracket, .Keyword_else, .Comma, .Colon => {},
                else => end = try parser.bitExpr(skip_nl, false, level),
            }
            colon_2 = parser.eatToken(.Colon, skip_nl);
        }

        var step: ?*Node = null;
        if (colon_2 != null) {
            switch (parser.tokens[parser.tok_index].id) {
                .Eof, .Nl, .RParen, .RBrace, .RBracket, .Keyword_else, .Comma, .Colon => {},
                else => step = try parser.bitExpr(skip_nl, false, level),
            }
        }

        const node = try parser.arena.create(Node.Range);
        node.* = .{
            .start = start,
            .end = end,
            .step = step,
            .colon_1 = colon_1.?,
            .colon_2 = colon_2,
        };
        return &node.base;
    }

    /// bit_expr : shift_expr (("&" shift_expr)* | ("|" shift_expr)* | ("^" shift_expr)*
    fn bitExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        var lhs = try parser.shiftExpr(skip_nl, allow_range, level);

        if (parser.eatTokenNoNl(.Ampersand)) |t| {
            // &
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .bit_and,
                    .rhs = try parser.shiftExpr(skip_nl, allow_range, level),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Ampersand)) |tt| tok = tt else break;
            }
        } else if (parser.eatTokenNoNl(.Pipe)) |t| {
            // |
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .bit_or,
                    .rhs = try parser.shiftExpr(skip_nl, allow_range, level),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Pipe)) |tt| tok = tt else break;
            }
        } else if (parser.eatTokenNoNl(.Caret)) |t| {
            // ^
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .bit_xor,
                    .rhs = try parser.shiftExpr(skip_nl, allow_range, level),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Caret)) |tt| tok = tt else break;
            }
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr)
    fn shiftExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        const lhs = try parser.addExpr(skip_nl, allow_range, level);

        // we can safely skip any newlines here
        const start = parser.tok_index;
        parser.skipNl();
        const tok = parser.nextToken(true);
        const id = parser.tokens[tok].id;
        switch (id) {
            .LArrArr, .RArrArr => {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = if (id == .LArrArr) .l_shift else .r_shift,
                    .rhs = try parser.addExpr(skip_nl, allow_range, level),
                };
                return &node.base;
            },
            else => {
                parser.tok_index = start;
                return lhs;
            },
        }
    }

    /// add_expr : mul_expr (("-" | "+" | "++") mul_expr)*
    fn addExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        var lhs = try parser.mulExpr(skip_nl, allow_range, level);

        while (true) {
            const tok = parser.nextToken(skip_nl);
            const id = parser.tokens[tok].id;
            switch (id) {
                .Minus, .Plus, .PlusPlus => {
                    parser.skipNl();
                    const node = try parser.arena.create(Node.Infix);
                    node.* = .{
                        .lhs = lhs,
                        .tok = tok,
                        .op = switch (id) {
                            .Minus => .sub,
                            .Plus => .add,
                            .PlusPlus => .append,
                            else => unreachable,
                        },
                        .rhs = try parser.mulExpr(skip_nl, allow_range, level),
                    };
                    lhs = &node.base;
                },
                else => {
                    parser.tok_index = tok;
                    return lhs;
                },
            }
        }
    }

    /// mul_expr : cast_expr (("*" | "/" | "//" | "%") cast_expr)*
    fn mulExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        var lhs = try parser.castExpr(skip_nl, allow_range, level);

        while (true) {
            // we can safely skip any newlines here
            const start = parser.tok_index;
            parser.skipNl();
            const tok = parser.nextToken(true);
            const id = parser.tokens[tok].id;
            switch (id) {
                .Asterisk, .Slash, .SlashSlash, .Percent => {
                    parser.skipNl();
                    const node = try parser.arena.create(Node.Infix);
                    node.* = .{
                        .lhs = lhs,
                        .tok = tok,
                        .op = switch (id) {
                            .Asterisk => .mul,
                            .Slash => .div,
                            .SlashSlash => .div_floor,
                            .Percent => .mod,
                            else => unreachable,
                        },
                        .rhs = try parser.castExpr(skip_nl, allow_range, level),
                    };
                    lhs = &node.base;
                },
                else => {
                    parser.tok_index = start;
                    return lhs;
                },
            }
        }

        return lhs;
    }

    /// cast_expr : prefix_expr ("as" type_name)?
    fn castExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        var lhs = try parser.prefixExpr(skip_nl, allow_range, level);

        if (parser.eatTokenNoNl(.Keyword_as)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.TypeInfix);
            node.* = .{
                .lhs = lhs,
                .op = .as,
                .tok = tok,
                .type_tok = try parser.typeName(skip_nl),
            };
            lhs = &node.base;
        }

        return lhs;
    }

    /// prefix_expr : ("-" | "+" | "~")? power_expr
    fn prefixExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        const tok = parser.nextToken(skip_nl);
        const id = parser.tokens[tok].id;
        switch (id) {
            .Minus, .Plus, .Tilde => {
                parser.skipNl();
                const node = try parser.arena.create(Node.Prefix);
                node.* = .{
                    .op = switch (id) {
                        .Minus => .minus,
                        .Plus => .plus,
                        .Tilde => .bit_not,
                        else => unreachable,
                    },
                    .tok = tok,
                    .rhs = try parser.powerExpr(skip_nl, allow_range, level),
                };
                return &node.base;
            },
            else => {
                parser.tok_index = tok;
                return try parser.powerExpr(skip_nl, allow_range, level);
            },
        }
    }

    /// power_expr : primary_expr suffix_expr* ("**" power_expr)?
    fn powerExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        var primary = try parser.primaryExpr(skip_nl, allow_range, level);
        primary = try parser.suffixExpr(primary, skip_nl, level);
        if (parser.eatTokenNoNl(.AsteriskAsterisk)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = primary,
                .tok = tok,
                .op = .pow,
                .rhs = try parser.powerExpr(skip_nl, allow_range, level),
            };
            return &node.base;
        }
        return primary;
    }

    /// suffix_expr
    ///     : "[" expr "]"
    ///     | "(" (expr ",")* expr? ")"
    ///     | "." IDENTIFIER
    fn suffixExpr(parser: *Parser, primary: *Node, skip_nl: bool, level: u16) Error!*Node {
        var lhs = primary;
        while (true) {
            if (parser.eatToken(.LBracket, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .subscript = try parser.expr(true, true, level) },
                    .r_tok = try parser.expectToken(.RBracket, false),
                };
                lhs = &node.base;
            } else if (parser.eatToken(.LParen, true)) |tok| {
                const args = try parser.listParser(skip_nl, level, expr, .RParen, null);
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .call = args.nodes },
                    .r_tok = args.term,
                };
                lhs = &node.base;
            } else if (parser.eatTokenNoNl(.Period)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .member,
                    .r_tok = try parser.expectToken(.Identifier, false),
                };
                lhs = &node.base;
            } else {
                return lhs;
            }
        }
    }

    /// assign : ("=" | "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") block_or_expr
    fn assign(parser: *Parser, lhs: *Node, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        const tok = parser.nextToken(false);
        const id = parser.tokens[tok].id;
        switch (id) {
            .Equal,
            .PlusEqual,
            .MinusEqual,
            .AsteriskEqual,
            .AsteriskAsteriskEqual,
            .SlashEqual,
            .SlashSlashEqual,
            .PercentEqual,
            .LArrArrEqual,
            .RArrArrEqual,
            .AmpersandEqual,
            .PipeEqual,
            .CaretEqual,
            => {
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = switch (id) {
                        .Equal => .assign,
                        .PlusEqual => .add_assign,
                        .MinusEqual => .sub_assign,
                        .AsteriskEqual => .mul_assign,
                        .AsteriskAsteriskEqual => .pow_assign,
                        .SlashEqual => .div_assign,
                        .SlashSlashEqual => .div_floor_assign,
                        .PercentEqual => .mod_assign,
                        .LArrArrEqual => .l_shift_assign,
                        .RArrArrEqual => .r_shift_assign,
                        .AmpersandEqual => .bit_and_assign,
                        .PipeEqual => .bit_or_assign,
                        .CaretEqual => .bit_x_or_assign,
                        else => unreachable,
                    },
                    .rhs = try parser.blockOrExpr(skip_nl, allow_range, level),
                };
                return &node.base;
            },
            else => {
                parser.tok_index = tok;
                return lhs;
            },
        }
    }

    /// primary_expr
    ///     : IDENTIFIER
    ///     | "_"
    ///     | STRING
    ///     | format_string
    ///     | NUMBER
    ///     | "true"
    ///     | "false"
    ///     | "(" ")"
    ///     | initializer
    ///     | "error" initializer?
    ///     | "@" IDENTIFIER initializer?
    ///     | "import" "(" STRING ")"
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    ///     | try
    fn primaryExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!*Node {
        const tok = parser.nextToken(false);
        const id = parser.tokens[tok].id;
        switch (id) {
            .Number, .Integer, .String, .Keyword_true, .Keyword_false => {
                if (skip_nl) parser.skipNl();
                const node = try parser.arena.create(Node.Literal);
                node.* = .{
                    .tok = tok,
                    .kind = switch (id) {
                        .Number => .num,
                        .Integer => .int,
                        .String => .str,
                        .Keyword_true => .True,
                        .Keyword_false => .False,
                        else => unreachable,
                    },
                };
                return &node.base;
            },
            .Identifier => {
                if (skip_nl) parser.skipNl();
                const node = try parser.arena.create(Node.SingleToken);
                node.* = .{
                    .base = .{ .id = .Identifier },
                    .tok = tok,
                };
                return &node.base;
            },
            .Keyword_this => {
                if (skip_nl) parser.skipNl();
                const node = try parser.arena.create(Node.SingleToken);
                node.* = .{
                    .base = .{ .id = .This },
                    .tok = tok,
                };
                return &node.base;
            },
            .Underscore => {
                if (skip_nl) parser.skipNl();
                const node = try parser.arena.create(Node.SingleToken);
                node.* = .{
                    .base = .{ .id = .Discard },
                    .tok = tok,
                };
                return &node.base;
            },
            .Keyword_error => {
                if (skip_nl) parser.skipNl();
                const node = try parser.arena.create(Node.Error);
                node.* = .{
                    .tok = tok,
                    .capture = try parser.initializer(skip_nl, level),
                };
                return &node.base;
            },
            .At => {
                parser.skipNl();
                const node = try parser.arena.create(Node.Tagged);
                node.* = .{
                    .at = tok,
                    .name = try parser.expectToken(.Identifier, skip_nl),
                    .capture = try parser.initializer(skip_nl, level),
                };
                return &node.base;
            },
            .Keyword_import => {
                _ = try parser.expectToken(.LParen, true);
                const node = try parser.arena.create(Node.Import);
                node.* = .{
                    .tok = tok,
                    .str_tok = try parser.expectToken(.String, true),
                    .r_paren = try parser.expectToken(.RParen, skip_nl),
                };
                return &node.base;
            },
            .LParen => {
                parser.skipNl();
                if (parser.eatToken(.RParen, skip_nl)) |rparen| {
                    const node = try parser.arena.create(Node.Literal);
                    node.* = .{
                        .tok = rparen,
                        .kind = .none,
                    };
                    return &node.base;
                } else {
                    parser.tok_index = tok;
                }
            },
            else => {
                parser.tok_index = tok;
            },
        }
        if (try parser.formatString(skip_nl, level)) |res| return res;
        if (try parser.initializer(skip_nl, level)) |res| return res;
        if (try parser.ifExpr(skip_nl, allow_range, level)) |res| return res;
        if (try parser.whileExpr(skip_nl, allow_range, level)) |res| return res;
        if (try parser.forExpr(skip_nl, allow_range, level)) |res| return res;
        if (try parser.matchExpr(skip_nl, level)) |res| return res;
        if (try parser.tryExpr(skip_nl, allow_range, level)) |res| return res;
        return parser.reportErr("expected Identifier, String, Number, true, false, '(', '{{', '[', error, try, import, if, while, for or match", parser.tokens[parser.tok_index]);
    }

    /// format_string : FORMAT_START expr (FORMAT expr)* FORMAT_END
    fn formatString(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const first = parser.eatToken(.FormatStart, true) orelse return null;

        var toks = std.ArrayList(TokenIndex).init(parser.gpa);
        defer toks.deinit();
        try toks.append(first);

        var args = NodeList.init(parser.gpa);
        defer args.deinit();

        while (true) {
            try args.append(try parser.expr(true, true, level));

            if (parser.eatToken(.Format, true)) |tok| {
                try toks.append(tok);
            } else break;
        }
        try toks.append(try parser.expectToken(.FormatEnd, skip_nl));

        const node = try parser.arena.create(Node.FormatString);
        node.* = .{
            .format = try parser.arena.dupe(TokenIndex, toks.items),
            .args = try parser.arena.dupe(*Node, args.items),
        };
        return &node.base;
    }

    /// initializer
    ///     : "(" block_or_expr ")"
    ///     | "(" (expr ",")+ expr? ")"
    ///     | "{" (expr_no_range ":" expr_no_range ",")* (expr_no_range ":" expr_no_range)? "}"
    ///     | "[" (expr ",")* expr? "]"
    fn initializer(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        if (parser.eatToken(.LBrace, true)) |tok| {
            const elems = try parser.listParser(skip_nl, level, mapItem, .RBrace, null);
            const node = try parser.arena.create(Node.ListTupleMap);
            node.* = .{
                .base = .{ .id = .Map },
                .l_tok = tok,
                .values = elems.nodes,
                .r_tok = elems.term,
            };
            return &node.base;
        } else if (parser.eatToken(.LBracket, true)) |tok| {
            const elems = try parser.listParser(skip_nl, level, expr, .RBracket, null);
            const node = try parser.arena.create(Node.ListTupleMap);
            node.* = .{
                .base = .{ .id = .List },
                .l_tok = tok,
                .values = elems.nodes,
                .r_tok = elems.term,
            };
            return &node.base;
        } else if (parser.eatToken(.LParen, false)) |tok| {
            if (try parser.block(level)) |b| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Grouped);
                node.* = .{
                    .l_tok = tok,
                    .expr = b,
                    .r_tok = try parser.expectToken(.RParen, skip_nl),
                };
                return &node.base;
            }
            parser.skipNl();
            const first = try parser.expr(true, true, level);
            if (parser.eatToken(.RParen, skip_nl)) |r_tok| {
                const node = try parser.arena.create(Node.Grouped);
                node.* = .{
                    .l_tok = tok,
                    .expr = first,
                    .r_tok = r_tok,
                };
                return &node.base;
            }
            _ = try parser.expectToken(.Comma, true);
            var elems = try parser.listParser(skip_nl, level, expr, .RParen, first);
            const node = try parser.arena.create(Node.ListTupleMap);
            node.* = .{
                .base = .{ .id = .Tuple },
                .l_tok = tok,
                .values = elems.nodes,
                .r_tok = elems.term,
            };
            return &node.base;
        } else return null;
    }

    /// expr (":" expr)?
    fn mapItem(parser: *Parser, skip_nl: bool, _: bool, level: u16) Error!*Node {
        var key: ?*Node = null;
        var value = try parser.expr(true, false, level);
        var colon: ?TokenIndex = null;
        if (parser.eatToken(.Colon, true)) |tok| {
            colon = tok;
            key = value;
            value = try parser.expr(true, false, level);
        }
        const item = try parser.arena.create(Node.MapItem);
        item.* = .{
            .key = key,
            .colon = colon,
            .value = value,
        };
        return &item.base;
    }

    const ListParserRes = struct {
        nodes: []*Node,
        term: TokenIndex,
    };

    /// (PARSE_FN ",")* PARSE_FN? TERM
    fn listParser(parser: *Parser, skip_nl: bool, level: u16, parseFn: fn (*Parser, bool, bool, u16) Error!*Node, term_id: TokenId, first: ?*Node) Error!ListParserRes {
        var nodes = NodeList.init(parser.gpa);
        defer nodes.deinit();
        if (first) |some| try nodes.append(some);

        var term: TokenIndex = parser.tok_index;
        var end = false;
        while (true) {
            if (parser.eatToken(term_id, skip_nl)) |term_tok| {
                term = term_tok;
                break;
            } else if (end) {
                term = try parser.expectToken(term_id, skip_nl);
                break;
            }
            try nodes.append(try parseFn(parser, true, true, level));
            if (parser.eatToken(.Comma, true) == null) end = true;
        }
        return ListParserRes{
            .nodes = try parser.arena.dupe(*Node, nodes.items),
            .term = term,
        };
    }

    /// block : NL BEGIN (stmt NL)+ END
    fn block(parser: *Parser, level: u16) Error!?*Node {
        _ = parser.eatToken(.Nl, false) orelse return null;
        var stmts = NodeList.init(parser.gpa);
        defer stmts.deinit();

        const indent = parser.eatToken(.Indent, false);
        if (indent == null or parser.tokens[indent.?].id.Indent <= level)
            return parser.reportErr("expected indentation", parser.tokens[parser.tok_index]);

        const new_level = parser.tokens[indent.?].id.Indent;
        var last_nl = parser.tok_index;
        while (true) {
            try stmts.append(try parser.stmt(new_level));
            last_nl = parser.eatToken(.Nl, false) orelse {
                last_nl = try parser.expectToken(.Eof, false);
                break;
            };
            if (!try parser.eatIndent(new_level)) break;
        }

        // reset to previous new line since all statements are expected to end in a newline
        parser.tok_index = last_nl;

        const node = try parser.arena.create(Node.Block);
        node.* = .{
            .base = .{ .id = .Block },
            .stmts = try parser.arena.dupe(*Node, stmts.items),
        };
        return &node.base;
    }

    /// if : "if" "(" (decl "=")? expr ")" block_or_expr ("else" block_or_expr)?
    fn ifExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_if, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true);
        const node = try parser.arena.create(Node.If);
        node.* = .{
            .if_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true, true, level) else null,
            .eq_tok = if (let_const != null) try parser.expectToken(.Equal, true) else null,
            .cond = try parser.expr(true, true, level),
            .r_paren = try parser.expectToken(.RParen, false),
            .if_body = try parser.blockOrExpr(skip_nl, allow_range, level),
            .else_tok = parser.eatTokenNoNl(.Keyword_else),
            .else_body = null,
        };
        if (node.else_tok != null) {
            node.else_body = try parser.blockOrExpr(skip_nl, allow_range, level);
        }
        return &node.base;
    }

    /// while : "while" "(" (decl "=")? expr ")" block_or_expr
    fn whileExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_while, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true);
        const node = try parser.arena.create(Node.While);
        node.* = .{
            .while_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true, true, level) else null,
            .eq_tok = if (let_const != null) try parser.expectToken(.Equal, true) else null,
            .cond = try parser.expr(true, true, level),
            .r_paren = try parser.expectToken(.RParen, false),
            .body = try parser.blockOrExpr(skip_nl, allow_range, level),
        };
        return &node.base;
    }

    /// for : "for" "(" (decl "in")? expr ")" block_or_expr
    fn forExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_for, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true);
        const node = try parser.arena.create(Node.For);
        node.* = .{
            .for_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true, true, level) else null,
            .in_tok = if (let_const != null) try parser.expectToken(.Keyword_in, true) else null,
            .cond = try parser.expr(true, true, level),
            .r_paren = try parser.expectToken(.RParen, false),
            .body = try parser.blockOrExpr(skip_nl, allow_range, level),
        };
        return &node.base;
    }

    /// match : "match" "(" expr ")" (NL match_case)+ NL
    fn matchExpr(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_match, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const cond = try parser.expr(true, true, level);
        const r_paren = try parser.expectToken(.RParen, false);
        _ = try parser.expectToken(.Nl, false);

        var cases = NodeList.init(parser.gpa);
        defer cases.deinit();

        const indent = parser.eatToken(.Indent, false);
        if (indent == null or parser.tokens[indent.?].id.Indent <= level)
            return parser.reportErr("expected indentation", parser.tokens[parser.tok_index]);

        const new_level = parser.tokens[indent.?].id.Indent;
        var last_nl = parser.tok_index;
        while (true) {
            try cases.append(try parser.matchCase(new_level));
            last_nl = parser.eatToken(.Nl, false) orelse {
                last_nl = try parser.expectToken(.Eof, false);
                break;
            };
            if (!try parser.eatIndent(new_level)) break;
        }

        // reset to previous new line since all statements are expected to end in a newline
        parser.tok_index = last_nl;

        const node = try parser.arena.create(Node.Match);
        node.* = .{
            .match_tok = tok,
            .expr = cond,
            .cases = try parser.arena.dupe(*Node, cases.items),
            .r_paren = r_paren,
        };
        return &node.base;
    }

    /// match_case
    ///     : decl "=>" block_or_expr
    ///     | expr ("," expr)* ","? "=>" block_or_expr
    fn matchCase(parser: *Parser, level: u16) Error!*Node {
        if (parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true)) |let_const|
        {
            const node = try parser.arena.create(Node.MatchLet);
            node.* = .{
                .let_const = let_const,
                .capture = try parser.primaryExpr(true, true, level),
                .eq_arr = try parser.expectToken(.EqualRarr, false),
                .expr = try parser.blockOrExpr(false, true, level),
            };
            return &node.base;
        } else if (parser.eatToken(.Underscore, true)) |u| {
            const node = try parser.arena.create(Node.MatchCatchAll);
            node.* = .{
                .tok = u,
                .eq_arr = try parser.expectToken(.EqualRarr, false),
                .expr = try parser.blockOrExpr(false, true, level),
            };
            return &node.base;
        } else {
            const items = try parser.listParser(true, level, expr, .EqualRarr, null);
            const node = try parser.arena.create(Node.MatchCase);
            node.* = .{
                .items = items.nodes,
                .eq_arr = items.term,
                .expr = try parser.blockOrExpr(false, true, level),
            };
            return &node.base;
        }
    }

    /// try : "try" block_or_expr ("catch" ("(" decl ")" | expr)? block_or_expr)+
    fn tryExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_try, false) orelse return null;
        const body = try parser.blockOrExpr(false, allow_range, level);

        var cases = NodeList.init(parser.gpa);
        defer cases.deinit();

        try cases.append((try parser.catchExpr(skip_nl, allow_range, level)) orelse {
            return parser.reportErr("expected at least one catch", parser.tokens[parser.tok_index]);
        });

        while (try parser.catchExpr(skip_nl, allow_range, level)) |catch_expr| {
            try cases.append(catch_expr);
        }

        const node = try parser.arena.create(Node.Try);
        node.* = .{
            .tok = tok,
            .expr = body,
            .catches = try parser.arena.dupe(*Node, cases.items),
        };
        return &node.base;
    }

    /// "catch" ("(" decl ")" | expr)? block_or_expr
    fn catchExpr(parser: *Parser, skip_nl: bool, allow_range: bool, level: u16) Error!?*Node {
        const tok = parser.eatTokenNoNl(.Keyword_catch) orelse return null;

        const node = try parser.arena.create(Node.Catch);
        node.* = .{
            .tok = tok,
            .let_const = null,
            .capture = null,
            .expr = undefined,
        };
        if (parser.eatTokenNoNl(.LParen)) |_| {
            parser.skipNl();
            node.let_const = parser.eatToken(.Keyword_let, true) orelse
                parser.eatToken(.Keyword_const, true);
            node.capture = if (node.let_const) |_|
                try parser.primaryExpr(true, true, level)
            else
                try parser.expr(true, true, level);

            _ = try parser.expectToken(.RParen, false);
        }
        node.expr = try parser.blockOrExpr(skip_nl, allow_range, level);
        return &node.base;
    }

    fn reportErr(parser: *Parser, msg: []const u8, tok: Token) Error {
        try parser.errors.add(.{ .data = msg }, tok.start, .err);
        return error.ParseError;
    }

    /// skips nl begins and ends
    fn skipNl(parser: *Parser) void {
        _ = parser.eatToken(.Nl, true);
    }

    fn eatIndent(parser: *Parser, level: u16) !bool {
        const start_index = parser.tok_index;
        const tok = parser.eatToken(.Indent, false) orelse return false;
        const indent = parser.tokens[tok].id.Indent;
        if (indent > level) return parser.reportErr("unexpected indentation", parser.tokens[parser.tok_index]);
        if (indent == level) return true;
        parser.tok_index = start_index;
        return false;
    }

    fn nextToken(parser: *Parser, skip_nl: bool) TokenIndex {
        const result = parser.tok_index;
        parser.tok_index += 1;
        std.debug.assert(parser.tokens[result].id != .Comment);

        if (parser.tok_index >= parser.tokens.len) return result;
        while (true) {
            switch (parser.tokens[parser.tok_index].id) {
                // skip nl and indent if they are not meaningful
                .Indent => if (!skip_nl) break,
                .Nl => if (!skip_nl and
                    (parser.tok_index + 1 <= parser.tokens.len and
                    parser.tokens[parser.tok_index + 1].id != .Comment)) break,
                // always skip comments
                .Comment => {},
                else => break,
            }
            parser.tok_index += 1;
        }
        return result;
    }

    fn eatToken(parser: *Parser, id: TokenId, skip_nl: bool) ?TokenIndex {
        return if (parser.tokens[parser.tok_index].id == id) parser.nextToken(skip_nl) else null;
    }

    fn eatTokenNoNl(parser: *Parser, id: TokenId) ?TokenIndex {
        const start = parser.tok_index;
        parser.skipNl();
        if (parser.eatToken(id, false)) |tok| return tok else {
            parser.tok_index = start;
            return null;
        }
    }

    fn expectToken(parser: *Parser, id: TokenId, skip_nl: bool) !TokenIndex {
        if (parser.eatToken(id, skip_nl)) |tok| return tok;
        const tok = parser.tokens[parser.tok_index];
        try parser.errors.add(try bog.Value.String.init(
            parser.gpa,
            "expected '{s}', found '{s}'",
            .{ Token.string(id), Token.string(tok.id) },
        ), tok.start, .err);
        return error.ParseError;
    }
};
