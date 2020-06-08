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

pub const Parser = struct {
    arena: *Allocator,
    gpa: *Allocator,
    errors: *bog.Errors,
    tokens: []const Token,
    tok_index: TokenIndex = 0,

    pub const Error = error{ParseError} || Allocator.Error;

    pub fn parseRepl(tree: *Tree, start_index: usize, errors: *bog.Errors) Error!?*Node {
        var parser = Parser{
            .arena = &tree.arena_allocator.allocator,
            .it = tree.tokens.iterator(start_index),
            .tree = tree,
            .errors = errors,
        };
        if (parser.eatToken(.Eof, true)) |_| return null;
        const ret = try parser.stmt();
        try tree.nodes.push(ret);
        _ = try parser.expectToken(.Eof, true);
        return ret;
    }

    /// stmt
    ///     : decl "=" block_or_expr
    ///     | expr
    fn stmt(parser: *Parser, level: u16) Error!*Node {
        if (try parser.decl(level)) |node| return node;
        return parser.expr(false, level);
    }

    /// decl : ("let" | "const") primary_expr
    fn decl(parser: *Parser, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true) orelse return null;
        const capture = try parser.primaryExpr(true, level);
        const eq_tok = try parser.expectToken(.Equal, true);
        const value = try parser.blockOrExpr(false, level);
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
    fn expr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        if (try parser.func(skip_nl, level)) |node| return node;
        if (try parser.jumpExpr(skip_nl, level)) |node| return node;
        return parser.assign(try parser.boolExpr(skip_nl, level), skip_nl, level);
    }

    /// block_or_expr : block | expr
    fn blockOrExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        if (!skip_nl) if (try parser.block(level)) |node| return node;
        parser.skipNl();
        return try parser.expr(skip_nl, level);
    }

    /// fn : "fn" "(" (primary_expr ",")* primary_expr? ")" block_or_expr
    fn func(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_fn, true) orelse return null;
        var r_paren: TokenIndex = undefined;
        var params = NodeList.init(parser.gpa);
        defer params.deinit();

        _ = try parser.expectToken(.LParen, true);
        var end = false;
        while (true) {
            if (parser.eatToken(.RParen, false)) |r_tok| {
                r_paren = r_tok;
                break;
            } else if (end) {
                r_paren = try parser.expectToken(.RParen, false);
                break;
            }
            try params.append(try parser.primaryExpr(true, level));
            if (parser.eatToken(.Comma, true) == null) end = true;
        }
        const node = try parser.arena.create(Node.Fn);
        node.* = .{
            .fn_tok = tok,
            .params = try parser.arena.dupe(*Node, params.items),
            .r_paren = r_paren,
            .body = try parser.blockOrExpr(skip_nl, level),
        };
        return &node.base;
    }

    /// jump_expr : "return" block_or_expr? | "break" | "continue"
    fn jumpExpr(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
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
            .Nl, .RParen, .RBrace, .RBracket, .Keyword_else, .Comma, .Colon => false,
            else => true,
        }) try parser.blockOrExpr(skip_nl, level) else null;
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
    fn boolExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        if (parser.eatToken(.Keyword_not, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Prefix);
            node.* = .{
                .op = .boolNot,
                .rhs = try parser.comparisonExpr(skip_nl, level),
                .tok = tok,
            };
            return &node.base;
        }
        var lhs = try parser.comparisonExpr(skip_nl, level);

        // TODO improve
        if (parser.eatTokenNoNl(.Keyword_or)) |t| {
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BoolOr,
                    .rhs = try parser.comparisonExpr(skip_nl, level),
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
                    .op = .BoolAnd,
                    .rhs = try parser.comparisonExpr(skip_nl, level),
                };
                lhs = &node.base;
            }
        }
        return lhs;
    }

    /// comparison_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr)?
    ///     | range_expr ("is" type_name)?
    fn comparisonExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        const lhs = try parser.rangeExpr(skip_nl, level);

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
                        .LArr => .LessThan,
                        .LArrEqual => .LessThanEqual,
                        .RArr => .GreaterThan,
                        .RArrEqual => .GreaterThanEqual,
                        .EqualEqual => .Equal,
                        .BangEqual => .NotEqual,
                        .Keyword_in => .In,
                        else => unreachable,
                    },
                    .rhs = try parser.rangeExpr(skip_nl, level),
                };
                return &node.base;
            },
            .Keyword_is => {
                const node = try parser.arena.create(Node.TypeInfix);
                node.* = .{
                    .lhs = lhs,
                    .op = .is,
                    .tok = tok,
                    .type_tok = try parser.typeName(),
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
    fn typeName(parser: *Parser) Error!TokenIndex {
        return parser.eatToken(.Keyword_error, true) orelse
            parser.eatToken(.Keyword_fn, true) orelse
            parser.eatToken(.Identifier, true) orelse
            parser.reportErr("expected type name", parser.tokens[parser.tok_index]);
    }

    /// range_expr : bit_expr ("..." bit_expr)?
    fn rangeExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        var lhs = try parser.bitExpr(skip_nl, level);

        if (parser.eatTokenNoNl(.Ellipsis)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok,
                .op = .Range,
                .rhs = try parser.bitExpr(skip_nl, level),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr)* | ("|" shift_expr)* | ("^" shift_expr)* | ("catch" ("(" decl ")")? block_or_expr))?
    fn bitExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        var lhs = try parser.shiftExpr(skip_nl, level);

        // TODO improve
        if (parser.eatTokenNoNl(.Ampersand)) |t| {
            // &
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitAnd,
                    .rhs = try parser.shiftExpr(skip_nl, level),
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
                    .op = .BitOr,
                    .rhs = try parser.shiftExpr(skip_nl, level),
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
                    .op = .BitXor,
                    .rhs = try parser.shiftExpr(skip_nl, level),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Caret)) |tt| tok = tt else break;
            }
        } else if (parser.eatTokenNoNl(.Keyword_catch)) |tok| {
            // catch
            const node = try parser.arena.create(Node.Catch);
            node.* = .{
                .tok = tok,
                .lhs = lhs,
                .capture = null,
                .let_const = null,
                .rhs = undefined,
            };
            if (parser.eatTokenNoNl(.LParen)) |_| {
                parser.skipNl();
                node.let_const = parser.eatToken(.Keyword_let, true) orelse
                    try parser.expectToken(.Keyword_const, true);
                node.capture = try parser.primaryExpr(true, level);
                _ = try parser.expectToken(.RParen, false);
            }
            node.rhs = try parser.blockOrExpr(skip_nl, level);
            lhs = &node.base;
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr)
    fn shiftExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        const lhs = try parser.addExpr(skip_nl, level);

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
                    .op = if (id == .LArrArr) .LShift else .RShift,
                    .rhs = try parser.addExpr(skip_nl, level),
                };
                return &node.base;
            },
            else => {
                parser.tok_index = start;
                return lhs;
            },
        }
    }

    /// add_expr : mul_expr (("-" | "+") mul_expr)*
    fn addExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        var lhs = try parser.mulExpr(skip_nl, level);

        while (true) {
            const tok = parser.nextToken(skip_nl);
            const id = parser.tokens[tok].id;
            switch (id) {
                .Minus, .Plus => {
                    parser.skipNl();
                    const node = try parser.arena.create(Node.Infix);
                    node.* = .{
                        .lhs = lhs,
                        .tok = tok,
                        .op = if (id == .Minus) .Sub else .Add,
                        .rhs = try parser.mulExpr(skip_nl, level),
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
    fn mulExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        var lhs = try parser.castExpr(skip_nl, level);

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
                            .Asterisk => .Mul,
                            .Slash => .Div,
                            .SlashSlash => .DivFloor,
                            .Percent => .Mod,
                            else => unreachable,
                        },
                        .rhs = try parser.castExpr(skip_nl, level),
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
    fn castExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        var lhs = try parser.prefixExpr(skip_nl, level);

        if (parser.eatTokenNoNl(.Keyword_as)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.TypeInfix);
            node.* = .{
                .lhs = lhs,
                .op = .as,
                .tok = tok,
                .type_tok = try parser.typeName(),
            };
            lhs = &node.base;
        }

        return lhs;
    }

    /// prefix_expr : ("try" | "-" | "+" | "~")? power_expr
    fn prefixExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        const tok = parser.nextToken(skip_nl);
        const id = parser.tokens[tok].id;
        switch (id) {
            .Keyword_try, .Minus, .Plus, .Tilde => {
                parser.skipNl();
                const node = try parser.arena.create(Node.Prefix);
                node.* = .{
                    .op = switch (id) {
                        .Keyword_try => .Try,
                        .Minus => .minus,
                        .Plus => .plus,
                        .Tilde => .bitNot,
                        else => unreachable,
                    },
                    .tok = tok,
                    .rhs = try parser.powerExpr(skip_nl, level),
                };
                return &node.base;
            },
            else => {
                parser.tok_index = tok;
                return try parser.powerExpr(skip_nl, level);
            },
        }
    }

    /// power_expr : primary_expr suffix_expr* ("**" power_expr)?
    fn powerExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
        var primary = try parser.primaryExpr(skip_nl, level);
        primary = try parser.suffixExpr(primary, skip_nl, level);
        if (parser.eatTokenNoNl(.AsteriskAsterisk)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = primary,
                .tok = tok,
                .op = .Pow,
                .rhs = try parser.powerExpr(skip_nl, level),
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
                    .op = .{ .subscript = try parser.expr(true, level) },
                    .r_tok = try parser.expectToken(.RBracket, false),
                };
                lhs = &node.base;
            } else if (parser.eatToken(.LParen, skip_nl)) |tok| {
                parser.skipNl();
                var r_paren: TokenIndex = undefined;
                var args = NodeList.init(parser.gpa);
                defer args.deinit();

                var end = false;
                while (true) {
                    if (parser.eatToken(.RParen, false)) |r_tok| {
                        r_paren = r_tok;
                        break;
                    } else if (end) {
                        r_paren = try parser.expectToken(.RParen, false);
                        break;
                    }
                    try args.append(try parser.expr(true, level));
                    if (parser.eatToken(.Comma, true) == null) end = true;
                }

                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .call = try parser.arena.dupe(*Node, args.items) },
                    .r_tok = r_paren,
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
    fn assign(parser: *Parser, lhs: *Node, skip_nl: bool, level: u16) Error!*Node {
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
                        .Equal => .Assign,
                        .PlusEqual => .AddAssign,
                        .MinusEqual => .SubAssign,
                        .AsteriskEqual => .MulAssign,
                        .AsteriskAsteriskEqual => .PowAssign,
                        .SlashEqual => .DivAssign,
                        .SlashSlashEqual => .DivFloorAssign,
                        .PercentEqual => .ModAssign,
                        .LArrArrEqual => .LShiftAssign,
                        .RArrArrEqual => .RShiftAssign,
                        .AmpersandEqual => .BitAndAssign,
                        .PipeEqual => .BitOrAssign,
                        .CaretEqual => .BitXOrAssign,
                        else => unreachable,
                    },
                    .rhs = try parser.blockOrExpr(skip_nl, level),
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
    ///     | NUMBER
    ///     | "true"
    ///     | "false"
    ///     | "(" block_or_expr ")"
    ///     | "(" (expr ",")+ expr? ")"
    ///     | "{" (expr ":" expr ",")* (expr ":" expr)? "}"
    ///     | "[" (expr ",")* expr? "]"
    ///     | "error" "(" expr ")"
    ///     | "import" "(" STRING ")"
    ///     | "native" "(" (STRING ",")? STRING ")"
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    fn primaryExpr(parser: *Parser, skip_nl: bool, level: u16) Error!*Node {
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
                _ = try parser.expectToken(.LParen, true);
                const node = try parser.arena.create(Node.Error);
                node.* = .{
                    .tok = tok,
                    .value = try parser.expr(true, level),
                    .r_paren = try parser.expectToken(.RParen, skip_nl),
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
            .Keyword_native => {
                _ = try parser.expectToken(.LParen, true);
                const node = try parser.arena.create(Node.Native);
                node.* = .{
                    .tok = tok,
                    .str_tok = try parser.expectToken(.String, true),
                    .r_paren = try parser.expectToken(.RParen, skip_nl),
                };
                return &node.base;
            },
            .LParen => {
                if (parser.eatToken(.RParen, skip_nl)) |rparen| {
                    const node = try parser.arena.create(Node.Literal);
                    node.* = .{
                        .tok = rparen,
                        .kind = .none,
                    };
                    return &node.base;
                } else {
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
                    const first = try parser.expr(true, level);
                    if (parser.eatToken(.RParen, true)) |r_tok| {
                        const node = try parser.arena.create(Node.Grouped);
                        node.* = .{
                            .l_tok = tok,
                            .expr = first,
                            .r_tok = r_tok,
                        };
                        return &node.base;
                    }
                    var elems = NodeList.init(parser.gpa);
                    defer elems.deinit();
                    try elems.append(first);
                    var r_paren: TokenIndex = undefined;

                    _ = try parser.expectToken(.Comma, true);
                    var end = false;
                    while (true) {
                        if (parser.eatToken(.RParen, skip_nl)) |r_tok| {
                            r_paren = r_tok;
                            break;
                        } else if (end) {
                            r_paren = try parser.expectToken(.RBrace, skip_nl);
                            break;
                        }
                        try elems.append(try parser.expr(true, level));
                        if (parser.eatToken(.Comma, true) == null) end = true;
                    }
                    const node = try parser.arena.create(Node.ListTupleMap);
                    node.* = .{
                        .base = .{ .id = .Tuple },
                        .l_tok = tok,
                        .values = try parser.arena.dupe(*Node, elems.items),
                        .r_tok = r_paren,
                    };
                    return &node.base;
                }
            },
            .LBrace => {
                parser.skipNl();

                // map
                var elems = NodeList.init(parser.gpa);
                defer elems.deinit();
                var r_brace: TokenIndex = undefined;

                var end = false;
                while (true) {
                    if (parser.eatToken(.RBrace, skip_nl)) |r_tok| {
                        r_brace = r_tok;
                        break;
                    } else if (end) {
                        r_brace = try parser.expectToken(.RBrace, skip_nl);
                        break;
                    }
                    var key: ?*Node = null;
                    var value = try parser.expr(true, level);
                    var colon: ?TokenIndex = null;
                    if (parser.eatToken(.Colon, true)) |col| {
                        colon = col;
                        key = value;
                        value = try parser.expr(true, level);
                    }
                    const item = try parser.arena.create(Node.MapItem);
                    item.* = .{
                        .key = key,
                        .colon = colon,
                        .value = value,
                    };
                    try elems.append(&item.base);
                    if (parser.eatToken(.Comma, true) == null) end = true;
                }

                const node = try parser.arena.create(Node.ListTupleMap);
                node.* = .{
                    .base = .{ .id = .Map },
                    .l_tok = tok,
                    .values = try parser.arena.dupe(*Node, elems.items),
                    .r_tok = r_brace,
                };
                return &node.base;
            },
            .LBracket => {
                parser.skipNl();

                // list
                parser.skipNl();
                var elems = NodeList.init(parser.gpa);
                defer elems.deinit();
                var r_bracket: TokenIndex = undefined;

                var end = false;
                while (true) {
                    if (parser.eatToken(.RBracket, skip_nl)) |r_tok| {
                        r_bracket = r_tok;
                        break;
                    } else if (end) {
                        r_bracket = try parser.expectToken(.RBracket, skip_nl);
                        break;
                    }
                    try elems.append(try parser.expr(true, level));
                    if (parser.eatToken(.Comma, true) == null) end = true;
                }
                const node = try parser.arena.create(Node.ListTupleMap);
                node.* = .{
                    .base = .{ .id = .List },
                    .l_tok = tok,
                    .values = try parser.arena.dupe(*Node, elems.items),
                    .r_tok = r_bracket,
                };
                return &node.base;
            },
            else => {
                parser.tok_index = tok;
            },
        }
        if (try parser.ifExpr(skip_nl, level)) |res| return res;
        if (try parser.whileExpr(skip_nl, level)) |res| return res;
        if (try parser.forExpr(skip_nl, level)) |res| return res;
        if (try parser.matchExpr(skip_nl, level)) |res| return res;
        return parser.reportErr("expected Identifier, String, Number, true, false, '(', '{{', '[', error, import, if, while, for or match", parser.tokens[parser.tok_index]);
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
    fn ifExpr(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_if, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true);
        const node = try parser.arena.create(Node.If);
        node.* = .{
            .if_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true, level) else null,
            .eq_tok = if (let_const != null) try parser.expectToken(.Equal, true) else null,
            .cond = try parser.expr(true, level),
            .r_paren = try parser.expectToken(.RParen, false),
            .if_body = try parser.blockOrExpr(skip_nl, level),
            .else_tok = parser.eatTokenNoNl(.Keyword_else),
            .else_body = null,
        };
        if (node.else_tok != null) {
            node.else_body = try parser.blockOrExpr(skip_nl, level);
        }
        return &node.base;
    }

    /// while : "while" "(" (decl "=")? expr ")" block_or_expr
    fn whileExpr(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_while, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true);
        const node = try parser.arena.create(Node.While);
        node.* = .{
            .while_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true, level) else null,
            .eq_tok = if (let_const != null) try parser.expectToken(.Equal, true) else null,
            .cond = try parser.expr(true, level),
            .r_paren = try parser.expectToken(.RParen, false),
            .body = try parser.blockOrExpr(skip_nl, level),
        };
        return &node.base;
    }

    /// for : "for" "(" (decl "in")? expr ")" block_or_expr
    fn forExpr(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_for, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true);
        const node = try parser.arena.create(Node.For);
        node.* = .{
            .for_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true, level) else null,
            .in_tok = if (let_const != null) try parser.expectToken(.Keyword_in, true) else null,
            .cond = try parser.expr(true, level),
            .r_paren = try parser.expectToken(.RParen, false),
            .body = try parser.blockOrExpr(skip_nl, level),
        };
        return &node.base;
    }

    /// match : "match" "(" expr ")" (NL match_case)+ NL
    fn matchExpr(parser: *Parser, skip_nl: bool, level: u16) Error!?*Node {
        const tok = parser.eatToken(.Keyword_match, true) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const cond = try parser.expr(true, level);
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
    ///     : decl ":" block_or_expr
    ///     | expr ("," expr)* ","? ":" block_or_expr
    fn matchCase(parser: *Parser, level: u16) Error!*Node {
        if (parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, true)) |let_const|
        {
            if (parser.eatToken(.Identifier, true)) |_| {
                const node = try parser.arena.create(Node.MatchCatchAll);
                node.* = .{
                    .tok = let_const,
                    .colon = try parser.expectToken(.Colon, false),
                    .expr = try parser.blockOrExpr(false, level),
                };
                return &node.base;
            }
            const capture = try parser.primaryExpr(true, level);
            const node = try parser.arena.create(Node.MatchLet);
            node.* = .{
                .let_const = let_const,
                .capture = capture,
                .colon = try parser.expectToken(.Colon, false),
                .expr = try parser.blockOrExpr(false, level),
            };
            return &node.base;
        } else if (parser.eatToken(.Underscore, true)) |u| {
            const node = try parser.arena.create(Node.MatchCatchAll);
            node.* = .{
                .tok = u,
                .colon = try parser.expectToken(.Colon, false),
                .expr = try parser.blockOrExpr(false, level),
            };
            return &node.base;
        } else {
            var items = NodeList.init(parser.gpa);
            defer items.deinit();
            var colon: TokenIndex = undefined;

            var end = false;
            while (true) {
                if (parser.eatToken(.Colon, false)) |tok| {
                    colon = tok;
                    break;
                } else if (end) {
                    colon = try parser.expectToken(.Colon, false);
                    break;
                }
                try items.append(try parser.expr(true, level));
                if (parser.eatToken(.Comma, true) == null) end = true;
            }
            const node = try parser.arena.create(Node.MatchCase);
            node.* = .{
                .lhs = try parser.arena.dupe(*Node, items.items),
                .colon = colon,
                .expr = try parser.blockOrExpr(false, level),
            };
            return &node.base;
        }
    }

    fn reportErr(parser: *Parser, msg: []const u8, tok: Token) Error {
        try parser.errors.add(msg, tok.start, .err);
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
        return parser.reportErr("unexpected token", parser.tokens[parser.tok_index]);
    }
};
