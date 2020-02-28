const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const Allocator = mem.Allocator;
const bog = @import("bog.zig");
const Token = bog.Token;
const TokenList = Token.List;
const TokenIndex = Token.Index;
const Tokenizer = bog.Tokenizer;
const Tree = bog.Tree;
const Node = bog.Node;
const NodeList = Node.List;
const Errors = bog.Errors;

pub const Parser = struct {
    it: TokenList.Iterator,
    errors: *Errors,
    tree: *Tree,
    arena: *Allocator,

    pub const Error = error{ParseError} || Allocator.Error;

    /// root : (stmt NL)* EOF
    pub fn parse(allocator: *Allocator, source: []const u8, errors: *Errors) (Error || Tokenizer.Error)!*Tree {
        const tree = blk: {
            var arena = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            const tree = try arena.allocator.create(Tree);
            tree.* = .{
                .source = source,
                .arena_allocator = arena,
                .tokens = undefined,
                .nodes = undefined,
            };
            break :blk tree;
        };
        errdefer tree.deinit();
        const arena = &tree.arena_allocator.allocator;
        tree.tokens = TokenList.init(arena);
        tree.nodes = NodeList.init(arena);

        try Tokenizer.tokenize(tree, errors);

        var parser = Parser{
            .arena = arena,
            .it = tree.tokens.iterator(0),
            .tree = tree,
            .errors = errors,
        };
        while (true) {
            if (parser.eatToken(.Eof, true)) |_| break;
            try tree.nodes.push(try parser.stmt());
            _ = parser.eatToken(.Nl, false) orelse {
                if (parser.sawEnd()) continue;
                _ = try parser.expectToken(.Eof, true);
                break;
            };
        }
        return tree;
    }

    pub fn parseRepl(tree: *Tree, start_index: usize, errors: *Errors) Error!?*Node {
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
    ///     : decl "=" expr
    ///     | expr
    fn stmt(parser: *Parser) Error!*Node {
        if (try parser.decl()) |node| return node;
        return parser.expr(false);
    }

    /// decl : ("let" | "const") primary_expr
    fn decl(parser: *Parser) Error!?*Node {
        const tok = parser.eatToken(.Keyword_let, false) orelse
            parser.eatToken(.Keyword_const, false) orelse return null;
        const capture = try parser.primaryExpr(true);
        const eq_tok = try parser.expectToken(.Equal, true);
        parser.skipNl();
        const value = try parser.expr(false);
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
    ///     | block
    fn expr(parser: *Parser, skip_nl: bool) Error!*Node {
        if (try parser.block()) |node| return node;
        if (try parser.func(skip_nl)) |node| return node;
        if (try parser.jumpExpr()) |node| return node;
        return parser.assign(try parser.boolExpr(skip_nl), skip_nl);
    }

    /// fn : "fn" "(" (primary_expr ",")* primary_expr? ")" expr
    fn func(parser: *Parser, skip_nl: bool) Error!?*Node {
        const tok = parser.eatToken(.Keyword_fn, skip_nl) orelse return null;
        const node = try parser.arena.create(Node.Fn);
        node.* = .{
            .fn_tok = tok,
            .params = NodeList.init(parser.arena),
            .r_paren = undefined,
            .body = undefined,
        };
        _ = try parser.expectToken(.LParen, true);
        var end = false;
        while (true) {
            if (parser.eatToken(.RParen, true)) |r_tok| {
                node.r_paren = r_tok;
                break;
            } else if (end) {
                node.r_paren = try parser.expectToken(.RParen, true);
                break;
            }
            try node.params.push(try parser.primaryExpr(true));
            if (parser.eatToken(.Comma, true) == null) end = true;
        }
        node.body = try parser.expr(skip_nl);
        return &node.base;
    }

    /// jump_expr : "return" expr? | "break" | "continue"
    fn jumpExpr(parser: *Parser) Error!?*Node {
        const tok = parser.eatTokenId(.Keyword_return, false) orelse
            parser.eatTokenId(.Keyword_break, false) orelse
            parser.eatTokenId(.Keyword_continue, false) orelse
            return null;
        const peek = parser.it.peek().?;
        const res = if (tok.id == .Keyword_return and switch (peek.id) {
            .Nl, .RParen, .RBrace, .RBracket, .Keyword_else, .Comma, .Colon => false,
            else => true,
        }) try parser.expr(false) else null;
        const node = try parser.arena.create(Node.Jump);
        node.* = .{
            .tok = tok.index,
            .op = switch (tok.id) {
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
    fn boolExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        if (parser.eatToken(.Keyword_not, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Prefix);
            node.* = .{
                .op = .boolNot,
                .rhs = try parser.comparisonExpr(skip_nl),
                .tok = tok,
            };
            return &node.base;
        }
        var lhs = try parser.comparisonExpr(skip_nl);

        // TODO improve
        if (parser.eatTokenNoNl(.Keyword_or, skip_nl)) |t| {
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BoolOr,
                    .rhs = try parser.comparisonExpr(skip_nl),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Keyword_or, skip_nl)) |tt| tok = tt else break;
            }
        } else {
            while (parser.eatTokenNoNl(.Keyword_and, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BoolAnd,
                    .rhs = try parser.comparisonExpr(skip_nl),
                };
                lhs = &node.base;
            }
        }
        return lhs;
    }

    /// comparison_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr)?
    ///     | range_expr ("is" type_name)?
    fn comparisonExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.rangeExpr(skip_nl);

        if (parser.eatTokenIdNoNl(.LArr, skip_nl) orelse
            parser.eatTokenIdNoNl(.LArrEqual, skip_nl) orelse
            parser.eatTokenIdNoNl(.RArr, skip_nl) orelse
            parser.eatTokenIdNoNl(.RArrEqual, skip_nl) orelse
            parser.eatTokenIdNoNl(.EqualEqual, skip_nl) orelse
            parser.eatTokenIdNoNl(.BangEqual, skip_nl) orelse
            parser.eatTokenIdNoNl(.Keyword_in, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = switch (tok.id) {
                    .LArr => .LessThan,
                    .LArrEqual => .LessThanEqual,
                    .RArr => .GreaterThan,
                    .RArrEqual => .GreaterThanEqual,
                    .EqualEqual => .Equal,
                    .BangEqual => .NotEqual,
                    .Keyword_in => .In,
                    else => unreachable,
                },
                .rhs = try parser.rangeExpr(skip_nl),
            };
            lhs = &node.base;
        } else if (parser.eatTokenNoNl(.Keyword_is, skip_nl)) |tok| {
            const node = try parser.arena.create(Node.TypeInfix);
            node.* = .{
                .lhs = lhs,
                .op = .is,
                .tok = tok,
                .type_tok = try parser.typeName(),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// type_name : "none" | "int" | "num" | "bool" | "str" | "tuple" | "map" | "list" | "error" | "range" | "fn"
    fn typeName(parser: *Parser) Error!TokenIndex {
        return parser.eatToken(.Keyword_error, true) orelse
            parser.eatToken(.Keyword_fn, true) orelse
            parser.eatToken(.Identifier, true) orelse
            parser.reportErr("expected type name", parser.it.peek().?);
    }

    /// range_expr : bit_expr ("..." bit_expr)?
    fn rangeExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.bitExpr(skip_nl);

        if (parser.eatTokenNoNl(.Ellipsis, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok,
                .op = .Range,
                .rhs = try parser.bitExpr(skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr)* | ("|" shift_expr)* | ("^" shift_expr)* | ("catch" ("(" decl ")")? expr))?
    fn bitExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.shiftExpr(skip_nl);

        // TODO improve
        if (parser.eatTokenNoNl(.Ampersand, skip_nl)) |t| {
            // &
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitAnd,
                    .rhs = try parser.shiftExpr(skip_nl),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Ampersand, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatTokenNoNl(.Pipe, skip_nl)) |t| {
            // |
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitOr,
                    .rhs = try parser.shiftExpr(skip_nl),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Pipe, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatTokenNoNl(.Caret, skip_nl)) |t| {
            // ^
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitXor,
                    .rhs = try parser.shiftExpr(skip_nl),
                };
                lhs = &node.base;
                if (parser.eatTokenNoNl(.Caret, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatTokenNoNl(.Keyword_catch, skip_nl)) |tok| {
            // catch
            const node = try parser.arena.create(Node.Catch);
            node.* = .{
                .tok = tok,
                .lhs = lhs,
                .capture = null,
                .let_const = null,
                .rhs = undefined,
            };
            if (parser.eatToken(.LParen, true)) |_| {
                node.let_const = parser.eatToken(.Keyword_let, true) orelse
                    try parser.expectToken(.Keyword_const, false);
                node.capture = try parser.primaryExpr(true);
                _ = try parser.expectToken(.RParen, true);
            }
            parser.skipNl();
            node.rhs = try parser.expr(skip_nl);
            lhs = &node.base;
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr)
    fn shiftExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.addExpr(skip_nl);

        if (parser.eatTokenIdNoNl(.LArrArr, skip_nl) orelse
            parser.eatTokenIdNoNl(.RArrArr, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = if (tok.id == .LArrArr) .LShift else .RShift,
                .rhs = try parser.addExpr(skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// add_expr : mul_expr (("-" | "+") mul_expr)*
    fn addExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.mulExpr(skip_nl);

        while (parser.eatTokenId(.Minus, skip_nl) orelse
            parser.eatTokenId(.Plus, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = if (tok.id == .Minus) .Sub else .Add,
                .rhs = try parser.mulExpr(skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// mul_expr : cast_expr (("*" | "/" | "//" | "%") cast_expr)*
    fn mulExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.castExpr(skip_nl);

        while (parser.eatTokenIdNoNl(.Asterisk, skip_nl) orelse
            parser.eatTokenIdNoNl(.Slash, skip_nl) orelse
            parser.eatTokenIdNoNl(.SlashSlash, skip_nl) orelse
            parser.eatTokenIdNoNl(.Percent, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = switch (tok.id) {
                    .Asterisk => .Mul,
                    .Slash => .Div,
                    .SlashSlash => .DivFloor,
                    .Percent => .Mod,
                    else => unreachable,
                },
                .rhs = try parser.castExpr(skip_nl),
            };
            lhs = &node.base;
        }

        return lhs;
    }

    /// cast_expr : prefix_expr ("as" type_name)?
    fn castExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var lhs = try parser.prefixExpr(skip_nl);

        if (parser.eatTokenNoNl(.Keyword_as, skip_nl)) |tok| {
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
    fn prefixExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        if (parser.eatTokenId(.Keyword_try, skip_nl) orelse
            parser.eatTokenId(.Minus, skip_nl) orelse
            parser.eatTokenId(.Plus, skip_nl) orelse
            parser.eatTokenId(.Tilde, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Prefix);
            node.* = .{
                .op = switch (tok.id) {
                    .Keyword_try => .Try,
                    .Minus => .minus,
                    .Plus => .plus,
                    .Tilde => .bitNot,
                    else => unreachable,
                },
                .tok = tok.index,
                .rhs = try parser.powerExpr(skip_nl),
            };
            return &node.base;
        }
        return try parser.powerExpr(skip_nl);
    }

    /// power_expr : primary_expr suffix_expr* ("**" power_expr)?
    fn powerExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        var primary = try parser.primaryExpr(skip_nl);
        primary = try parser.suffixExpr(primary, skip_nl);
        if (parser.eatTokenNoNl(.AsteriskAsterisk, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = primary,
                .tok = tok,
                .op = .Pow,
                .rhs = try parser.powerExpr(skip_nl),
            };
            return &node.base;
        }
        return primary;
    }

    /// suffix_expr
    ///     : "[" expr "]"
    ///     | "(" (expr ",")* expr? ")"
    ///     | "." IDENTIFIER
    fn suffixExpr(parser: *Parser, primary: *Node, skip_nl: bool) Error!*Node {
        var lhs = primary;
        while (true) {
            if (parser.eatToken(.LBracket, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .subscript = try parser.expr(true) },
                    .r_tok = try parser.expectToken(.RBracket, true),
                };
                lhs = &node.base;
            } else if (parser.eatToken(.LParen, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .call = NodeList.init(parser.arena) },
                    .r_tok = undefined,
                };
                var end = false;
                while (true) {
                    if (parser.eatToken(.RParen, true)) |r_tok| {
                        node.r_tok = r_tok;
                        break;
                    } else if (end) {
                        node.r_tok = try parser.expectToken(.RParen, true);
                        break;
                    }
                    try node.op.call.push(try parser.expr(true));
                    if (parser.eatToken(.Comma, true) == null) end = true;
                }
                lhs = &node.base;
            } else if (parser.eatTokenNoNl(.Period, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .member,
                    .r_tok = try parser.expectToken(.Identifier, true),
                };
                lhs = &node.base;
            } else {
                return lhs;
            }
        }
    }

    /// assign : ("=" | "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") expr
    fn assign(parser: *Parser, lhs: *Node, skip_nl: bool) Error!*Node {
        // assignment cannot happen in places where NL is not necessary
        if (parser.eatTokenId(.Equal, false) orelse
            parser.eatTokenId(.PlusEqual, false) orelse
            parser.eatTokenId(.MinusEqual, false) orelse
            parser.eatTokenId(.AsteriskEqual, false) orelse
            parser.eatTokenId(.AsteriskAsteriskEqual, false) orelse
            parser.eatTokenId(.SlashEqual, false) orelse
            parser.eatTokenId(.SlashSlashEqual, false) orelse
            parser.eatTokenId(.PercentEqual, false) orelse
            parser.eatTokenId(.LArrArrEqual, false) orelse
            parser.eatTokenId(.RArrArrEqual, false) orelse
            parser.eatTokenId(.AmpersandEqual, false) orelse
            parser.eatTokenId(.PipeEqual, false) orelse
            parser.eatTokenId(.CaretEqual, false)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = switch (tok.id) {
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
                .rhs = try parser.expr(false),
            };
            return &node.base;
        }

        return lhs;
    }

    /// primary_expr
    ///     : IDENTIFIER
    ///     | "_"
    ///     | STRING
    ///     | NUMBER
    ///     | "true"
    ///     | "false"
    ///     | "(" expr ")"
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
    fn primaryExpr(parser: *Parser, skip_nl: bool) Error!*Node {
        if (parser.eatTokenId(.Number, skip_nl) orelse
            parser.eatTokenId(.Integer, skip_nl) orelse
            parser.eatTokenId(.String, skip_nl) orelse
            parser.eatTokenId(.Keyword_true, skip_nl) orelse
            parser.eatTokenId(.Keyword_false, skip_nl)) |tok|
        {
            const node = try parser.arena.create(Node.Literal);
            node.* = .{
                .tok = tok.index,
                .kind = switch (tok.id) {
                    .Number => .num,
                    .Integer => .int,
                    .String => .str,
                    .Keyword_true => .True,
                    .Keyword_false => .False,
                    else => unreachable,
                },
            };
            return &node.base;
        }
        if (parser.eatToken(.Identifier, skip_nl)) |tok| {
            const node = try parser.arena.create(Node.SingleToken);
            node.* = .{
                .base = .{ .id = .Identifier },
                .tok = tok,
            };
            return &node.base;
        }
        if (parser.eatToken(.Underscore, skip_nl)) |tok| {
            const node = try parser.arena.create(Node.SingleToken);
            node.* = .{
                .base = .{ .id = .Discard },
                .tok = tok,
            };
            return &node.base;
        }
        if (parser.eatToken(.Keyword_error, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const node = try parser.arena.create(Node.Error);
            node.* = .{
                .tok = tok,
                .value = try parser.expr(true),
                .r_paren = try parser.expectToken(.RParen, true),
            };
            return &node.base;
        }
        if (parser.eatToken(.Keyword_import, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const node = try parser.arena.create(Node.Import);
            node.* = .{
                .tok = tok,
                .str_tok = try parser.expectToken(.String, true),
                .r_paren = try parser.expectToken(.RParen, true),
            };
            return &node.base;
        }
        if (parser.eatToken(.Keyword_native, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const node = try parser.arena.create(Node.Native);
            node.* = .{
                .tok = tok,
                .str_tok = try parser.expectToken(.String, true),
                .r_paren = try parser.expectToken(.RParen, true),
            };
            return &node.base;
        }
        if (parser.eatToken(.LParen, skip_nl)) |tok| {
            if (parser.eatToken(.RParen, false)) |rparen| {
                const node = try parser.arena.create(Node.Literal);
                node.* = .{
                    .tok = rparen,
                    .kind = .none,
                };
                return &node.base;
            } else {
                const first = try parser.expr(true);
                if (parser.eatToken(.RParen, true)) |r_tok| {
                    const node = try parser.arena.create(Node.Grouped);
                    node.* = .{
                        .l_tok = tok,
                        .expr = first,
                        .r_tok = r_tok,
                    };
                    return &node.base;
                }
                const node = try parser.arena.create(Node.ListTupleMap);
                node.* = .{
                    .base = .{ .id = .Tuple },
                    .l_tok = tok,
                    .values = NodeList.init(parser.arena),
                    .r_tok = undefined,
                };
                try node.values.push(first);
                _ = try parser.expectToken(.Comma, true);
                var end = false;
                while (true) {
                    if (parser.eatToken(.RParen, true)) |r_tok| {
                        node.r_tok = r_tok;
                        return &node.base;
                    } else if (end) {
                        node.r_tok = try parser.expectToken(.RBrace, true);
                        break;
                    }
                    try node.values.push(try parser.expr(true));
                    if (parser.eatToken(.Comma, true) == null) end = true;
                }
            }
        }
        if (parser.eatToken(.LBrace, skip_nl)) |tok| {
            // map
            const node = try parser.arena.create(Node.ListTupleMap);
            node.* = .{
                .base = .{ .id = .Map },
                .l_tok = tok,
                .values = NodeList.init(parser.arena),
                .r_tok = undefined,
            };
            var end = false;
            while (true) {
                if (parser.eatToken(.RBrace, true)) |r_tok| {
                    node.r_tok = r_tok;
                    break;
                } else if (end) {
                    node.r_tok = try parser.expectToken(.RBrace, true);
                    break;
                }
                var key: ?*Node = null;
                var value = try parser.expr(true);
                var colon: ?TokenIndex = null;
                if (parser.eatToken(.Colon, true)) |col| {
                    colon = col;
                    key = value;
                    value = try parser.expr(true);
                }
                const item = try parser.arena.create(Node.MapItem);
                item.* = .{
                    .key = key,
                    .colon = colon,
                    .value = value,
                };
                try node.values.push(&item.base);
                if (parser.eatToken(.Comma, true) == null) end = true;
            }
            return &node.base;
        }
        if (parser.eatToken(.LBracket, skip_nl)) |tok| {
            // list
            parser.skipNl();
            const node = try parser.arena.create(Node.ListTupleMap);
            node.* = .{
                .base = .{ .id = .List },
                .l_tok = tok,
                .values = NodeList.init(parser.arena),
                .r_tok = undefined,
            };
            var end = false;
            while (true) {
                if (parser.eatToken(.RBracket, true)) |r_tok| {
                    node.r_tok = r_tok;
                    break;
                } else if (end) {
                    node.r_tok = try parser.expectToken(.RBracket, true);
                    break;
                }
                try node.values.push(try parser.expr(true));
                if (parser.eatToken(.Comma, true) == null) end = true;
            }
            return &node.base;
        }
        if (try parser.ifExpr(skip_nl)) |res| return res;
        if (try parser.whileExpr(skip_nl)) |res| return res;
        if (try parser.forExpr(skip_nl)) |res| return res;
        if (try parser.matchExpr(skip_nl)) |res| return res;
        return parser.reportErr("expected Identifier, String, Number, true, false, '(', '{{', '[', error, import, if, while, for or match", parser.it.peek().?);
    }

    /// block : NL BEGIN (stmt NL)+ END
    fn block(parser: *Parser) Error!?*Node {
        _ = parser.eatToken(.Nl, false) orelse return null;
        _ = parser.eatToken(.Begin, false) orelse {
            parser.it.index -= 1;
            return null;
        };
        const node = try parser.arena.create(Node.Block);
        node.* = .{
            .base = .{ .id = .Block },
            .stmts = NodeList.init(parser.arena),
        };
        while (true) {
            try node.stmts.push(try parser.stmt());
            _ = parser.eatToken(.Nl, false) orelse blk: {
                if (parser.sawEnd()) break :blk;
                if (parser.eatToken(.Eof, false)) |_| {
                    _ = parser.it.prev();
                    break;
                }
                _ = try parser.expectToken(.End, false);
                break;
            };
            if (parser.eatToken(.End, false)) |_|
                break;
        }
        return &node.base;
    }

    /// if : "if" "(" (decl "=")? expr ")" expr ("else" expr)?
    fn ifExpr(parser: *Parser, skip_nl: bool) Error!?*Node {
        const tok = parser.eatToken(.Keyword_if, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, false);
        const node = try parser.arena.create(Node.If);
        node.* = .{
            .if_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true) else null,
            .eq_tok = if (let_const != null) try parser.expectToken(.Equal, true) else null,
            .cond = try parser.expr(true),
            .r_paren = try parser.expectToken(.RParen, true),
            .if_body = try parser.expr(skip_nl),
            .else_tok = parser.eatTokenNoNl(.Keyword_else, skip_nl),
            .else_body = null,
        };
        if (node.else_tok != null) {
            node.else_body = try parser.expr(skip_nl);
        }
        return &node.base;
    }

    /// while : "while" "(" (decl "=")? expr ")" expr
    fn whileExpr(parser: *Parser, skip_nl: bool) Error!?*Node {
        const tok = parser.eatToken(.Keyword_while, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, false);
        const node = try parser.arena.create(Node.While);
        node.* = .{
            .while_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true) else null,
            .eq_tok = if (let_const != null) try parser.expectToken(.Equal, true) else null,
            .cond = try parser.expr(true),
            .r_paren = try parser.expectToken(.RParen, true),
            .body = try parser.expr(skip_nl),
        };
        return &node.base;
    }

    /// for : "for" "(" (decl "in")? expr ")" expr
    fn forExpr(parser: *Parser, skip_nl: bool) Error!?*Node {
        const tok = parser.eatToken(.Keyword_for, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const let_const = parser.eatToken(.Keyword_let, true) orelse
            parser.eatToken(.Keyword_const, false);
        const node = try parser.arena.create(Node.For);
        node.* = .{
            .for_tok = tok,
            .let_const = let_const,
            .capture = if (let_const != null) try parser.primaryExpr(true) else null,
            .in_tok = if (let_const != null) try parser.expectToken(.Keyword_in, true) else null,
            .cond = try parser.expr(true),
            .r_paren = try parser.expectToken(.RParen, true),
            .body = try parser.expr(skip_nl),
        };
        return &node.base;
    }

    /// match : "match" "(" expr ")" (NL match_case)+ NL
    fn matchExpr(parser: *Parser, skip_nl: bool) Error!?*Node {
        const tok = parser.eatToken(.Keyword_match, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const node = try parser.arena.create(Node.Match);
        node.* = .{
            .match_tok = tok,
            .expr = try parser.expr(true),
            .body = NodeList.init(parser.arena),
            .r_paren = try parser.expectToken(.RParen, true),
        };
        _ = try parser.expectToken(.Nl, false);
        _ = try parser.expectToken(.Begin, false);
        while (true) {
            try node.body.push(try parser.matchCase());
            _ = parser.eatToken(.Nl, false) orelse {
                _ = try parser.expectToken(.End, false);
                break;
            };
            if (parser.eatToken(.End, false)) |_|
                break;
        }
        return &node.base;
    }

    /// match_case
    ///     : decl ":" expr
    ///     | expr ("," expr)* ","? ":" expr
    fn matchCase(parser: *Parser) Error!*Node {
        if (parser.eatToken(.Keyword_let, false) orelse
            parser.eatToken(.Keyword_const, false)) |let_const|
        {
            if (parser.eatToken(.Identifier, true)) |_| {
                const node = try parser.arena.create(Node.MatchCatchAll);
                node.* = .{
                    .tok = let_const,
                    .colon = try parser.expectToken(.Colon, true),
                    .expr = try parser.expr(false),
                };
                return &node.base;
            }
            const capture = try parser.primaryExpr(true);
            const node = try parser.arena.create(Node.MatchLet);
            node.* = .{
                .let_const = let_const,
                .capture = capture,
                .colon = try parser.expectToken(.Colon, true),
                .expr = try parser.expr(false),
            };
            return &node.base;
        } else if (parser.eatToken(.Underscore, false)) |u| {
            const node = try parser.arena.create(Node.MatchCatchAll);
            node.* = .{
                .tok = u,
                .colon = try parser.expectToken(.Colon, true),
                .expr = try parser.expr(false),
            };
            return &node.base;
        } else {
            const node = try parser.arena.create(Node.MatchCase);
            node.* = .{
                .lhs = NodeList.init(parser.arena),
                .expr = undefined,
                .colon = undefined,
            };
            var end = false;
            while (true) {
                if (parser.eatToken(.Colon, true)) |colon| {
                    node.colon = colon;
                    break;
                } else if (end) {
                    node.colon = try parser.expectToken(.Colon, true);
                    break;
                }
                try node.lhs.push(try parser.expr(true));
                if (parser.eatToken(.Comma, true) == null) end = true;
            }
            node.expr = try parser.expr(false);
            return &node.base;
        }
    }

    fn reportErr(parser: *Parser, msg: []const u8, tok: *Token) Error {
        try parser.errors.add(msg, tok.start, .err);
        return error.ParseError;
    }

    /// skips nl begins and ends
    fn skipNl(parser: *Parser) void {
        _ = parser.eatToken(.Nl, true);
    }

    const TokAndId = struct {
        index: TokenIndex,
        id: Token.Id,
        tok: *Token,
    };

    fn sawEnd(parser: *Parser) bool {
        const res = parser.it.prev().?.id == .End;
        _ = parser.it.next();
        return res;
    }

    fn eatTokenId(parser: *Parser, id: Token.Id, skip_nl: bool) ?TokAndId {
        var next_tok = parser.it.next().?;
        while (true) {
            switch (next_tok.id) {
                // skip nl, begin and end if they are not meaningful
                .Nl, .Begin, .End => if (!skip_nl) break,
                // always skip comments
                .Comment => {},
                else => break,
            }
            next_tok = parser.it.next().?;
        }
        if (next_tok.id == id) {
            return TokAndId{
                // parser.it.index points to the next token
                .index = @intCast(TokenIndex, parser.it.index - 1),
                .id = id,
                .tok = next_tok,
            };
        } else {
            _ = parser.it.prev();
            return null;
        }
    }

    /// skip nl and eat token id, resets index if not found
    fn eatTokenIdNoNl(parser: *Parser, id: Token.Id, skip_nl: bool) ?TokAndId {
        const start_index = parser.it.index;
        _ = parser.eatToken(.Nl, false);
        if (parser.eatToken(.End, false) == null)
            return parser.eatTokenId(id, true);

        parser.it.set(start_index);
        return null;
    }

    fn eatToken(parser: *Parser, id: Token.Id, skip_nl: bool) ?TokenIndex {
        const wrapped = parser.eatTokenId(id, skip_nl) orelse return null;
        return wrapped.index;
    }

    /// skip nl and eat token id, resets index if not found
    fn eatTokenNoNl(parser: *Parser, id: Token.Id, skip_nl: bool) ?TokenIndex {
        return if (parser.eatTokenIdNoNl(id, skip_nl)) |some| some.index else null;
    }

    fn expectToken(parser: *Parser, id: Token.Id, skip_nl: bool) !TokenIndex {
        if (parser.eatToken(id, skip_nl)) |tok| return tok;
        return parser.reportErr("unexpected token", parser.it.peek().?);
    }
};
