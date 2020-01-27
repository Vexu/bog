const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenList = tokenizer.TokenList;
const TokenIndex = tokenizer.TokenIndex;
const Allocator = mem.Allocator;
const TypeId = @import("value.zig").TypeId;
const ast = @import("ast.zig");
const Node = ast.Node;
const NodeList = ast.NodeList;

pub const Error = struct {
    tok: *Token,
    kind: Kind,

    const Kind = enum {
        AssignmentRValue,
        JumpRValue,
        UnexpectedToken,
        PrimaryExpr,
        UndeclaredIdentifier,
        TypeName,
        Unwrap,
    };

    pub fn render(err: Error, stream: var) !void {
        switch (err.kind) {
            .AssignmentRValue => try stream.write("assignment cannot be used as an r-value"),
            .JumpRValue => try stream.print("'{}' cannot be used as an r-value", .{err.tok.id.string()}),
            .UnexpectedToken => try stream.print("unexpected token '{}'", .{err.tok.id.string()}),
            .PrimaryExpr => try stream.print("expected Identifier, String, Number, true, false, '(', '{{', '[', error, import, if, while, for, match. found '{}'", .{err.tok.id.string()}),
            .UndeclaredIdentifier => try stream.print("use of undeclared identifier '{}'", .{err.tok.id.Identifier}),
            .TypeName => try stream.write("expected type name"),
            .Unwrap => try stream.write("expected identifier, '{', '(', '[' or 'error'"),
        }
    }
};

pub const ErrorList = std.SegmentedList(Error, 0);

pub fn parse(arena: *std.heap.ArenaAllocator, it: TokenList.Iterator, errors: *ErrorList) ParseError!NodeList {
    var parser = Parser{
        .arena = &arena.allocator,
        .it = it,
        .errors = errors,
    };
    var list = NodeList.init(parser.arena);
    while (true) {
        try list.push((try parser.next()) orelse break);
    }
    return list;
}

pub const ParseError = error{ParseError} || Allocator.Error;

pub const Parser = struct {
    it: TokenList.Iterator,
    errors: *ErrorList,
    arena: *Allocator,

    const LRValue = enum {
        L,
        R,
    };

    /// root : (stmt NL)* EOF
    pub fn next(parser: *Parser) ParseError!?*Node {
        if (parser.eatToken(.Eof, true)) |_| return null;
        const res = try parser.stmt();
        _ = parser.eatToken(.Nl, false) orelse {
            _ = try parser.expectToken(.Eof, true); // TODO
            _ = parser.it.prev();
            return res;
        };
        return res;
    }

    /// stmt : let | expr.l
    fn stmt(parser: *Parser) ParseError!*Node {
        if (try parser.let()) |node| return node;
        return parser.expr(.L, false);
    }

    /// let : "let" unwrap "=" expr.r
    fn let(parser: *Parser) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_let, false) orelse return null;
        const unwrapped = try parser.unwrap();
        const eq_tok = try parser.expectToken(.Equal, true);
        parser.skipNl();
        const body = try parser.expr(.R, false);
        const node = try parser.arena.create(Node.Let);
        node.* = .{
            .let_tok = tok,
            .unwrap = unwrapped,
            .eq_tok = eq_tok,
            .body = body,
        };
        return &node.base;
    }

    /// unwrap
    ///     : IDENTIFIER
    ///     | "{" ((expr.r ":")? (unwrap | "_") ",")* "}"
    ///     | "(" ("..." ",")? ((unwrap | "_") ",")* ("," "...")? ")"
    ///     | "[" ("..." ",")? ((unwrap | "_") ",")* ("," "...")? "]"
    ///     | "error" "(" (unwrap | "_") ")"
    fn unwrap(parser: *Parser) ParseError!*Node {
        if (parser.eatToken(.Identifier, true)) |tok| {
            const node = try parser.arena.create(Node.SingleToken);
            node.* = .{
                .base = .{ .id = .Identifier },
                .tok = tok,
            };
            return &node.base;
        }
        const node = try parser.arena.create(Node.Unwrap);
        node.base = .{ .id = .Unwrap };
        if (parser.eatToken(.LBrace, true)) |tok| {
            node.r_tok = tok;
            @panic("TODO");

            // node.l_tok = try parser.expectToken(.RBrace, true);
        } else if (parser.eatToken(.LParen, true)) |tok| {
            node.r_tok = tok;
            try parser.unwrapList(node, .RParen);
        } else if (parser.eatToken(.LBracket, true)) |tok| {
            node.r_tok = tok;
            try parser.unwrapList(node, .RBracket);
        } else if (parser.eatToken(.Keyword_error, true)) |tok| {
            node.r_tok = tok;
            _ = try parser.expectToken(.LParen, true);
            node.op = .{ .Error = try parser.unwrap() };
            node.l_tok = try parser.expectToken(.RParen, true);
        } else return parser.reportErr(.Unwrap, parser.it.peek().?);
        return &node.base;
    }

    fn discard(parser: *Parser) ParseError!?*Node {
        const tok = parser.eatToken(.Underscore, true) orelse return null;
        const node = try parser.arena.create(Node.SingleToken);
        node.* = .{
            .base = .{ .id = .Discard },
            .tok = tok,
        };
        return &node.base;
    }

    /// ("..." ",")? ((unwrap | "_") ",")* ("," "...")?
    fn unwrapList(parser: *Parser, node: *Node.Unwrap, l_tok_id: Token.Id) ParseError!void {
        @panic("TODO");
        // var end = false;
        // while (true) {
        //     if (parser.eatToken(.Ellipsis, true)) |tok| {
        //         if (node.)
        //     }
        //     if (parser.eatToken(l_tok_id, true)) |tok| {
        //         node.r_tok = tok;
        //     } else if (end) {
        //         node.r_tok = try parser.expectToken(l_tok_id, true);
        //         break;
        //     }
        //     const param = try parser.arena.create(Node.ListTupleCallItem);
        //     param.* = .{
        //         .value = try parser.expr(.R, true),
        //         .comma = parser.eatToken(.Comma),
        //     };
        //     try node.op.Call.push(&param.base);
        //     if (param.comma == null) end = true;
        // }
        // lhs = &node.base;
    }

    /// expr
    ///     : fn
    ///     | [.l jump_expr]
    ///     | bool_expr
    fn expr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        if (try parser.func(skip_nl)) |node| return node;
        if (try parser.jumpExpr(lr_value)) |node| return node;
        return parser.boolExpr(lr_value, skip_nl);
    }

    /// fn : "fn" "(" (unwrap ",")* ")" expr.r
    fn func(parser: *Parser, skip_nl: bool) ParseError!?*Node {
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
            const param = try parser.arena.create(Node.ListTupleCallItem);
            param.* = .{
                .value = try parser.unwrap(),
                .comma = parser.eatToken(.Comma, true),
            };
            try node.params.push(&param.base);
            if (param.comma == null) end = true;
        }
        node.r_paren = try parser.expectToken(.RParen, true);
        parser.skipNl();
        node.body = try parser.expr(.R, false);
        return &node.base;
    }

    /// jump_expr : "return" expr.r | "break" | "continue"
    fn jumpExpr(parser: *Parser, lr_value: LRValue) ParseError!?*Node {
        const tok = parser.eatTokenId(.Keyword_return, false) orelse
            parser.eatTokenId(.Keyword_break, false) orelse
            parser.eatTokenId(.Keyword_continue, false) orelse
            return null;
        if (lr_value != .L) {
            return parser.reportErr(.JumpRValue, tok.tok);
        }
        const node = try parser.arena.create(Node.Jump);
        node.* = .{
            .tok = tok.index,
            .op = switch (tok.id) {
                .Keyword_return => .{ .Return = try parser.expr(.R, false) },
                .Keyword_break => .Break,
                .Keyword_continue => .Continue,
                else => unreachable,
            },
        };
        return &node.base;
    }

    /// bool_expr
    ///     : "not" comparision_expr.r
    ///     | comparision_expr ("or" comparision_expr.r)*
    ///     | comparision_expr ("and" comparision_expr.r)*
    fn boolExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        if (parser.eatToken(.Keyword_not, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Prefix);
            node.* = .{
                .op = .BoolNot,
                .rhs = try parser.comparisionExpr(.R, skip_nl),
                .tok = tok,
            };
            return &node.base;
        }
        var lhs = try parser.comparisionExpr(lr_value, skip_nl);

        // TODO improve
        if (parser.eatToken(.Keyword_or, skip_nl)) |t| {
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BoolOr,
                    .rhs = try parser.comparisionExpr(.R, skip_nl),
                };
                lhs = &node.base;
                if (parser.eatToken(.Keyword_or, skip_nl)) |tt| tok = tt else break;
            }
        } else {
            while (parser.eatToken(.Keyword_and, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BoolAnd,
                    .rhs = try parser.comparisionExpr(.R, skip_nl),
                };
                lhs = &node.base;
            }
        }
        return lhs;
    }

    /// comparision_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr.r)?
    ///     | range_expr ("is" type_name)?
    fn comparisionExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.rangeExpr(lr_value, skip_nl);

        if (parser.eatTokenId(.LArr, skip_nl) orelse
            parser.eatTokenId(.LArrEqual, skip_nl) orelse
            parser.eatTokenId(.RArr, skip_nl) orelse
            parser.eatTokenId(.RArrEqual, skip_nl) orelse
            parser.eatTokenId(.EqualEqual, skip_nl) orelse
            parser.eatTokenId(.BangEqual, skip_nl) orelse
            parser.eatTokenId(.Keyword_in, skip_nl)) |tok|
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
                .rhs = try parser.rangeExpr(.R, skip_nl),
            };
            lhs = &node.base;
        } else if (parser.eatToken(.Keyword_is, skip_nl)) |tok| {
            const node = try parser.arena.create(Node.TypeInfix);
            node.* = .{
                .lhs = lhs,
                .tok = tok + 1,
                .op = .Is,
                .type_tok = try parser.typeName(),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// type_name : "none" | "int" | "num" | "bool" | "str" | "tuple" | "map" | "list" | "error" | "range" | "fn"
    fn typeName(parser: *Parser) ParseError!TokenIndex {
        return parser.eatToken(.Keyword_error, true) orelse
            parser.eatToken(.Keyword_fn, true) orelse
            parser.eatToken(.Identifier, true) orelse
            parser.reportErr(.TypeName, parser.it.peek().?);
    }

    /// range_expr : bit_expr ("..." bit_expr.r)?
    fn rangeExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.bitExpr(lr_value, skip_nl);

        if (parser.eatToken(.Ellipsis, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok,
                .op = .Range,
                .rhs = try parser.bitExpr(.R, skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr.r)* | ("|" shift_expr.r)* | ("|" shift_expr.r)*) | ("catch" ("let" unwrap ":")? expr)
    fn bitExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.shiftExpr(lr_value, skip_nl);

        // TODO improve
        if (parser.eatToken(.Ampersand, skip_nl)) |t| {
            // &
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitAnd,
                    .rhs = try parser.shiftExpr(.R, skip_nl),
                };
                lhs = &node.base;
                if (parser.eatToken(.Keyword_or, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Pipe, skip_nl)) |t| {
            // |
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitOr,
                    .rhs = try parser.shiftExpr(.R, skip_nl),
                };
                lhs = &node.base;
                if (parser.eatToken(.Pipe, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Caret, skip_nl)) |t| {
            // ^
            var tok = t;
            while (true) {
                parser.skipNl();
                const node = try parser.arena.create(Node.Infix);
                node.* = .{
                    .lhs = lhs,
                    .tok = tok,
                    .op = .BitXor,
                    .rhs = try parser.shiftExpr(.R, skip_nl),
                };
                lhs = &node.base;
                if (parser.eatToken(.Caret, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Keyword_catch, skip_nl)) |tok| {
            // catch
            const node = try parser.arena.create(Node.Catch);
            node.* = .{
                .tok = tok,
                .lhs = lhs,
                .unwrap = null,
                .colon = null,
                .rhs = undefined,
            };
            if (parser.eatToken(.Keyword_let, true)) |_| {
                node.unwrap = try parser.unwrap();
                node.colon = try parser.expectToken(.Colon, true);
            }
            parser.skipNl();
            node.rhs = try parser.expr(lr_value, skip_nl);
            lhs = &node.base;
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr.r)
    fn shiftExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.addExpr(lr_value, skip_nl);

        if (parser.eatTokenId(.LArrArr, skip_nl) orelse
            parser.eatTokenId(.RArrArr, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = if (tok.id == .LArrArr) .LShift else .RShift,
                .rhs = try parser.addExpr(.R, skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// add_expr : mul_expr (("-" | "+") mul_expr.r)*
    fn addExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.mulExpr(lr_value, skip_nl);

        while (parser.eatTokenId(.Minus, skip_nl) orelse
            parser.eatTokenId(.Plus, skip_nl)) |tok|
        {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = lhs,
                .tok = tok.index,
                .op = if (tok.id == .Minus) .Sub else .Add,
                .rhs = try parser.mulExpr(.R, skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// mul_expr : cast_expr (("*" | "/" | "//" | "%") cast_expr.r)*
    fn mulExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.castExpr(lr_value, skip_nl);

        while (parser.eatTokenId(.Asterisk, skip_nl) orelse
            parser.eatTokenId(.Slash, skip_nl) orelse
            parser.eatTokenId(.SlashSlash, skip_nl) orelse
            parser.eatTokenId(.Percent, skip_nl)) |tok|
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
                .rhs = try parser.castExpr(.R, skip_nl),
            };
            lhs = &node.base;
        }

        return lhs;
    }

    /// cast_expr : prefix_expr ("as" type_name)?
    fn castExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var lhs = try parser.prefixExpr(lr_value, skip_nl);

        if (parser.eatToken(.Keyword_as, skip_nl)) |tok| {
            const node = try parser.arena.create(Node.TypeInfix);
            node.* = .{
                .lhs = lhs,
                .tok = tok + 1,
                .op = .As,
                .type_tok = try parser.typeName(),
            };
            lhs = &node.base;
        }

        return lhs;
    }

    /// prefix_expr
    ///     : ("try" | "-" | "+" | "~") power_expr.r
    ///     | power_expr
    fn prefixExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
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
                    .Minus => .Minus,
                    .Plus => .Plus,
                    .Tilde => .BitNot,
                    else => unreachable,
                },
                .tok = tok.index,
                .rhs = try parser.powerExpr(.R, skip_nl),
            };
            return &node.base;
        }
        return try parser.powerExpr(lr_value, skip_nl);
    }

    /// power_expr : primary_expr suffix_expr*  ([.l assign?] | ("**" power_expr.r)?)
    fn powerExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
        var primary = try parser.primaryExpr(lr_value, skip_nl);
        primary = try parser.suffixExpr(primary, skip_nl);
        if (parser.eatToken(.AsteriskAsterisk, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .lhs = primary,
                .tok = tok,
                .op = .Pow,
                .rhs = try parser.powerExpr(.R, skip_nl),
            };
            return &node.base;
        }
        return try parser.assign(lr_value, primary, skip_nl);
    }

    /// suffix_expr
    ///     : "[" expr.r "]"
    ///     | "(" (expr.r ",")* ")"
    ///     | "." IDENTIFIER
    fn suffixExpr(parser: *Parser, primary: *Node, skip_nl: bool) ParseError!*Node {
        var lhs = primary;
        while (true) {
            if (parser.eatToken(.LBracket, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .ArrAccess = try parser.expr(.R, true) },
                    .r_tok = try parser.expectToken(.RBracket, true),
                };
                lhs = &node.base;
            } else if (parser.eatToken(.LParen, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .{ .Call = NodeList.init(parser.arena) },
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
                    const param = try parser.arena.create(Node.ListTupleCallItem);
                    param.* = .{
                        .value = try parser.expr(.R, true),
                        .comma = parser.eatToken(.Comma, true),
                    };
                    try node.op.Call.push(&param.base);
                    if (param.comma == null) end = true;
                }
                lhs = &node.base;
            } else if (parser.eatToken(.Period, skip_nl)) |tok| {
                parser.skipNl();
                const node = try parser.arena.create(Node.Suffix);
                node.* = .{
                    .lhs = lhs,
                    .l_tok = tok,
                    .op = .Member,
                    .r_tok = try parser.expectToken(.Identifier, true),
                };
                lhs = &node.base;
            } else {
                return lhs;
            }
        }
    }

    /// assign
    ///     : "=" expr.r
    ///     | ("+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") bit_expr.r
    fn assign(parser: *Parser, lr_value: LRValue, lhs: *Node, skip_nl: bool) ParseError!*Node {
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
            if (lr_value != .L) {
                return parser.reportErr(.AssignmentRValue, tok.tok);
            }
            std.debug.assert(!skip_nl);
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
                    .RArrArrEqual => .RShfitAssign,
                    .AmpersandEqual => .BitAndAssign,
                    .PipeEqual => .BitOrAssign,
                    .CaretEqual => .BitXOrAssign,
                    else => unreachable,
                },
                .rhs = if (tok.id == .Equal)
                    try parser.expr(.R, false)
                else
                    try parser.bitExpr(.R, false),
            };
            return &node.base;
        }

        return lhs;
    }

    /// primary_expr
    ///     : IDENTIFIER
    ///     | STRING
    ///     | NUMBER
    ///     | "true"
    ///     | "false"
    ///     | "(" (expr.r ",")* ")"
    ///     | "{" (expr.r ":" expr.r ",")* "}"
    ///     | "[" (expr.r ",")* "]"
    ///     | "error" "(" expr.r ")"
    ///     | "import" "(" STRING ")"
    ///     | block
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    fn primaryExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!*Node {
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
                    .Number => .Num,
                    .Integer => .Int,
                    .String => .Str,
                    .Keyword_true, .Keyword_false => .Bool,
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
        if (parser.eatToken(.Keyword_error, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const node = try parser.arena.create(Node.Error);
            node.* = .{
                .tok = tok,
                .value = try parser.expr(.R, true),
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
            };
            _ = try parser.expectToken(.RParen, true);
            return &node.base;
        }
        if (parser.eatToken(.LParen, skip_nl)) |tok| {
            if (parser.eatToken(.RParen, true)) |_| {
                const node = try parser.arena.create(Node.Literal);
                node.* = .{
                    .tok = tok,
                    .kind = .None,
                };
                return &node.base;
            } else {
                const first = try parser.expr(.R, true);
                if (parser.eatToken(.RParen, true)) |r_tok| {
                    const node = try parser.arena.create(Node.Grouped);
                    node.* = .{
                        .l_tok = tok,
                        .expr = first,
                        .r_tok = r_tok,
                    };
                    return &node.base;
                }
                const node = try parser.arena.create(Node.ListTupleMapBlock);
                node.* = .{
                    .base = .{ .id = .Tuple },
                    .l_tok = tok,
                    .values = NodeList.init(parser.arena),
                    .r_tok = undefined,
                };
                var last = first;
                while (true) {
                    const comma = try parser.expectToken(.Comma, true);
                    const item = try parser.arena.create(Node.ListTupleCallItem);
                    item.* = .{
                        .value = last,
                        .comma = comma,
                    };
                    try node.values.push(&item.base);
                    if (parser.eatToken(.RParen, true)) |r_tok| {
                        node.r_tok = r_tok;
                        return &node.base;
                    }
                    last = try parser.expr(.R, true);
                }
            }
        }
        if (parser.eatToken(.LBrace, skip_nl)) |tok| {
            if (parser.eatToken(.Nl, false)) |_| {
                // block
                return try parser.block(lr_value, tok);
            } else {
                // map
                const node = try parser.arena.create(Node.ListTupleMapBlock);
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
                    var value = try parser.expr(.R, true);
                    var colon: ?TokenIndex = null;
                    if (parser.eatToken(.Colon, true)) |col| {
                        colon = col;
                        key = value;
                        value = try parser.expr(.R, true);
                    }
                    const item = try parser.arena.create(Node.MapItem);
                    item.* = .{
                        .key = key,
                        .colon = colon,
                        .value = value,
                        .comma = try parser.expectToken(.Comma, true),
                    };
                    try node.values.push(&item.base);
                    if (item.comma == null) end = true;
                }
            }
        }
        if (parser.eatToken(.LBracket, skip_nl)) |tok| {
            // list
            parser.skipNl();
            const node = try parser.arena.create(Node.ListTupleMapBlock);
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
                const val = try parser.arena.create(Node.ListTupleCallItem);
                val.* = .{
                    .value = try parser.expr(.R, true),
                    .comma = parser.eatToken(.Comma, true),
                };
                try node.values.push(&val.base);
                if (val.comma == null) end = true;
            }
            return &node.base;
        }
        if (try parser.ifExpr(lr_value, skip_nl)) |res| return res;
        if (try parser.whileExpr(lr_value, skip_nl)) |res| return res;
        if (try parser.forExpr(lr_value, skip_nl)) |res| return res;
        if (try parser.matchExpr(lr_value, skip_nl)) |res| return res;
        return parser.reportErr(.PrimaryExpr, parser.it.peek().?);
    }

    /// block : "{" (NL stmt)+ "}"
    fn block(parser: *Parser, lr_value: LRValue, l_tok: TokenIndex) ParseError!*Node {
        const node = try parser.arena.create(Node.ListTupleMapBlock);
        node.* = .{
            .base = .{ .id = .Block },
            .values = NodeList.init(parser.arena),
            .l_tok = l_tok,
            .r_tok = undefined,
        };
        while (true) {
            try node.values.push(try parser.stmt());
            _ = try parser.expectToken(.Nl, false);
            if (parser.eatToken(.RBrace, true)) |tok| {
                node.r_tok = tok;
                return &node.base;
            }
        }
    }

    /// if : "if" "(" bool_expr.r ")" expr ("else" expr)?
    fn ifExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_if, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const node = try parser.arena.create(Node.If);
        node.* = .{
            .if_tok = tok,
            .cond = try parser.boolExpr(.R, true),
            .r_paren = try parser.expectToken(.RParen, true),
            .if_body = try parser.expr(lr_value, skip_nl),
            .else_tok = parser.eatToken(.Keyword_else, skip_nl),
            .else_body = null,
        };
        if (node.else_tok != null) {
            node.else_body = try parser.expr(lr_value, skip_nl);
        }
        return &node.base;
    }

    /// while : "while" "(" bool_expr.r ")" expr
    fn whileExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_while, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const node = try parser.arena.create(Node.While);
        node.* = .{
            .while_tok = tok,
            .cond = try parser.boolExpr(.R, true),
            .r_paren = try parser.expectToken(.RParen, true),
            .body = try parser.expr(lr_value, skip_nl),
        };
        return &node.base;
    }

    /// for : "for" "(" ("let" unwrap "in") range_expr.r ")" expr
    fn forExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_for, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const unwrapped = if (parser.eatToken(.Keyword_let, true)) |_|
            try parser.unwrap()
        else
            null;
        const node = try parser.arena.create(Node.For);
        node.* = .{
            .for_tok = tok,
            .unwrap = unwrapped,
            .in_tok = if (unwrapped != null) try parser.expectToken(.Keyword_in, true) else null,
            .cond = try parser.rangeExpr(.R, true),
            .r_paren = try parser.expectToken(.RParen, true),
            .body = try parser.expr(lr_value, skip_nl),
        };
        return &node.base;
    }

    /// match : "match" "(" bool_expr.r ")" "{" (NL match_case ",")+ "}"
    fn matchExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_match, skip_nl) orelse return null;
        _ = try parser.expectToken(.LParen, true);
        const node = try parser.arena.create(Node.Match);
        node.* = .{
            .match_tok = tok,
            .expr = try parser.boolExpr(.R, true),
            .r_paren = try parser.expectToken(.RParen, true),
            .body = NodeList.init(parser.arena),
            .body_r_brace = undefined,
        };
        _ = try parser.expectToken(.LBrace, true);
        _ = try parser.expectToken(.Nl, false);
        while (true) {
            try node.body.push(try parser.matchCase(lr_value));
            _ = try parser.expectToken(.Nl, false);
            if (parser.eatToken(.RBrace, true)) |r_tok| {
                node.body_r_brace = r_tok;
                break;
            }
        }
        return &node.base;
    }

    /// match_case
    ///     : "let" unwrap ":" expr
    ///     | "_" ":" expr
    ///     | (expr.r ",")+ ":" expr
    fn matchCase(parser: *Parser, lr_value: LRValue) ParseError!*Node {
        if (parser.eatToken(.Underscore, false)) |tok| {
            return parser.finishCatchAll(lr_value, tok);
        } else if (parser.eatToken(.Keyword_let, false)) |_| {
            if (parser.eatToken(.Identifier, true)) |tok| {
                return parser.finishCatchAll(lr_value, tok);
            }
            const unwrapped = try parser.unwrap();
            std.debug.assert(unwrapped.id == .Unwrap);
            _ = try parser.expectToken(.Colon, true);
            const node = try parser.arena.create(Node.MatchLet);
            node.* = .{
                .unwrap = @fieldParentPtr(Node.Unwrap, "base", unwrapped),
                .expr = try parser.expr(lr_value, false),
            };
            return &node.base;
        } else {
            const node = try parser.arena.create(Node.MatchCase);
            node.* = .{
                .lhs = NodeList.init(parser.arena),
                .colon = undefined,
                .expr = undefined,
            };
            while (true) {
                const val = try parser.arena.create(Node.ListTupleCallItem);
                val.* = .{
                    .value = try parser.expr(.R, true),
                    .comma = parser.eatToken(.Comma, true),
                };
                try node.lhs.push(&val.base);
                if (val.comma == null) break;
            }
            node.colon = try parser.expectToken(.Colon, true);
            node.expr = try parser.expr(lr_value, false);
            return &node.base;
        }
    }

    fn finishCatchAll(parser: *Parser, lr_value: LRValue, tok: TokenIndex) ParseError!*Node {
        _ = try parser.expectToken(.Colon, true);
        const node = try parser.arena.create(Node.MatchCatchAll);
        node.* = .{
            .tok = tok,
            .expr = try parser.expr(lr_value, false),
        };
        return &node.base;
    }

    fn reportErr(parser: *Parser, kind: Error.Kind, tok: *Token) ParseError {
        try parser.errors.push(.{
            .kind = kind,
            .tok = tok,
        });
        return error.ParseError;
    }

    fn skipNl(parser: *Parser) void {
        _ = parser.eatToken(.Nl, true);
    }

    const TokAndId = struct {
        index: TokenIndex,
        id: Token.Id,
        tok: *Token,
    };

    fn eatTokenId(parser: *Parser, id: Token.Id, skip_nl: bool) ?TokAndId {
        var next_tok = parser.it.next().?;
        while (next_tok.id == .Comment)
            next_tok = parser.it.next().?;
        if (skip_nl and next_tok.id == .Nl)
            next_tok = parser.it.next().?;
        if (next_tok.id == id) {
            return TokAndId{
                .index = @intCast(TokenIndex, parser.it.index - 1),
                .id = id,
                .tok = next_tok,
            };
        } else {
            _ = parser.it.prev();
            return null;
        }
    }

    fn eatToken(parser: *Parser, id: Token.Id, skip_nl: bool) ?TokenIndex {
        const wrapped = parser.eatTokenId(id, skip_nl) orelse return null;
        return wrapped.index;
    }

    fn expectToken(parser: *Parser, id: Token.Id, skip_nl: bool) !TokenIndex {
        if (parser.eatToken(id, skip_nl)) |tok| return tok;
        return parser.reportErr(.UnexpectedToken, parser.it.peek().?);
    }
};
