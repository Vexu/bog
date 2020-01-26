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
        StackOverflow,
        TypeName,
        Unwrap,
    };

    pub fn render(err: Error, stream: var) !void {
        switch (err.kind) {
            .AssignmentRValue => try stream.write("assignment cannot be used as an r-value"),
            .JumpRValue => try stream.print("'{}' cannot be used as an r-value", .{err.tok.string()}),
            .UnexpectedToken => try stream.print("unexpected token '{}'", .{err.tok.string()}),
            .PrimaryExpr => try stream.print("expected Identifier, String, Number, true, false, '(', '{{', '[', error, import, if, while, for, match. found '{}'", .{err.tok.string()}),
            .UndeclaredIdentifier => try stream.print("use of undeclared identifier '{}'", .{err.tok.id.Identifier}),
            .StackOverflow => try stream.write("operation caused stack of 250 to run out"),
            .TypeName => try stream.write("expected type name"),
            .Unwrap => try stream.write("expected identifier, '{', '(', '[' or 'error'"),
        }
    }
};

pub const ErrorList = std.SegmentedList(Error, 0);

pub fn parse(arena: *heap.ArenaAllocator, it: *TokenList.Iterator, errors: *ErrorList) ParseError!NodeList {
    var parser = .{
        .arena = &arena.allocator,
        .it = it,
        .errors = errors,
    };
    var list = NodeList.init(parser.arena);
    while (true) {
        try list.push((try next) orelse break);
    }
    return list;
}

pub const ParseError = error{ParseError} || Allocator.Error;

pub const Parser = struct {
    it: *TokenList.Iterator,
    errors: *ErrorList,
    arena: *Allocator,

    const LRValue = enum {
        L,
        R,
    };

    /// root : (stmt NL)* EOF
    pub fn next(parser: *ParseError) ParseError!?*Node {
        if (parser.eatToken(.Eof, true)) |_| return null;
        const res = try parser.stmt();
        try parser.expectToken(.Nl, false);
        return res;
    }

    /// stmt : let | expr.l
    fn stmt(parser: *Parser) ParseError!*Node {
        if (parser.let()) |node| return node;
        return parser.expr(.L, false);
    }

    /// let : "let" unwrap "=" expr.r
    fn let(parser: *Parser) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_let, false) orelse return null;
        const unwrap = try parser.unwrap();
        const eq_tok = try parser.expectToken(.Equal, true);
        parser.skipNl();
        const body = try parser.expr(.R, false);
        const node = try parser.arena.create(Node.Let);
        node.* = .{
            .let_tok = tok,
            .unwrap = unwrap,
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
    ///     | "error" "(" unwrap ")"
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
        if (parser.eatToken(.LBrace)) |tok| {
            node.r_tok = tok;
            return error.ParseError; // TODO

            node.l_tok = try parser.expectToken(.RBrace, true);
        } else if (parser.eatToken(.LParen)) |tok| {
            node.r_tok = tok;
            return error.ParseError; // TODO

            node.l_tok = try parser.expectToken(.RParen, true);
        } else if (parser.eatToken(.LBracket)) |tok| {
            node.r_tok = tok;
            return error.ParseError; // TODO

            node.l_tok = try parser.expectToken(.RBracket, true);
        } else if (parser.eatToken(.Keyword_error)) |tok| {
            node.r_tok = tok;
            _ = try parser.expectToken(.LParen, true);
            node.op = .{ .Error = try parser.unwrap() };
            node.l_tok = try parser.expectToken(.RParen, true);
        } else parser.reportErr(.Unwrap, parser.it.peek().?);
        return &node.base;
    }

    fn unwrapDiscard(parser: *Parser) ParseError!?*Node {
        if (parser.eatToken(.Underscore, true)) |tok| {
            const node = try parser.arena.create(Node.SingleToken);
            node.* = .{
                .base = .{ .id = .Discard },
                .tok = tok,
            };
            return &node.base;
        }
        return null;
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
        while (parser.eatToken(.RParen, true) == null) {
            const param = try parser.arena.create(Node.Fn);
            param.* = .{
                .value = try parser.unwrap(),
                .comma = parser.eatToken(.Comma),
            };
            try node.params.push(&param.base);
            if (param.comma == null) break;
        }
        node.r_paren = try parser.expectToken(.RParen, true);
        parser.skipNl();
        node.body = try parser.expr(.R, false);
        return &node.base;
    }

    /// jump_expr : "return" expr.r | "break" | "continue"
    fn jumpExpr(parser: *Parser, lr_value: LRValue) ParseError!?*Node {
        const tok = parser.eatToken(.Keyword_return, false) orelse
            parser.eatToken(.Keyword_break, false) orelse
            parser.eatToken(.Keyword_continue, false) orelse
            return null;
        if (lr_value != .L) {
            return parser.reportErr(.JumpRValue, tok);
        }
        const node = try parser.arena.create(Node.Jump);
        node.* = .{
            .tok = tok,
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
    fn comparisionExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.rangeExpr(lr_value, skip_nl)) orelse return null;

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
                .tok = tok + 1,
                .op = .Is,
                .type_id = try parser.typeName(),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// type_name : "none" | "int" | "num" | "bool" | "str" | "tuple" | "map" | "list" | "error" | "range" | "fn"
    fn typeName(parser: *Parser) ParseError!TypeId {
        return if (parser.eatToken(.Keyword_error, true)) |_|
            .Error
        else if (parser.eatToken(.Keyword_fn, true)) |_|
            .Fn
        else if (parser.eatToken(.Identifier, true)) |tok|
            if (mem.eql(u8, tok.id.Identifier, "none"))
                .None
            else if (mem.eql(u8, tok.id.Identifier, "int"))
                .Int
            else if (mem.eql(u8, tok.id.Identifier, "num"))
                .Num
            else if (mem.eql(u8, tok.id.Identifier, "bool"))
                .Bool
            else if (mem.eql(u8, tok.id.Identifier, "str"))
                .Str
            else if (mem.eql(u8, tok.id.Identifier, "tuple"))
                .Tuple
            else if (mem.eql(u8, tok.id.Identifier, "map"))
                .Map
            else if (mem.eql(u8, tok.id.Identifier, "list"))
                .List
            else if (mem.eql(u8, tok.id.Identifier, "range"))
                TypeId.Range
            else
                return parser.reportErr(.TypeName, tok)
        else
            return parser.reportErr(.TypeName, parser.it.peek().?);
    }

    /// range_expr : bit_expr ("..." bit_expr.r)?
    fn rangeExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.bitExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Ellipsis, skip_nl)) |tok| {
            parser.skipNl();
            const node = try parser.arena.create(Node.Infix);
            node.* = .{
                .tok = tok,
                .op = .Range,
                .rhs = try parser.bitExpr(.R, skip_nl),
            };
            lhs = &node.base;
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr.r)* | ("|" shift_expr.r)* | ("|" shift_expr.r)*) | ("catch" ("let" unwrap ":")? expr)
    fn bitExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.shiftExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        if (parser.eatToken(.Ampersand, skip_nl)) |t| {
            // &
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.shiftExpr(.R, skip_nl)).?;
                lhs = try parser.infix(lhs, tok, rhs);
                if (parser.eatToken(.Ampersand, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Pipe, skip_nl)) |t| {
            // |
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.shiftExpr(.R, skip_nl)).?;
                lhs = try parser.infix(lhs, tok, rhs);
                if (parser.eatToken(.Pipe, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Caret, skip_nl)) |t| {
            // ^
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.shiftExpr(.R, skip_nl)).?;
                lhs = try parser.infix(lhs, tok, rhs);
                if (parser.eatToken(.Caret, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Keyword_catch, skip_nl)) |_| {
            // catch
            try parser.builder.jumpNotErr(lhs);
            defer parser.builder.finishJumps(1);
            if (parser.eatToken(.Pipe, true)) |_| {
                @panic("TODO");
                // const unwrap = try parser.unwrap();
                // lhs = try parser.builder.unwrap(lhs, unwrap);
                // _ = try parser.expectToken(.Pipe, true);
            }
            parser.skipNl();
            if (try parser.expr(lr_value, skip_nl)) |rhs| {
                try parser.builder.move(rhs, lhs);
            }
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr.r)
    fn shiftExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.addExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArrArr, skip_nl) orelse
            parser.eatToken(.RArrArr, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.addExpr(.R, skip_nl)).?;
            return try parser.infix(lhs, tok, rhs);
        }
        return lhs;
    }

    /// add_expr : mul_expr (("-" | "+") mul_expr.r)*
    fn addExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.mulExpr(lr_value, skip_nl)) orelse return null;

        while (parser.eatToken(.Minus, skip_nl) orelse
            parser.eatToken(.Plus, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.mulExpr(.R, skip_nl)).?;
            lhs = try parser.infix(lhs, tok, rhs);
        }
        return lhs;
    }

    /// mul_expr : cast_expr (("*" | "/" | "//" | "%") cast_expr.r)*
    fn mulExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.castExpr(lr_value, skip_nl)) orelse return null;

        while (parser.eatToken(.Asterisk, skip_nl) orelse
            parser.eatToken(.Slash, skip_nl) orelse
            parser.eatToken(.SlashSlash, skip_nl) orelse
            parser.eatToken(.Percent, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.castExpr(.R, skip_nl)).?;
            lhs = try parser.infix(lhs, tok, rhs);
        }

        return lhs;
    }

    /// cast_expr : prefix_expr ("as" type_name)?
    fn castExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.prefixExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Keyword_as, skip_nl)) |tok| {
            const id = try parser.typeName();
            lhs = try parser.builder.cast(lhs, tok, id);
        }

        return lhs;
    }

    /// prefix_expr
    ///     : ("try" | "-" | "+" | "~") power_expr.r
    ///     | power_expr
    fn prefixExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        if (parser.eatToken(.Keyword_try, skip_nl) orelse
            parser.eatToken(.Minus, skip_nl) orelse
            parser.eatToken(.Plus, skip_nl) orelse
            parser.eatToken(.Tilde, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.powerExpr(.R, skip_nl)).?;
            return parser.builder.prefix(tok, rhs) catch |e| switch (e) {
                error.StackOverflow => return parser.reportErr(.StackOverflow, tok),
                else => |err| return err,
            };
        }
        return try parser.powerExpr(lr_value, skip_nl);
    }

    /// power_expr : primary_expr suffix_expr*  ([.l assign?] | ("**" power_expr.r)?)
    fn powerExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var primary = (try parser.primaryExpr(lr_value, skip_nl)) orelse return null;
        primary = try parser.suffixExpr(primary, skip_nl);
        if (parser.eatToken(.AsteriskAsterisk, skip_nl)) |tok| {
            parser.skipNl();
            const rhs = (try parser.powerExpr(.R, skip_nl)).?;
            return try parser.infix(primary, tok, rhs);
        }
        return try parser.assign(lr_value, primary, skip_nl);
    }

    /// suffix_expr
    ///     : "[" bool_expr.r "]"
    ///     | "(" (bool_expr.r ",")* ")"
    ///     | "." IDENTIFIER
    fn suffixExpr(parser: *Parser, lhs: RegRef, skip_nl: bool) ParseError!RegRef {
        while (parser.eatToken(.LBracket, skip_nl) orelse
            parser.eatToken(.LParen, skip_nl) orelse
            parser.eatToken(.Period, skip_nl)) |tok|
        {
            // TODO
            return error.Unimplemented;
            // const rhs = (try parser.boolExpr(.R, true)).?;
            // lhs = try parser.infix(lhs, tok, rhs);
        }

        return lhs;
    }

    /// assign
    ///     : "=" expr.r
    ///     | ("+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") bit_expr.r
    fn assign(parser: *Parser, lr_value: LRValue, lhs: RegRef, skip_nl: bool) ParseError!?RegRef {
        // assignment cannot happen in places where NL is not necessary
        if (parser.eatToken(.Equal, false) orelse
            parser.eatToken(.MinusEqual, false) orelse
            parser.eatToken(.AsteriskEqual, false) orelse
            parser.eatToken(.AsteriskAsteriskEqual, false) orelse
            parser.eatToken(.SlashEqual, false) orelse
            parser.eatToken(.SlashSlashEqual, false) orelse
            parser.eatToken(.PercentEqual, false) orelse
            parser.eatToken(.LArrArrEqual, false) orelse
            parser.eatToken(.RArrArrEqual, false) orelse
            parser.eatToken(.AmpersandEqual, false) orelse
            parser.eatToken(.PipeEqual, false) orelse
            parser.eatToken(.CaretEqual, false)) |tok|
        {
            if (lr_value != .L) {
                return parser.reportErr(.AssignmentRValue, tok);
            }
            std.debug.assert(!skip_nl);
            const rhs = if (tok.id == .Equal)
                (try parser.expr(.R, false)).?
            else
                (try parser.bitExpr(.R, false)).?;
            try parser.builder.assign(lhs, tok, rhs);
            return null;
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
    fn primaryExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        if (parser.eatToken(.Number, skip_nl) orelse
            parser.eatToken(.Integer, skip_nl) orelse
            parser.eatToken(.String, skip_nl) orelse
            parser.eatToken(.Keyword_true, skip_nl) orelse
            parser.eatToken(.Keyword_false, skip_nl)) |tok|
        {
            return parser.builder.constant(tok) catch |e| switch (e) {
                error.StackOverflow => return parser.reportErr(.StackOverflow, tok),
                else => |err| return err,
            };
        }
        if (parser.eatToken(.Identifier, skip_nl)) |tok| {
            return parser.builder.declRef(tok) catch |e| switch (e) {
                error.UndeclaredIdentifier => return parser.reportErr(.UndeclaredIdentifier, tok),
                else => |err| return err,
            };
        }
        if (parser.eatToken(.Keyword_error, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const val = (try parser.expr(.R, true)).?;
            _ = try parser.expectToken(.RParen, true);
            return try parser.builder.buildErr(tok, val);
        }
        if (parser.eatToken(.Keyword_import, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const str = try parser.expectToken(.String, true);
            _ = try parser.expectToken(.RParen, true);
            return parser.builder.import(tok, str) catch |e| switch (e) {
                error.StackOverflow => return parser.reportErr(.StackOverflow, tok),
                else => |err| return err,
            };
        }
        if (parser.eatToken(.LParen, skip_nl)) |tok| {
            if (parser.eatToken(.Nl, false)) |_| {
                // block
                return try parser.block(lr_value);
            } else if (parser.eatToken(.RParen, false)) |t| {
                return parser.builder.constant(t) catch |e| switch (e) {
                    error.StackOverflow => return parser.reportErr(.StackOverflow, tok),
                    else => |err| return err,
                };
            } else {
                return error.Unimplemented;
                // const start = parser.builder.maybeTuple();
                // var val = try parser.expr(.R);
                // tuple or grouped expr
            }
        }
        if (parser.eatToken(.LBrace, skip_nl)) |tok| {
            //     | "{" ((IDENTIFIER | STRING) ":" expr.r ",")* "}"
            return error.Unimplemented;
        }
        if (parser.eatToken(.LBracket, skip_nl)) |tok| {
            //     | "[" (expr.r ",")* "]"
            const arr = try parser.builder.buildList(tok);
            var count: u32 = 0;
            var rbracket: *Token = undefined;
            if (parser.eatToken(.RBracket, true)) |t| {
                rbracket = t;
            } else {
                while (true) {
                    const val = (try parser.expr(.R, true)).?;
                    try parser.builder.listPush(val);
                    if (parser.eatToken(.Comma, true) == null) break;
                }
                rbracket = try parser.expectToken(.RBracket, true);
            }
            return try parser.builder.finishList(rbracket, count);
        }
        if (try parser.ifExpr(lr_value, skip_nl)) |res| return res;
        if (try parser.whileExpr(lr_value, skip_nl)) |res| return res;
        if (try parser.forExpr(lr_value, skip_nl)) |res| return res;
        if (try parser.matchExpr(lr_value, skip_nl)) |res| return res;
        return parser.reportErr(.PrimaryExpr, parser.it.peek().?);
    }

    /// block : "{" (NL stmt)+ "}"
    fn block(parser: *Parser, lr_value: LRValue) ParseError!?RegRef {
        var last: ?RegRef = null;
        while (true) {
            const res = try parser.stmt();
            if (last) |some|
                try parser.builder.discard(some);
            last = res;
            _ = try parser.expectToken(.Nl, false);
            if (parser.eatToken(.RParen, true)) |tok| {
                if (lr_value == .L and last == null) {
                    // TODO expression has no result
                    return error.Unimplemented;
                }
                return last;
            }
        }
    }

    /// if : "if" "(" bool_expr.r ")" expr ("else" expr)?
    fn ifExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_if, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    /// while : "while" "(" bool_expr.r ")" expr
    fn whileExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_while, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    /// for : "for" "(" ("let" unwrap "in") range_expr.r ")" expr
    fn forExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_for, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    /// match : "match" "(" bool_expr.r ")" "{" (NL match_case ",")+ "}"
    fn matchExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_match, skip_nl) orelse return null;
        return error.Unimplemented;
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
    };

    fn eatTokenId(parser: *Parser, id: Token.Id, skip_nl: bool) ?*TokAndId {
        var next = parser.it.next().?;
        if (skip_nl and next.id == .Nl)
            next = parser.it.next().?;
        if (next.id == id) {
            return TokAndId {
                .index = parser.it.index,
                .id = id,
            };
        } else {
            _ = parser.it.prev();
            return null;
        }
    }

    fn eatToken(parser: *Parser, id: Token.Id, skip_nl: bool) ?*TokenIndex {
        const wrapped = parser.eatToken(id, skip_nl) orelse return null;
        return wrapped.index;
    }

    fn expectToken(parser: *Parser, id: Token.Id, skip_nl: bool) !*Token {
        if (parser.eatToken(id, skip_nl)) |tok| return tok;
        return parser.reportErr(.UnexpectedToken, parser.it.peek().?);
    }
};
