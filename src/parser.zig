const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenList = tokenizer.TokenList;
const Allocator = mem.Allocator;
const bytecode = @import("bytecode.zig");
const Builder = bytecode.Builder;
const RegRef = bytecode.RegRef;

pub const Parser = struct {
    builder: *Builder,
    token_it: *TokenList.Iterator,

    pub fn parse(builder: *Builder, tokens: *TokenList.Iterator) !void {
        var parser = Parser{
            .builder = builder,
            .token_it = tokens,
        };
        try parser.root();
    }

    const LRValue = enum {
        L,
        R,
    };

    /// root : (stmt NL)* EOF
    fn root(parser: *Parser) !void {
        while (true) {
            if (parser.eatToken(.Eof, true)) |_| return;
            const res = try parser.stmt();
            if (res) |some|
                try parser.builder.discard(some);
            if (parser.eatToken(.Nl, false) == null) {
                _ = try parser.expectToken(.Eof, true);
                return;
            }
        }
    }

    /// stmt : let | expr.l
    fn stmt(parser: *Parser) !?RegRef {
        return if (try parser.let())
            null
        else
            try parser.expr(.L, false);
    }

    /// let : "let" unwrap "=" expr.r
    fn let(parser: *Parser) anyerror!bool {
        const tok = parser.eatToken(.Keyword_let, false) orelse return false;
        return error.Unimplemented;
    }

    /// expr
    ///     : fn
    ///     | [.l jump_expr]
    ///     | bool_expr
    fn expr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!?RegRef {
        return if (try parser.func(skip_nl)) |val|
            val
        else if (try parser.jumpExpr(lr_value))
            null
        else
            try parser.boolExpr(lr_value, skip_nl);
    }

    /// fn : "fn" "(" (unwrap ",")* ")" expr
    fn func(parser: *Parser, skip_nl: bool) anyerror!?RegRef {
        const tok = parser.eatToken(.Keyword_fn, skip_nl) orelse return null;
        return error.Unimplemented;
        // try parser.builder.beginFunc(tok);
        // try parser.expectToken(.LParen, true);
        // try parser.expectToken(.LParen, true);
        // const res = try parser.expr(.R true);
        // return try parser.builder.endFunc(tok, res);
    }

    /// jump_expr : "return" expr.r | "break" | "continue"
    fn jumpExpr(parser: *Parser, lr_value: LRValue) anyerror!bool {
        const tok = parser.eatToken(.Keyword_return, false) orelse
            parser.eatToken(.Keyword_break, false) orelse
            parser.eatToken(.Keyword_continue, false) orelse
            return false;
        if (lr_value != .L) {
            // TODO return, break, continue do not produce values
            return error.ParseError;
        }
        return error.Unimplemented;
        // return true;
    }

    /// bool_expr : comparision_expr (("or" comparision_expr.r)* | ("and" comparision_expr.r)*)
    fn boolExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.comparisionExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        if (parser.eatToken(.Keyword_or, skip_nl)) |t| {
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.comparisionExpr(.R, skip_nl)).?;
                lhs = try parser.builder.infix(lhs, tok, rhs);
                if (parser.eatToken(.Keyword_or, skip_nl)) |tt| tok = tt else break;
            }
        } else {
            while (parser.eatToken(.Keyword_and, skip_nl)) |tok| {
                parser.skipNl();
                const rhs = (try parser.comparisionExpr(.R, skip_nl)).?;
                lhs = try parser.builder.infix(lhs, tok, rhs);
            }
        }
        return lhs;
    }

    /// comparision_expr : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in"  | "is") range_expr.r)
    fn comparisionExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.rangeExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArr, skip_nl) orelse
            parser.eatToken(.LArrEqual, skip_nl) orelse
            parser.eatToken(.RArr, skip_nl) orelse
            parser.eatToken(.RArrEqual, skip_nl) orelse
            parser.eatToken(.EqualEqual, skip_nl) orelse
            parser.eatToken(.BangEqual, skip_nl) orelse
            parser.eatToken(.Keyword_in, skip_nl) orelse
            parser.eatToken(.Keyword_is, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.rangeExpr(.R, skip_nl)).?;
            lhs = try parser.builder.infix(lhs, tok, rhs);
        }
        return lhs;
    }

    /// range_expr : bit_expr ("..." bit_expr.r)?
    fn rangeExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.bitExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Ellipsis, skip_nl)) |tok| {
            parser.skipNl();
            const rhs = (try parser.bitExpr(.R, skip_nl)).?;
            lhs = try parser.builder.infix(lhs, tok, rhs);
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr.r)* | ("|" shift_expr.r)* | ("|" shift_expr.r)*) | ("catch" ("|" unwrap "|")? expr)
    fn bitExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.shiftExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        if (parser.eatToken(.Ampersand, skip_nl)) |t| {
            // &
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.shiftExpr(.R, skip_nl)).?;
                lhs = try parser.builder.infix(lhs, tok, rhs);
                if (parser.eatToken(.Ampersand, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Pipe, skip_nl)) |t| {
            // |
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.shiftExpr(.R, skip_nl)).?;
                lhs = try parser.builder.infix(lhs, tok, rhs);
                if (parser.eatToken(.Pipe, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Caret, skip_nl)) |t| {
            // ^
            var tok = t;
            while (true) {
                parser.skipNl();
                const rhs = (try parser.shiftExpr(.R, skip_nl)).?;
                lhs = try parser.builder.infix(lhs, tok, rhs);
                if (parser.eatToken(.Caret, skip_nl)) |tt| tok = tt else break;
            }
        } else if (parser.eatToken(.Keyword_catch, skip_nl)) |_| {
            // catch
            const jump = try parser.builder.jumpNotErr(lhs);
            defer parser.builder.finishJump(jump);
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
    fn shiftExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.addExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArrArr, skip_nl) orelse
            parser.eatToken(.RArrArr, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.addExpr(.R, skip_nl)).?;
            return try parser.builder.infix(lhs, tok, rhs);
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
            lhs = try parser.builder.infix(lhs, tok, rhs);
        }
        return lhs;
    }

    /// mul_expr : prefix_expr (("*" | "/" | "//" | "%") prefix_expr.r)*
    fn mulExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!?RegRef {
        var lhs = (try parser.prefixExpr(lr_value, skip_nl)) orelse return null;

        while (parser.eatToken(.Asterisk, skip_nl) orelse
            parser.eatToken(.Slash, skip_nl) orelse
            parser.eatToken(.SlashSlash, skip_nl) orelse
            parser.eatToken(.Percent, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.prefixExpr(.R, skip_nl)).?;
            lhs = try parser.builder.infix(lhs, tok, rhs);
        }

        return lhs;
    }

    /// prefix_expr
    ///     : ("try" | "-" | "+" | "not" | "~") bool_expr.r
    ///     | primary_expr suffix_expr* [.l assign]?
    fn prefixExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!?RegRef {
        if (parser.eatToken(.Keyword_try, skip_nl) orelse
            parser.eatToken(.Minus, skip_nl) orelse
            parser.eatToken(.Plus, skip_nl) orelse
            parser.eatToken(.Tilde, skip_nl) orelse
            parser.eatToken(.Keyword_not, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.boolExpr(.R, skip_nl)).?;
            return try parser.builder.prefix(tok, rhs);
        }
        var primary = try parser.primaryExpr(lr_value, skip_nl);
        primary = try parser.suffixExpr(primary, skip_nl);
        return try parser.assign(lr_value, primary, skip_nl);
    }

    /// suffix_expr
    ///     : "[" bool_expr.r "]"
    ///     | "(" (bool_expr.r ",")* ")"
    ///     | "." IDENTIFIER
    fn suffixExpr(parser: *Parser, lhs: RegRef, skip_nl: bool) anyerror!RegRef {
        while (parser.eatToken(.LBracket, skip_nl) orelse
            parser.eatToken(.LParen, skip_nl) orelse
            parser.eatToken(.Period, skip_nl)) |tok|
        {
            // TODO
            return error.Unimplemented;
            // const rhs = (try parser.boolExpr(.R, true)).?;
            // lhs = try parser.builder.infix(lhs, tok, rhs);
        }

        return lhs;
    }

    /// assign
    ///     : "=" expr.r
    ///     | ("+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") bit_expr.r
    fn assign(parser: *Parser, lr_value: LRValue, lhs: RegRef, skip_nl: bool) anyerror!?RegRef {
        // assignment cannot happen in places where NL is not necessary
        if (parser.eatToken(.Equal, false) orelse
            parser.eatToken(.MinusEqual, false) orelse
            parser.eatToken(.AsteriskEqual, false) orelse
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
                // TODO assignment does not produce value
                return error.ParseError;
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
    ///     | "{" ((IDENTIFIER | STRING) ":" expr.r ",")* "}"
    ///     | "[" (expr.r ",")* "]"
    ///     | "error" "(" expr.r ")"
    ///     | "import" "(" STRING ")"
    ///     | block
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    fn primaryExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!RegRef {
        if (parser.eatToken(.Number, skip_nl) orelse
            parser.eatToken(.String, skip_nl) orelse
            parser.eatToken(.Keyword_true, skip_nl) orelse
            parser.eatToken(.Keyword_false, skip_nl)) |tok|
        {
            return parser.builder.constant(tok);
        }
        if (parser.eatToken(.Identifier, skip_nl)) |tok| {
            return parser.builder.declRef(tok);
        }
        if (parser.eatToken(.Keyword_error, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const val = (try parser.expr(.R, true)).?;
            _ = try parser.expectToken(.RParen, true);
            return parser.builder.buildErr(tok, val);
        }
        if (parser.eatToken(.Keyword_import, skip_nl)) |tok| {
            _ = try parser.expectToken(.LParen, true);
            const str = try parser.builder.constant(try parser.expectToken(.String, true));
            _ = try parser.expectToken(.RParen, true);
            return parser.builder.import(tok, str);
        }
        if (parser.eatToken(.LParen, skip_nl)) |tok| {
            if (parser.eatToken(.Nl, false)) |_| {
                // block
            } else {
                // tuple or grouped expr
            }
        }
        if (parser.eatToken(.LBrace, skip_nl)) |tok| {
            //     | "{" ((IDENTIFIER | STRING) ":" expr.r ",")* "}"
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
        //     | if
        //     | while
        //     | for
        //     | match
        // TODO expected Identifier, String, Number, true, false, '(', '{', '[', error, import, if, while, for, match
        return error.ParseError;
    }

    fn skipNl(parser: *Parser) void {
        _ = parser.eatToken(.Nl, true);
    }

    fn eatToken(parser: *Parser, id: @TagType(Token.Id), skip_nl: bool) ?*Token {
        var next = parser.token_it.next().?;
        if (skip_nl and next.id == .Nl)
            next = parser.token_it.next().?;
        if (next.id == id) {
            return next;
        } else {
            _ = parser.token_it.prev();
            return null;
        }
    }

    fn expectToken(parser: *Parser, id: @TagType(Token.Id), skip_nl: bool) anyerror!*Token {
        if (parser.eatToken(id, skip_nl)) |tok| return tok;
        // TODO err expected token {id} found {parser.token_it.next().?.id}
        return error.ParseError;
    }
};
