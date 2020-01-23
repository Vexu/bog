const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const tokenizer = @import("tokenizer.zig");
const Tokenizer = tokenizer.Tokenizer;
const Token = tokenizer.Token;
const TokenList = tokenizer.TokenList;
const Allocator = mem.Allocator;
const bytecode = @import("bytecode.zig");
const Builder = bytecode.Builder;
const RegRef = Builder.RegRef;

pub const Parser = struct {
    builder: Builder,
    tokenizer: Tokenizer,
    tokens: TokenList,
    token_it: TokenList.Iterator,

    pub fn init(allocator: *Allocator) Parser {
        return .{
            .builder = Builder.init(allocator),
            .tokenizer = .{
                .it = .{
                    .i = 0,
                    .bytes = "",
                },
            },
            .tokens = TokenList.init(allocator),
            .token_it = undefined, // set in `tokenize`
        };
    }

    pub fn deinit(parser: *Parser) void {
        parser.tokens.deinit();
    }

    pub fn parse(parser: *Parser, input: []const u8) !void {
        try parser.tokenize(input);
        try parser.root();
    }

    fn tokenize(parser: *Parser, input: []const u8) !void {
        parser.tokenizer.it.bytes = input;
        const new_len = if (parser.tokens.len != 0) blk: {
            const len = parser.tokens.len - 1; // pop .Eof
            parser.tokens.len = len;
            break :blk len;
        } else 0;
        parser.token_it = parser.tokens.iterator(new_len);
        while (true) {
            const tok = try parser.tokens.addOne();
            tok.* = parser.tokenizer.next();
            if (tok.id == .Eof)
                break;
        }
    }

    const LRValue = enum {
        L,
        R,
    };

    /// root : (stmt NL)* EOF
    fn root(parser: *Parser) !void {
        if (parser.eatToken(.Eof, true)) |_| return;
        while (true) {
            const res = try parser.stmt();
            if (res) |some|
                try parser.builder.discard(some);
            if (parser.eatToken(.Nl, false)) |_| continue;
            _ = try parser.expectToken(.Eof, false);
            return;
        }
    }

    /// stmt : let | expr.l
    fn stmt(parser: *Parser) !?RegRef {
        if (parser.eatToken(.Keyword_let, true)) |_| {
            try parser.let();
            return null;
        } else return parser.expr(.L);
    }

    /// let : let : "let" unwrap "=" expr.r
    fn let(parser: *Parser) anyerror!void {
        unreachable;
    }

    /// expr
    ///     : fn 
    ///     | bool_expr
    ///     | "return" expr.r
    ///     | "break"
    ///     | "continue"
    fn expr(parser: *Parser, lr_value: LRValue) anyerror!?RegRef {
        return if (parser.eatToken(.Keyword_fn, true)) |_|
            try parser.func()
        else
            try parser.boolExpr(lr_value, true);
    }

    /// fn : "fn" "(" (unwrap ",")* ")" expr
    fn func(parser: *Parser) anyerror!RegRef {
        unreachable;
    }

    /// bool_expr : comparision_expr (("or" comparision_expr)* | ("and" comparision_expr)*)
    fn boolExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.comparisionExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        if (parser.eatToken(.Keyword_or, skip_nl)) |_| {
            while (true) {
                const rhs = (try parser.comparisionExpr(lr_value, true)).?;
                lhs = try parser.builder.boolOr(lhs, rhs);
                if (parser.eatToken(.Keyword_or, skip_nl) == null) break;
            }
        } else {
            while (parser.eatToken(.Keyword_and, skip_nl)) |_| {
                const rhs = (try parser.comparisionExpr(lr_value, true)).?;
                lhs = try parser.builder.boolAnd(lhs, rhs);
            }
        }
        return lhs;
    }

    /// comparision_expr : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in"  | "is") range_expr)
    fn comparisionExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.rangeExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArr, skip_nl)) |_| {
            // <
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.lessThan(lhs, rhs);
        } else if (parser.eatToken(.LArrEqual, skip_nl)) |_| {
            // <=
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.lessThanEqual(lhs, rhs);
        } else if (parser.eatToken(.RArr, skip_nl)) |_| {
            // >
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.greaterThan(lhs, rhs);
        } else if (parser.eatToken(.RArrEqual, skip_nl)) |_| {
            // >=
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.greaterThanEqual(lhs, rhs);
        } else if (parser.eatToken(.EqualEqual, skip_nl)) |_| {
            // ==
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.equal(lhs, rhs);
        } else if (parser.eatToken(.BangEqual, skip_nl)) |_| {
            // !=
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.notEqual(lhs, rhs);
        } else if (parser.eatToken(.Keyword_in, skip_nl)) |_| {
            // in
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.in(lhs, rhs);
        } else if (parser.eatToken(.Keyword_is, skip_nl)) |_| {
            // is
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.is(lhs, rhs);
        }
        return lhs;
    }

    /// range_expr : bit_expr ("..." bit_expr)?
    fn rangeExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.bitExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Ellipsis, skip_nl)) |_| {
            const rhs = (try parser.bitExpr(lr_value, true)).?;
            lhs = try parser.builder.range(lhs, rhs);
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr)* | ("|" shift_expr)* | ("|" shift_expr)*) | ("catch" ("|" unwrap "|")? expr)
    fn bitExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.shiftExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        if (parser.eatToken(.Ampersand, skip_nl)) |_| {
            // &
            while (true) {
                const rhs = (try parser.shiftExpr(lr_value, true)).?;
                lhs = try parser.builder.bitAnd(lhs, rhs);
                if (parser.eatToken(.Ampersand, skip_nl) == null) break;
            }
        } else if (parser.eatToken(.Pipe, skip_nl)) |_| {
            // |
            while (true) {
                const rhs = (try parser.shiftExpr(lr_value, true)).?;
                lhs = try parser.builder.bitOr(lhs, rhs);
                if (parser.eatToken(.Pipe, skip_nl) == null) break;
            }
        } else if (parser.eatToken(.Caret, skip_nl)) |_| {
            // ^
            while (true) {
                const rhs = (try parser.shiftExpr(lr_value, true)).?;
                lhs = try parser.builder.bitXor(lhs, rhs);
                if (parser.eatToken(.Caret, skip_nl) == null) break;
            }
        } else if (parser.eatToken(.RArrEqual, skip_nl)) |_| {
            // catch
            const is_err = try parser.builder.isErr(lhs);
            const jump = try parser.builder.jumpFalse(is_err);
            if (parser.eatToken(.Pipe, true)) |_| {
                @panic("TODO");
                // const unwrap = try parser.unwrap();
                // lhs = try parser.builder.unwrap(lhs, unwrap);
                // _ = try parser.expectToken(.Pipe, true);
            }
            const rhs = (try parser.expr(lr_value)).?;
            try parser.builder.move(rhs, lhs);
            parser.builder.finishJump(jump);
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr)
    fn shiftExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.addExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArrArr, skip_nl)) |_| {
            // <<
            const rhs = (try parser.addExpr(lr_value, true)).?;
            lhs = try parser.builder.leftShift(lhs, rhs);
        } else if (parser.eatToken(.RArrArr, skip_nl)) |_| {
            // >>
            const rhs = (try parser.addExpr(lr_value, true)).?;
            lhs = try parser.builder.rightShift(lhs, rhs);
        }
        return lhs;
    }

    /// add_expr : mul_expr (("-" | "+") mul_expr)*
    fn addExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?RegRef {
        var lhs = (try parser.mulExpr(lr_value, skip_nl)) orelse return null;

        while (true) {
            if (parser.eatToken(.Minus, skip_nl)) |_| {
                // -
                const rhs = (try parser.mulExpr(lr_value, true)).?;
                lhs = try parser.builder.sub(lhs, rhs);
            } else if (parser.eatToken(.Plus, skip_nl)) |_| {
                // +
                const rhs = (try parser.mulExpr(lr_value, true)).?;
                lhs = try parser.builder.add(lhs, rhs);
            } else break;
        }
        return lhs;
    }

    /// mul_expr : prefix_expr (("*" | "/" | "//" | "%") prefix_expr)*
    fn mulExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!?RegRef {
        var lhs = (try parser.prefixExpr(lr_value, skip_nl)) orelse return null;

        while (true) {
            if (parser.eatToken(.Asterisk, skip_nl)) |_| {
                // *
                const rhs = (try parser.prefixExpr(lr_value, true)).?;
                lhs = try parser.builder.mul(lhs, rhs);
            } else if (parser.eatToken(.Slash, skip_nl)) |_| {
                // /
                const rhs = (try parser.prefixExpr(lr_value, true)).?;
                lhs = try parser.builder.div(lhs, rhs);
            } else if (parser.eatToken(.SlashSlash, skip_nl)) |_| {
                // //
                const rhs = (try parser.prefixExpr(lr_value, true)).?;
                lhs = try parser.builder.divFloor(lhs, rhs);
            } else if (parser.eatToken(.Percent, skip_nl)) |_| {
                // %
                const rhs = (try parser.prefixExpr(lr_value, true)).?;
                lhs = try parser.builder.mod(lhs, rhs);
            } else break;
        }

        return lhs;
    }

    /// prefix_expr 
    ///     : "try" bool_expr.r
    ///     | ("-" | "+" | "not" | "~")? primary_expr suffix_expr* [.l assign]?
    fn prefixExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!?RegRef {
        if (parser.eatToken(.Keyword_try, skip_nl)) |_| {
            const rhs = (try parser.boolExpr(.R, true)).?;
            return try parser.builder.tryExpr(rhs);
        }
        const prefix_op = parser.eatToken(.Minus, skip_nl) orelse parser.eatToken(.Plus, skip_nl) orelse
            parser.eatToken(.Tilde, skip_nl) orelse parser.eatToken(.Keyword_not, skip_nl);
        var primary = try parser.primaryExpr(lr_value, skip_nl);
        // while (try parser.suffixExpr(skip_nl, primary)) |suffix| {
        //     primary = suffix;
        // }
        if (prefix_op) |some| {
            primary = switch (some.id) {
                .Plus => primary, // unary plus is no op
                .Minus => try parser.builder.negate(primary),
                .Tilde => try parser.builder.bitNot(primary),
                .Keyword_not => try parser.builder.boolNot(primary),
                else => unreachable,
            };
        } else if (lr_value == .L) {
            // try parser.assign(skip_nl);
        }
        return primary;
    }

    /// assign
    ///     : "=" expr.r
    ///     | ("+=" | "-=" | "*=" | "/=" | "//=" | "%=" | "<<=" | ">>=" | "&=" | "|=" | "^=") bit_expr.r
    fn assign(parser: *Parser, skip_nl: bool) anyerror!?RegRef {
        unreachable;
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
    ///     | "error" expr.r
    ///     | "import" STRING
    ///     | block
    ///     | if
    ///     | while
    ///     | for
    ///     | match
    fn primaryExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) anyerror!RegRef {
        if (parser.eatToken(.Number, skip_nl)) |tok| {
            return parser.builder.constNumber(tok.id.Number);
        }
        unreachable;
    }

    fn eatToken(parser: *Parser, id: @TagType(Token.Id), skip_nl: bool) ?*Token {
        var next = parser.token_it.next().?;
        if (skip_nl) {
            while (next.id == .Nl) {
                next = parser.token_it.next().?;
            }
        }
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
