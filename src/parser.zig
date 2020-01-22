const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const tokenizer = @import("tokenizer.zig");
const Tokenizer = tokenizer.Tokenizer;
const Token = tokenizer.Token;
const TokenList = tokenizer.TokenList;
const bytecode = @import("bytecode.zig");
const Allocator = std.mem.Allocator;

pub const Parser = struct {
    // err_stream: std.io.OutStream(std.fs.File.WriteError),
    // builder: bytecode.Builder,
    tokenizer: Tokenizer,
    tokens: TokenList,
    token_it: TokenList.Iterator,
    // eof_callback: ?fn () error{OutOfMemory}![]const u8 = null,

    pub fn init(allocator: *Allocator) Parser {
        return .{
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
        while (true) {
            if (parser.eatToken(.Eof, true)) |_| break;
            try builder.discard(try parser.stmt());
            if (parser.eatToken(.Nl, false)) |_| continue;
        }
    }

    /// stmt : let | expr.l
    fn stmt(parser: *Parser) !?u8 {
        if (parser.eatToken(.Keyword_let, true)) |_| {
            try parser.let();
            return null;
        } else return parser.expr(.L);
    }

    /// let : let : "let" unwrap "=" expr.r
    fn let(parser: *Parser) !void {}

    /// expr : fn | bool_expr
    fn expr(parser: *Parser, lr_value: LRValue) !?u8 {
        return if (parser.eatToken(.Keyword_fn, true)) |_|
            try parser.func()
        else
            try parser.boolExpr(lr_value, false);
    }

    /// fn : "fn" "(" (unwrap ",")* ")" expr
    fn func(parser: *Parser) !u8 {}

    /// bool_expr : comparision_expr (("or" comparision_expr)* | ("and" comparision_expr)*)
    fn boolExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?u8 {
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
    fn comparisionExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?u8 {
        var lhs = (try parser.rangeExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArr, skip_nl)) |_| {
            // <
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.lessThan(lhs, rhs);
        } else if (parser.eatToken(.LArrEqual, skip_nl)) |_| {
            // <=
            const rhs = (try parser.rangeExpr(lr_value, true)).?;
            lhs = try parser.builder.lessThanEqual(lhs, rhs);
        } else if (parser.eatToken(.RArrr, skip_nl)) |_| {
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
    fn rangeExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?u8 {
        var lhs = (try parser.bitExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Ellipsis, skip_nl)) |_| {
            const rhs = (try parser.bitExpr(lr_value, true)).?;
            lhs = try parser.builder.range(lhs, rhs);
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr)* | ("|" shift_expr)* | ("|" shift_expr)*) | ("catch" ("|" unwrap "|")? expr)
    fn bitExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?u8 {
        var lhs = (try parser.shiftExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Ampersand, skip_nl)) |_| {
            // &
            const rhs = (try parser.shiftExpr(lr_value, true)).?;
            lhs = try parser.builder.bitAnd(lhs, rhs);
        } else if (parser.eatToken(.LArrEqual, skip_nl)) |_| {
            // |
            const rhs = (try parser.shiftExpr(lr_value, true)).?;
            lhs = try parser.builder.bitOr(lhs, rhs);
        } else if (parser.eatToken(.RArrr, skip_nl)) |_| {
            // ^
            const rhs = (try parser.shiftExpr(lr_value, true)).?;
            lhs = try parser.builder.bitXor(lhs, rhs);
        } else if (parser.eatToken(.RArrEqual, skip_nl)) |_| {
            // catch
            const is_err = parser.builder.isErr(lhs);
            const jump = parser.builder.jumpFalse(is_err);
            if (parser.eatToken(.Pipe, true)) {
                @panic("TODO");
                // const unwrap = try parser.unwrap();
                // lhs = try parser.builder.unwrap(lhs, unwrap);
                // _ = try parser.expectToken(.Pipe, true);
            }
            const rhs = (try parser.expr(lr_value, true)).?;
            parser.builder.move(lhs, rhs);
            parser.builder.finishJump(jump);
        }
        return lhs;
    }

    /// shift_expr : add_expr (("<<" | ">>") add_expr)
    fn shiftExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?u8 {
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
    fn addExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) !?u8 {
        var lhs = (try parser.mulExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        if (parser.eatToken(.Minus, skip_nl)) |_| {
            // -
            while (true) {
                const rhs = (try parser.mulExpr(lr_value, true)).?;
                lhs = try parser.builder.sub(lhs, rhs);
                if (parser.eatToken(.Minus, skip_nl) == null) break;
            }
        } else {
            // +
            while (parser.eatToken(.Plus, skip_nl)) |_| {
                const rhs = (try parser.mulExpr(lr_value, true)).?;
                lhs = try parser.builder.add(lhs, rhs);
            }
        }
        return lhs;
    }

    fn eatToken(parser: *Parser, id: Token.id, skip_nl: bool) ?*Token {}

    fn expectToken(parser: *Parser, id: Token.id, skip_nl: bool) ?*Token {}
};
