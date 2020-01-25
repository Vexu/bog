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
const TypeId = @import("value.zig").TypeId;

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
        }
    }
};

pub const ErrorList = std.SegmentedList(Error, 0);

pub const Parser = struct {
    builder: *Builder,
    token_it: *TokenList.Iterator,
    errors: ErrorList,

    pub const ParseError = error{
        ParseError,

        // TODO remove possibility of these
        Unimplemented,
    } || Allocator.Error;

    pub fn init(allocator: *Allocator, builder: *Builder) Parser {
        return .{
            .builder = builder,
            .errors = ErrorList.init(allocator),
            .token_it = undefined, // set in `parse`
        };
    }

    pub fn deinit(parser: *Parser) void {
        parser.errors.deinit();
    }

    pub fn parse(parser: *Parser, tokens: *TokenList.Iterator) ParseError!void {
        parser.token_it = tokens;
        try parser.root();
    }

    const LRValue = enum {
        L,
        R,
    };

    /// root : (stmt NL)* EOF
    fn root(parser: *Parser) ParseError!void {
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
    fn stmt(parser: *Parser) ParseError!?RegRef {
        return if (try parser.let())
            null
        else
            try parser.expr(.L, false);
    }

    /// let : "let" unwrap "=" expr.r
    fn let(parser: *Parser) ParseError!bool {
        const tok = parser.eatToken(.Keyword_let, false) orelse return false;
        return error.Unimplemented;
    }

    /// expr
    ///     : fn
    ///     | [.l jump_expr]
    ///     | bool_expr
    fn expr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        return if (try parser.func(skip_nl)) |val|
            val
        else if (try parser.jumpExpr(lr_value))
            null
        else
            try parser.boolExpr(lr_value, skip_nl);
    }

    // TODO const expr folding
    const ExprRes = union(enum) {
        None,
        Rt: RegRef,
        Int: i64,
        Num: f64,
    };

    /// fn : "fn" "(" (unwrap ",")* ")" expr
    fn func(parser: *Parser, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_fn, skip_nl) orelse return null;
        return error.Unimplemented;
        // try parser.builder.beginFunc(tok);
        // try parser.expectToken(.LParen, true);
        // try parser.expectToken(.LParen, true);
        // const res = try parser.expr(.R true);
        // return try parser.builder.endFunc(tok, res);
    }

    /// jump_expr : "return" expr.r | "break" | "continue"
    fn jumpExpr(parser: *Parser, lr_value: LRValue) ParseError!bool {
        const tok = parser.eatToken(.Keyword_return, false) orelse
            parser.eatToken(.Keyword_break, false) orelse
            parser.eatToken(.Keyword_continue, false) orelse
            return false;
        if (lr_value != .L) {
            return parser.reportErr(.JumpRValue, tok);
        }
        return error.Unimplemented;
        // return true;
    }

    /// bool_expr
    ///     : "not" comparision_expr.r
    ///     | comparision_expr ("or" comparision_expr.r)*
    ///     | comparision_expr ("and" comparision_expr.r)*
    fn boolExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        if (parser.eatToken(.Keyword_not, skip_nl)) |tok| {
            parser.skipNl();
            const rhs = (try parser.comparisionExpr(.R, skip_nl)).?;
            return parser.builder.prefix(tok, rhs) catch |e| switch (e) {
                error.StackOverflow => return parser.reportErr(.StackOverflow, tok),
                else => |err| return err,
            };
        }
        var lhs = (try parser.comparisionExpr(lr_value, skip_nl)) orelse return null;

        // TODO improve
        var jump_count: u32 = 0;
        if (parser.eatToken(.Keyword_or, skip_nl)) |t| {
            var tok = t;
            while (true) {
                try parser.builder.jumpFalse(lhs);
                jump_count += 1;
                parser.skipNl();
                const rhs = (try parser.comparisionExpr(.R, skip_nl)).?;
                lhs = try parser.infix(lhs, tok, rhs);
                if (parser.eatToken(.Keyword_or, skip_nl)) |tt| tok = tt else break;
            }
        } else {
            while (parser.eatToken(.Keyword_and, skip_nl)) |tok| {
                try parser.builder.jumpFalse(lhs);
                jump_count += 1;
                parser.skipNl();
                const rhs = (try parser.comparisionExpr(.R, skip_nl)).?;
                lhs = try parser.infix(lhs, tok, rhs);
            }
        }
        parser.builder.finishJumps(jump_count);
        return lhs;
    }

    /// comparision_expr
    ///     : range_expr (("<" | "<=" | ">" | ">="| "==" | "!=" | "in") range_expr.r)?
    ///     | range_expr ("is" type_name)?
    fn comparisionExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.rangeExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.LArr, skip_nl) orelse
            parser.eatToken(.LArrEqual, skip_nl) orelse
            parser.eatToken(.RArr, skip_nl) orelse
            parser.eatToken(.RArrEqual, skip_nl) orelse
            parser.eatToken(.EqualEqual, skip_nl) orelse
            parser.eatToken(.BangEqual, skip_nl) orelse
            parser.eatToken(.Keyword_in, skip_nl)) |tok|
        {
            parser.skipNl();
            const rhs = (try parser.rangeExpr(.R, skip_nl)).?;
            lhs = try parser.infix(lhs, tok, rhs);
        } else if (parser.eatToken(.Keyword_is, skip_nl)) |tok| {
            const id = try parser.typeName();
            lhs = try parser.builder.isType(lhs, tok, id);
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
            return parser.reportErr(.TypeName, parser.token_it.peek().?);
    }

    /// range_expr : bit_expr ("..." bit_expr.r)?
    fn rangeExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        var lhs = (try parser.bitExpr(lr_value, skip_nl)) orelse return null;

        if (parser.eatToken(.Ellipsis, skip_nl)) |tok| {
            parser.skipNl();
            const rhs = (try parser.bitExpr(.R, skip_nl)).?;
            lhs = try parser.infix(lhs, tok, rhs);
        }
        return lhs;
    }

    /// bit_expr : shift_expr (("&" shift_expr.r)* | ("|" shift_expr.r)* | ("|" shift_expr.r)*) | ("catch" ("|" unwrap "|")? expr)
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
    ///     | "{" ((IDENTIFIER | STRING) ":" expr.r ",")* "}"
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
        return parser.reportErr(.PrimaryExpr, parser.token_it.peek().?);
    }

    /// block : "(" (NL stmt)+ ")"
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

    /// if : "if" "(" bool_expr.r ")" expr ("else" "if" "(" bool_expr.r ")" expr)* ("else" expr)?
    fn ifExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_if, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    /// while : "while" "(" bool_expr.r ")" expr
    fn whileExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_while, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    /// for : "for" "(" unwrap "in" range_expr.r ")" expr
    fn forExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_for, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    /// match : "match" "(" bool_expr.r ")" "(" (NL match_case ",")+ ")"
    fn matchExpr(parser: *Parser, lr_value: LRValue, skip_nl: bool) ParseError!?RegRef {
        const tok = parser.eatToken(.Keyword_match, skip_nl) orelse return null;
        return error.Unimplemented;
    }

    fn infix(parser: *Parser, lhs: RegRef, tok: *Token, rhs: RegRef) ParseError!RegRef {
        return parser.builder.infix(lhs, tok, rhs) catch |e| switch (e) {
            error.StackOverflow => return parser.reportErr(.StackOverflow, tok),
            else => |err| return err,
        };
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

    fn expectToken(parser: *Parser, id: @TagType(Token.Id), skip_nl: bool) !*Token {
        if (parser.eatToken(id, skip_nl)) |tok| return tok;
        return parser.reportErr(.UnexpectedToken, parser.token_it.peek().?);
    }
};
