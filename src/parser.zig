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

    ///root <- (stmt NL)* EOF
    fn root(parser: *Parser) !void {

    }

    ///stmt
    ///   <- if
    ///    / while
    ///    / for
    ///    / match
    ///    / let
    ///    / expr
    ///    / assign
    ///    / "return" expr
    ///    / "break"
    ///    / "continue"
    fn stmt(parser: *Parser) !void {
        if (parser.eatToken(.Keyword_if)) |_| {

        } else if (parser.eatToken(.Keyword_if)) |_| {

        } else if (parser.eatToken(.Keyword_while)) |_| {

        } else if (parser.eatToken(.Keyword_for)) |_| {

        } else if (parser.eatToken(.Keyword_match)) |_| {

        } else if (parser.eatToken(.Keyword_let)) |_| {

        } else if (parser.eatToken(.Keyword_return)) |_| {

        } else if (parser.eatToken(.Keyword_break)) |_| {

        } else if (parser.eatToken(.Keyword_continue)) |_| {

        } 
        // else if (parser.eatToken(.Identifier)) |tok| {
        //     if (parser.isAssign()) {

        //     } else {
        //         it.index -= 1;
        //     }
        // }
        const res = try parser.expr();
        try parser.builder.discard(res);
    }

    /// expr <- (fn / import / bool_expr)
    fn expr(parser: *Parser) !u8 {

    }

    /// fn <- "|" (unwrap ",")* "|" (block / bool_expr)
    fn func(parser: *Parser) !u8 {}

    /// import <- "import" STRING

    /// bool_expr <- comparision_expr (("or" comparision_expr)* / ("and" comparision_expr)*)

    fn block(parser: *Parser) !void {
        const indent = try parser.getIndent();
    }

    // fn isAssign(parser: *Parser) bool {
    //     while (parser.eatToken())
    // }

    fn eatToken(parser: *Parser, id: Token.id) ?*Token {
    }

    fn expectToken(parser: *Parser, id: Token.id) ?*Token {
    }
};
