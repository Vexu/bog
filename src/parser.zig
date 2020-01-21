const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const tokenizer = @import("tokenizer.zig");
const Tokenizer = tokenizer.Tokenizer;
const Token = tokenizer.Token;
const TokenList = tokenizer.TokenList;
const bytecode = @import("bytecode.zig");

pub const Parser = struct {
    // err_stream: std.io.OutStream(std.fs.File.WriteError),
    // builder: bytecode.Builder,
    tokenizer: Tokenizer,
    it: Tokeniter,

    fn parse(parser: *Parser, input: []const u8) !void {
        
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
