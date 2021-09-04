const std = @import("std");
const mem = std.mem;
const math = std.math;
const testing = std.testing;
const unicode = std.unicode;
const bog = @import("bog.zig");
const Errors = bog.Errors;

fn isWhiteSpace(c: u32) bool {
    return switch (c) {
        ' ',
        '\t',
        '\r',
        // NO-BREAK SPACE
        0x00A0,
        // OGHAM SPACE MARK
        0x1680,
        // MONGOLIAN VOWEL SEPARATOR
        0x180E,
        // EN QUAD
        0x2000,
        // EM QUAD
        0x2001,
        // EN SPACE
        0x2002,
        // EM SPACE
        0x2003,
        // THREE-PER-EM SPACE
        0x2004,
        // FOUR-PER-EM SPACE
        0x2005,
        // SIX-PER-EM SPACE
        0x2006,
        // FIGURE SPACE
        0x2007,
        // PUNCTUATION SPACE
        0x2008,
        // THIN SPACE
        0x2009,
        // HAIR SPACE
        0x200A,
        // ZERO WIDTH SPACE
        0x200B,
        // NARROW NO-BREAK SPACE
        0x202F,
        // MEDIUM MATHEMATICAL SPACE
        0x205F,
        // IDEOGRAPHIC SPACE
        0x3000,
        // ZERO WIDTH NO-BREAK SPACE
        0xFEFF,
        // HALFWIDTH HANGUL FILLER
        0xFFA0,
        => true,
        else => false,
    };
}

fn isIdentifier(c: u32) bool {
    return switch (c) {
        'a'...'z',
        'A'...'Z',
        '_',
        '0'...'9',
        // unicode identifiers
        0x00A8,
        0x00AA,
        0x00AD,
        0x00AF,
        0x00B2...0x00B5,
        0x00B7...0x00BA,
        0x00BC...0x00BE,
        0x00C0...0x00D6,
        0x00D8...0x00F6,
        0x00F8...0x167F,
        0x1681...0x180D,
        0x180F...0x1FFF,
        0x200B...0x200D,
        0x202A...0x202E,
        0x203F...0x2040,
        0x2054,
        0x2060...0x218F,
        0x2460...0x24FF,
        0x2776...0x2793,
        0x2C00...0x2DFF,
        0x2E80...0x2FFF,
        0x3004...0x3007,
        0x3021...0x302F,
        0x3031...0xD7FF,
        0xF900...0xFD3D,
        0xFD40...0xFDCF,
        0xFDF0...0xFE44,
        0xFE47...0xFFFD,
        0x10000...0x1FFFD,
        0x20000...0x2FFFD,
        0x30000...0x3FFFD,
        0x40000...0x4FFFD,
        0x50000...0x5FFFD,
        0x60000...0x6FFFD,
        0x70000...0x7FFFD,
        0x80000...0x8FFFD,
        0x90000...0x9FFFD,
        0xA0000...0xAFFFD,
        0xB0000...0xBFFFD,
        0xC0000...0xCFFFD,
        0xD0000...0xDFFFD,
        0xE0000...0xEFFFD,
        => true,
        else => false,
    };
}

pub const Token = struct {
    start: u32,
    end: u32,
    id: Id,

    pub const List = std.ArrayList(Token);
    pub const Index = u32;

    pub const Id = union(enum) {
        Eof,
        Indent: u16,
        Comment,
        Identifier,
        String,
        Integer,
        Number,
        Nl,
        Pipe,
        PipeEqual,
        Equal,
        EqualEqual,
        BangEqual,
        LParen,
        RParen,
        Percent,
        PercentEqual,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        Period,
        EqualRarr,
        Caret,
        CaretEqual,
        Plus,
        PlusEqual,
        PlusPlus,
        Minus,
        MinusEqual,
        Asterisk,
        AsteriskEqual,
        AsteriskAsterisk,
        AsteriskAsteriskEqual,
        Slash,
        SlashEqual,
        SlashSlash,
        SlashSlashEqual,
        Comma,
        Ampersand,
        AmpersandEqual,
        LArr,
        LArrEqual,
        LArrArr,
        LArrArrEqual,
        RArr,
        RArrEqual,
        RArrArr,
        RArrArrEqual,
        Tilde,
        Colon,
        Underscore,
        At,
        FormatStart,
        Format,
        FormatEnd,

        /// keywords
        Keyword_not,
        Keyword_and,
        Keyword_or,
        Keyword_let,
        Keyword_continue,
        Keyword_break,
        Keyword_return,
        Keyword_if,
        Keyword_else,
        Keyword_false,
        Keyword_true,
        Keyword_for,
        Keyword_while,
        Keyword_match,
        Keyword_catch,
        Keyword_try,
        Keyword_error,
        Keyword_import,
        Keyword_is,
        Keyword_in,
        Keyword_fn,
        Keyword_as,
        Keyword_const,
        Keyword_this,
    };

    pub const keywords = std.ComptimeStringMap(Id, .{
        .{ "not", .Keyword_not },
        .{ "and", .Keyword_and },
        .{ "or", .Keyword_or },
        .{ "let", .Keyword_let },
        .{ "continue", .Keyword_continue },
        .{ "break", .Keyword_break },
        .{ "return", .Keyword_return },
        .{ "if", .Keyword_if },
        .{ "else", .Keyword_else },
        .{ "false", .Keyword_false },
        .{ "true", .Keyword_true },
        .{ "for", .Keyword_for },
        .{ "while", .Keyword_while },
        .{ "match", .Keyword_match },
        .{ "catch", .Keyword_catch },
        .{ "try", .Keyword_try },
        .{ "error", .Keyword_error },
        .{ "import", .Keyword_import },
        .{ "is", .Keyword_is },
        .{ "in", .Keyword_in },
        .{ "fn", .Keyword_fn },
        .{ "as", .Keyword_as },
        .{ "const", .Keyword_const },
        .{ "this", .Keyword_this },
        .{ "_", .Underscore },
    });

    pub fn string(id: std.meta.Tag(Id)) []const u8 {
        return switch (id) {
            .Comment => "<Comment>",
            .Eof => "<EOF>",
            .Nl => "<NL>",
            .Indent => "<INDENT>",
            .Identifier => "Identifier",
            .String => "String",
            .Integer => "Integer",
            .Number => "Number",
            .Pipe => "|",
            .PipeEqual => "|=",
            .Equal => "=",
            .EqualEqual => "==",
            .BangEqual => "!=",
            .LParen => "(",
            .RParen => ")",
            .Percent => "%",
            .PercentEqual => "%=",
            .LBrace => "{",
            .RBrace => "}",
            .LBracket => "[",
            .RBracket => "]",
            .Period => ".",
            .EqualRarr => "=>",
            .Caret => "^",
            .CaretEqual => "^=",
            .Plus => "+",
            .PlusEqual => "+=",
            .PlusPlus => "++",
            .Minus => "-",
            .MinusEqual => "-=",
            .Asterisk => "*",
            .AsteriskEqual => "*=",
            .AsteriskAsterisk => "**",
            .AsteriskAsteriskEqual => "**=",
            .Slash => "/",
            .SlashEqual => "/=",
            .SlashSlash => "//",
            .SlashSlashEqual => "//=",
            .Comma => ",",
            .Ampersand => "&",
            .AmpersandEqual => "&=",
            .LArr => "<",
            .LArrEqual => "<=",
            .LArrArr => "<<",
            .LArrArrEqual => "<<=",
            .RArr => ">",
            .RArrEqual => ">=",
            .RArrArr => ">>",
            .RArrArrEqual => ">>=",
            .Tilde => "~",
            .Colon => ":",
            .Underscore => "_",
            .At => "@",
            .FormatStart, .Format, .FormatEnd => "Format string",

            .Keyword_not => "not",
            .Keyword_and => "and",
            .Keyword_or => "or",
            .Keyword_let => "let",
            .Keyword_continue => "continue",
            .Keyword_break => "break",
            .Keyword_return => "return",
            .Keyword_if => "if",
            .Keyword_else => "else",
            .Keyword_false => "false",
            .Keyword_true => "true",
            .Keyword_for => "for",
            .Keyword_while => "while",
            .Keyword_match => "match",
            .Keyword_catch => "catch",
            .Keyword_try => "try",
            .Keyword_error => "error",
            .Keyword_import => "import",
            .Keyword_is => "is",
            .Keyword_in => "in",
            .Keyword_fn => "fn",
            .Keyword_as => "as",
            .Keyword_const => "const",
            .Keyword_this => "this",
        };
    }
};

pub fn tokenize(allocator: *mem.Allocator, source: []const u8, errors: *Errors) Tokenizer.Error![]const Token {
    // estimate one token per 8 bytes to reduce allocation in the beginning
    const estimated = source.len / 8;
    var tokenizer = Tokenizer{
        .tokens = try Token.List.initCapacity(allocator, estimated),
        .errors = errors,
        .it = .{
            .i = 0,
            .bytes = source,
        },
        .repl = false,
    };
    errdefer tokenizer.tokens.deinit();
    while (true) {
        const tok = try tokenizer.tokens.addOne();
        tok.* = try tokenizer.next();
        if (tok.id == .Eof) {
            // std.debug.warn("estimated: {}, actual: {}\n\n", .{estimated, tokenizer.tokens.items.len});
            return tokenizer.tokens.toOwnedSlice();
        }
    }
}

pub fn tokenizeRepl(repl: *@import("repl.zig").Repl) Tokenizer.Error!bool {
    // remove previous eof
    const self = &repl.tokenizer;
    self.it.bytes = repl.buffer.items;
    self.errors = &repl.vm.errors;

    if (self.tokens.items.len > 0) _ = self.tokens.pop();
    const start_len = self.tokens.items.len;

    while (true) {
        const tok = try self.tokens.addOne();
        tok.* = try self.next();
        if (tok.id == .Eof) {
            // check if more input is expected
            return if (self.tokens.items.len == start_len + 2)
                true
            else if (self.paren_level != 0 or
                self.string or
                self.expect_indent or
                self.format_string != 0 or
                self.indent_level != 0)
                false
            else
                true;
        }
    }
}

pub const Tokenizer = struct {
    errors: *Errors,
    tokens: Token.List,
    it: unicode.Utf8Iterator,

    /// indentation specific variables
    indent_char: ?u32 = null,

    /// level of parentheses
    paren_level: u16 = 0,

    /// level of parentheses at the start of the format string
    format_paren_level: u16 = 0,

    /// how many of `indent_char` are in one indentation level
    chars_per_indent: ?u8 = null,
    indent_level: u16 = 0,

    /// format string delimiter, 0 if not in a format string
    format_string: u8 = 0,

    /// saw a nl, need to check for indentation
    expect_indent: bool = false,

    /// currently in a multiline string
    string: bool = false,
    repl: bool,

    pub const Error = error{TokenizeError} || mem.Allocator.Error;

    fn reportErr(self: *Tokenizer, msg: []const u8, c: u21) Error {
        try self.errors.add(
            .{ .data = msg },
            @truncate(u32, self.it.i - (unicode.utf8CodepointSequenceLength(c) catch unreachable)),
            .err,
        );
        self.it.i = self.it.bytes.len;
        return error.TokenizeError;
    }

    fn getIndent(self: *Tokenizer) !?Token {
        var start_index = self.it.i;
        var count: u8 = 0;
        // get all indentation characters
        while (self.it.nextCodepoint()) |c| switch (c) {
            '\r' => continue,
            '\n', ';' => {
                // empty line; rest count
                count = 0;
                if (self.repl) break;
            },
            '#' => {
                self.it.i -= 1;
                return null;
            },
            else => if (self.indent_char != null and c == self.indent_char.?) {
                count += 1;
            } else if (isWhiteSpace(c)) {
                self.indent_char = c;
                count += 1;
            } else {
                self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                break;
            },
        } else {
            if (self.repl) {
                if (self.indent_level == 0 and self.tokens.items.len > 2) {
                    switch (self.tokens.items[self.tokens.items.len - 3].id) {
                        // no further input is expected after these tokens
                        // so we can stop asking for more input
                        .Comment,
                        .Identifier,
                        .Keyword_this,
                        .String,
                        .Integer,
                        .Number,
                        .RBrace,
                        .RBracket,
                        .Underscore,
                        .Keyword_break,
                        .Keyword_continue,
                        .Keyword_false,
                        .Keyword_true,
                        => return null,
                        else => {},
                    }
                }
                self.expect_indent = true;
                return null;
            }
            // EOF level goes to zero
            count = 0;
        }
        if (count == 0) {
            // back to level zero, close all blocks
            self.indent_char = null;
            self.chars_per_indent = null;
            return null;
        }

        errdefer if (self.repl) {
            // reset indentation in case of error
            self.indent_char = null;
            self.chars_per_indent = null;
        };
        if (self.chars_per_indent) |some| {
            if (count % some != 0) {
                // inconsistent amount of `ìndent_char`s per level
                return self.reportErr("inconsistent indentation", 'a');
            }
        } else {
            self.chars_per_indent = count;
        }
        const level = @divExact(count, self.chars_per_indent.?);

        if (level > 50) {
            return self.reportErr("indentation exceeds maximum of 50 levels", 'a');
        }

        // needed by the repl tokenizer
        self.indent_level = level;
        return Token{
            .id = .{ .Indent = level },
            .start = @truncate(u32, start_index),
            .end = @truncate(u32, self.it.i),
        };
    }

    fn next(self: *Tokenizer) !Token {
        // get indent
        if (self.expect_indent) {
            self.expect_indent = false;
            if (try self.getIndent()) |some|
                return some;
        }
        var start_index = self.it.i;
        var state: enum {
            Start,
            String,
            EscapeSequence,
            HexEscape,
            UnicodeStart,
            UnicodeEscape,
            UnicodeEnd,
            Identifier,
            Equal,
            Bang,
            Pipe,
            Percent,
            Asterisk,
            AsteriskAsterisk,
            Plus,
            LArr,
            LArrArr,
            RArr,
            RArrArr,
            Caret,
            Minus,
            Slash,
            SlashSlash,
            Ampersand,
            LineComment,
            BinaryNumber,
            OctalNumber,
            HexNumber,
            Number,
            Zero,
            NumberDot,
            FloatFraction,
            FloatExponent,
            FloatExponentDigits,
            f,
            FormatString,
            FormatBrace,
        } = .Start;
        var res: Token.Id = .Eof;
        var str_delimit: u32 = undefined;
        var counter: u32 = 0;
        var dot_index: ?usize = null;
        var escape_end_state: @TypeOf(state) = .String;

        while (self.it.nextCodepoint()) |c| {
            switch (state) {
                .Start => switch (c) {
                    '#' => {
                        state = .LineComment;
                    },
                    '\n', ';' => {
                        res = .Nl;
                        self.expect_indent = true;
                        break;
                    },
                    '"', '\'' => {
                        self.string = true;
                        str_delimit = c;
                        state = .String;
                    },
                    '=' => {
                        state = .Equal;
                    },
                    '!' => {
                        state = .Bang;
                    },
                    '|' => {
                        state = .Pipe;
                    },
                    '(' => {
                        self.paren_level += 1;
                        res = .LParen;
                        break;
                    },
                    ')' => {
                        if (self.paren_level == 0) {
                            return self.reportErr("unmatched ')'", c);
                        }
                        self.paren_level -= 1;
                        res = .RParen;
                        break;
                    },
                    '[' => {
                        self.paren_level += 1;
                        res = .LBracket;
                        break;
                    },
                    ']' => {
                        if (self.paren_level == 0) {
                            return self.reportErr("unmatched ']'", c);
                        }
                        self.paren_level -= 1;
                        res = .RBracket;
                        break;
                    },
                    ',' => {
                        res = .Comma;
                        break;
                    },
                    '@' => {
                        res = .At;
                        break;
                    },
                    '%' => {
                        state = .Percent;
                    },
                    '*' => {
                        state = .Asterisk;
                    },
                    '+' => {
                        state = .Plus;
                    },
                    '<' => {
                        state = .LArr;
                    },
                    '>' => {
                        state = .RArr;
                    },
                    '^' => {
                        state = .Caret;
                    },
                    '{' => {
                        self.paren_level += 1;
                        res = .LBrace;
                        break;
                    },
                    '}' => {
                        if (self.format_string != 0 and self.format_paren_level == self.paren_level) {
                            escape_end_state = .FormatString;
                            state = .FormatString;
                            res = .Format;
                            continue;
                        }
                        if (self.paren_level == 0) {
                            return self.reportErr("unmatched '}'", c);
                        }
                        self.paren_level -= 1;
                        res = .RBrace;
                        break;
                    },
                    '~' => {
                        res = .Tilde;
                        break;
                    },
                    ':' => {
                        if (self.format_string != 0 and self.format_paren_level == self.paren_level) {
                            escape_end_state = .FormatString;
                            state = .FormatString;
                            res = .Format;
                            continue;
                        }
                        res = .Colon;
                        break;
                    },
                    '.' => {
                        res = .Period;
                        break;
                    },
                    '-' => {
                        state = .Minus;
                    },
                    '/' => {
                        state = .Slash;
                    },
                    '&' => {
                        state = .Ampersand;
                    },
                    '0' => {
                        state = .Zero;
                    },
                    '1'...'9' => {
                        state = .Number;
                    },
                    'f' => if (self.format_string != 0) {
                        state = .Identifier;
                    } else {
                        state = .f;
                    },
                    else => {
                        if (isWhiteSpace(c)) {
                            start_index = self.it.i;
                        } else if (isIdentifier(c)) {
                            state = .Identifier;
                        } else {
                            return self.reportErr("invalid character", c);
                        }
                    },
                },
                .String => switch (c) {
                    '\\' => {
                        state = .EscapeSequence;
                    },
                    '\n', '\r' => {
                        if (str_delimit == '\'') {
                            return self.reportErr("invalid newline, use'\"' for multiline strings", c);
                        }
                    },
                    else => {
                        if (c == str_delimit) {
                            self.string = false;
                            res = .String;
                            break;
                        }
                    },
                },
                .EscapeSequence => switch (c) {
                    '\'', '"', '\\', 'r', 't', 'n' => {
                        state = escape_end_state;
                    },
                    'x' => {
                        counter = 0;
                        state = .HexEscape;
                    },
                    'u' => {
                        state = .UnicodeStart;
                    },
                    else => {
                        return self.reportErr("invalid escape sequence", c);
                    },
                },
                .HexEscape => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        counter += 1;
                        if (counter > 2) {
                            state = escape_end_state;
                        }
                    },
                    else => {
                        if (counter != 2) {
                            return self.reportErr("\\x pattern must be followed by 2 hex digits", c);
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        state = escape_end_state;
                    },
                },
                .UnicodeStart => if (c == '{') {
                    counter = 0;
                    state = .UnicodeEscape;
                } else {
                    return self.reportErr("expected '{' after '\\u'", c);
                },
                .UnicodeEscape => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        counter += 1;
                        if (counter > 6) {
                            state = .UnicodeEnd;
                        }
                    },
                    '}' => {
                        state = escape_end_state;
                    },
                    else => {
                        return self.reportErr("expected hex digits or '}'", c);
                    },
                },
                .UnicodeEnd => if (c == '}') {
                    state = escape_end_state;
                } else {
                    return self.reportErr("expected '}'", c);
                },
                .f => switch (c) {
                    '\'' => {
                        self.format_string = '\'';
                        self.format_paren_level = self.paren_level;
                        res = .FormatStart;
                        state = .FormatString;
                        escape_end_state = .FormatString;
                    },
                    '"' => {
                        self.format_string = '"';
                        self.format_paren_level = self.paren_level;
                        res = .FormatStart;
                        state = .FormatString;
                        escape_end_state = .FormatString;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        state = .Identifier;
                    },
                },
                .FormatString => {
                    if (c == self.format_string) {
                        if (res == .FormatStart) {
                            res = .String;
                        } else {
                            res = .FormatEnd;
                        }
                        self.format_string = 0;
                        break;
                    } else if (c == '\\') {
                        state = .EscapeSequence;
                    } else if (c == '\n' or c == '\r') {
                        if (self.format_string == '\'') {
                            return self.reportErr("invalid newline, use'\"' for multiline strings", c);
                        }
                    } else if (c == '{') {
                        state = .FormatBrace;
                    }
                },
                .FormatBrace => {
                    if (c == '{') {
                        state = .FormatString;
                    } else {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        break;
                    }
                },
                .Identifier => {
                    if (!isIdentifier(c)) {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        const slice = self.it.bytes[start_index..self.it.i];
                        res = Token.keywords.get(slice) orelse .Identifier;
                        break;
                    }
                },
                .Equal => switch (c) {
                    '=' => {
                        res = .EqualEqual;
                        break;
                    },
                    '>' => {
                        res = .EqualRarr;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Equal;
                        break;
                    },
                },
                .Bang => switch (c) {
                    '=' => {
                        res = .BangEqual;
                        break;
                    },
                    else => {
                        return self.reportErr("invalid character, use 'not' for boolean not", c);
                    },
                },
                .Pipe => switch (c) {
                    '=' => {
                        res = .PipeEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Pipe;
                        break;
                    },
                },
                .Percent => switch (c) {
                    '=' => {
                        res = .PercentEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Percent;
                        break;
                    },
                },
                .Asterisk => switch (c) {
                    '=' => {
                        res = .AsteriskEqual;
                        break;
                    },
                    '*' => {
                        state = .AsteriskAsterisk;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Asterisk;
                        break;
                    },
                },
                .AsteriskAsterisk => switch (c) {
                    '=' => {
                        res = .AsteriskAsteriskEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .AsteriskAsterisk;
                        break;
                    },
                },
                .Plus => switch (c) {
                    '=' => {
                        res = .PlusEqual;
                        break;
                    },
                    '+' => {
                        res = .PlusPlus;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Plus;
                        break;
                    },
                },
                .LArr => switch (c) {
                    '<' => {
                        state = .LArrArr;
                    },
                    '=' => {
                        res = .LArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .LArr;
                        break;
                    },
                },
                .LArrArr => switch (c) {
                    '=' => {
                        res = .LArrArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .LArrArr;
                        break;
                    },
                },
                .RArr => switch (c) {
                    '>' => {
                        state = .RArrArr;
                    },
                    '=' => {
                        res = .RArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .RArr;
                        break;
                    },
                },
                .RArrArr => switch (c) {
                    '=' => {
                        res = .RArrArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .RArrArr;
                        break;
                    },
                },
                .Caret => switch (c) {
                    '=' => {
                        res = .CaretEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Caret;
                        break;
                    },
                },
                .Minus => switch (c) {
                    '=' => {
                        res = .MinusEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Minus;
                        break;
                    },
                },
                .Slash => switch (c) {
                    '/' => {
                        state = .SlashSlash;
                    },
                    '=' => {
                        res = .SlashEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Slash;
                        break;
                    },
                },
                .SlashSlash => switch (c) {
                    '=' => {
                        res = .SlashSlashEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .SlashSlash;
                        break;
                    },
                },
                .Ampersand => switch (c) {
                    '=' => {
                        res = .AmpersandEqual;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .Ampersand;
                        break;
                    },
                },
                .LineComment => switch (c) {
                    '\n', '\r' => {
                        self.it.i -= 1;
                        res = .Comment;
                        break;
                    },
                    else => {},
                },
                .Zero => switch (c) {
                    'b' => {
                        state = .BinaryNumber;
                    },
                    'o' => {
                        state = .OctalNumber;
                    },
                    'x' => {
                        state = .HexNumber;
                    },
                    '.', ',' => {
                        state = .NumberDot;
                        dot_index = self.it.i - 1;
                    },
                    '0'...'7' => {
                        return self.reportErr("invalid character, octal literals start with '0o'", c);
                    },
                    '_' => {
                        state = .Number;
                    },
                    else => {
                        if (isIdentifier(c)) {
                            state = .Identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .NumberDot => switch (c) {
                    '.' => {
                        self.it.i -= 2;
                        res = .Integer;
                        break;
                    },
                    '0'...'9', 'e', 'E', '_' => {
                        self.it.i -= 1;
                        state = .FloatFraction;
                    },
                    else => {
                        self.it.i = dot_index.?;
                        res = .Integer;
                        break;
                    },
                },
                .BinaryNumber => switch (c) {
                    '0', '1', '_' => {},
                    else => {
                        if (isIdentifier(c)) {
                            state = .Identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .OctalNumber => switch (c) {
                    '0'...'7', '_' => {},
                    else => {
                        if (isIdentifier(c)) {
                            state = .Identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .HexNumber => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    'p', 'P' => {
                        state = .FloatExponent;
                    },
                    else => {
                        if (isIdentifier(c)) {
                            state = .Identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .Number => switch (c) {
                    '0'...'9', '_' => {},
                    '.' => {
                        state = .NumberDot;
                        dot_index = self.it.i - 1;
                    },
                    'e', 'E' => {
                        state = .FloatExponent;
                    },
                    else => {
                        if (isIdentifier(c)) {
                            state = .Identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .FloatFraction => switch (c) {
                    '0'...'9', '_' => {},
                    'e', 'E' => {
                        state = .FloatExponent;
                    },
                    '.', ',' => {
                        self.it.i -= 1;
                        res = .Number;
                        break;
                    },
                    else => {
                        if (isIdentifier(c)) {
                            self.it.i = dot_index orelse {
                                state = .Identifier;
                                continue;
                            };
                            res = .Integer;
                            break;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Number;
                        break;
                    },
                },
                .FloatExponent => switch (c) {
                    '+', '-' => {
                        state = .FloatExponentDigits;
                    },
                    else => {
                        if (isIdentifier(c)) {
                            self.it.i = dot_index orelse {
                                state = .Identifier;
                                continue;
                            };
                            res = .Integer;
                            break;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        state = .FloatExponentDigits;
                    },
                },
                .FloatExponentDigits => switch (c) {
                    '0'...'9' => {
                        counter += 1;
                    },
                    '_' => {},
                    else => {
                        if (counter != 0 and !isIdentifier(c)) {
                            self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                            res = .Number;
                            break;
                        }
                        self.it.i = dot_index orelse {
                            self.it.i = start_index;
                            state = .Identifier;
                            continue;
                        };
                        res = .Integer;
                        break;
                    },
                },
            }
        } else {
            switch (state) {
                .Start => {},
                .Identifier => {
                    const slice = self.it.bytes[start_index..];
                    res = Token.keywords.get(slice) orelse .Identifier;
                },
                .BinaryNumber,
                .OctalNumber,
                .HexNumber,
                .Number,
                .Zero,
                => res = .Integer,
                .FormatBrace => {},

                .String, .FormatString => {
                    if (self.repl) {
                        // if running in repl this might be a multiline string
                        self.it.i = start_index;
                        res = .Eof;
                    } else return self.reportErr("unterminated string", 'a');
                },
                .LineComment => res = .Comment,
                .FloatFraction => res = .Number,
                .Equal => res = .Equal,
                .Minus => res = .Minus,
                .Slash => res = .Slash,
                .SlashSlash => res = .SlashSlash,
                .Ampersand => res = .Ampersand,
                .Pipe => res = .Pipe,
                .RArr => res = .RArr,
                .RArrArr => res = .RArrArr,
                .LArr => res = .LArr,
                .LArrArr => res = .LArrArr,
                .Plus => res = .Plus,
                .Percent => res = .Percent,
                .Caret => res = .Caret,
                .Asterisk => res = .Asterisk,
                .AsteriskAsterisk => res = .AsteriskAsterisk,
                .NumberDot => {
                    self.it.i -= 1;
                    res = .Integer;
                },
                else => {
                    return self.reportErr("unexpected EOF", 'a');
                },
            }
        }
        return Token{
            .id = res,
            .start = @truncate(u32, start_index),
            .end = @truncate(u32, self.it.i),
        };
    }
};

fn expectTokens(source: []const u8, expected_tokens: []const Token.Id) !void {
    var errors = Errors.init(std.testing.allocator);
    defer errors.deinit();
    var tokenizer = Tokenizer{
        .tokens = undefined,
        .errors = &errors,
        .repl = false,
        .it = .{
            .i = 0,
            .bytes = source,
        },
    };
    blk: {
        for (expected_tokens) |expected_token| {
            const token = tokenizer.next() catch break :blk;
            try std.testing.expectEqual(expected_token, token.id);
        }
        const last_token = tokenizer.next() catch break :blk;
        try std.testing.expect(last_token.id == .Eof);
        return;
    }
    errors.render(source, std.io.getStdErr().writer()) catch {};
    @panic("test failed");
}

test "operators" {
    try expectTokens(
        \\!= | |= = ==
        \\( ) { } [ ] . @ =>
        \\^ ^= + += ++ - -=
        \\* *= ** **= % %= / /= // //=
        \\, & &= < <= <<
        \\<<= > >= >> >>= ~ _
        \\#hello world
    , &[_]Token.Id{
        .BangEqual,
        .Pipe,
        .PipeEqual,
        .Equal,
        .EqualEqual,
        .Nl,
        .LParen,
        .RParen,
        .LBrace,
        .RBrace,
        .LBracket,
        .RBracket,
        .Period,
        .At,
        .EqualRarr,
        .Nl,
        .Caret,
        .CaretEqual,
        .Plus,
        .PlusEqual,
        .PlusPlus,
        .Minus,
        .MinusEqual,
        .Nl,
        .Asterisk,
        .AsteriskEqual,
        .AsteriskAsterisk,
        .AsteriskAsteriskEqual,
        .Percent,
        .PercentEqual,
        .Slash,
        .SlashEqual,
        .SlashSlash,
        .SlashSlashEqual,
        .Nl,
        .Comma,
        .Ampersand,
        .AmpersandEqual,
        .LArr,
        .LArrEqual,
        .LArrArr,
        .Nl,
        .LArrArrEqual,
        .RArr,
        .RArrEqual,
        .RArrArr,
        .RArrArrEqual,
        .Tilde,
        .Underscore,
        .Nl,
        .Comment,
    });
}

test "keywords" {
    try expectTokens(
        \\not　and or let continue break return if else false true for
        \\while match catch try error import is in fn as const this
    , &[_]Token.Id{
        .Keyword_not,
        .Keyword_and,
        .Keyword_or,
        .Keyword_let,
        .Keyword_continue,
        .Keyword_break,
        .Keyword_return,
        .Keyword_if,
        .Keyword_else,
        .Keyword_false,
        .Keyword_true,
        .Keyword_for,
        .Nl,
        .Keyword_while,
        .Keyword_match,
        .Keyword_catch,
        .Keyword_try,
        .Keyword_error,
        .Keyword_import,
        .Keyword_is,
        .Keyword_in,
        .Keyword_fn,
        .Keyword_as,
        .Keyword_const,
        .Keyword_this,
    });
}

test "indentation" {
    try expectTokens(
        \\if
        \\    if
        \\
        \\        if
        \\if
        \\ if
        \\ if
        \\
    , &[_]Token.Id{
        .Keyword_if,
        .Nl,
        .{ .Indent = 1 },
        .Keyword_if,
        .Nl,
        .{ .Indent = 2 },
        .Keyword_if,
        .Nl,
        .Keyword_if,
        .Nl,
        .{ .Indent = 1 },
        .Keyword_if,
        .Nl,
        .{ .Indent = 1 },
        .Keyword_if,
        .Nl,
    });
}

test "identifiers" {
    try expectTokens(
        \\0b1gg
        \\0x1gg
        \\0o1gg
        \\0gg
        \\1gg
    , &[_]Token.Id{
        .Identifier,
        .Nl,
        .Identifier,
        .Nl,
        .Identifier,
        .Nl,
        .Identifier,
        .Nl,
        .Identifier,
    });
    try expectTokens(
        \\30.30f
        \\30.30ee
        \\30.30e+12a
        \\30.30e+12-
        \\30.30e+-
    , &[_]Token.Id{
        .Integer,
        .Period,
        .Identifier,
        .Nl,
        .Integer,
        .Period,
        .Identifier,
        .Nl,
        .Integer,
        .Period,
        .Identifier,
        .Plus,
        .Identifier,
        .Nl,
        .Number,
        .Minus,
        .Nl,
        .Integer,
        .Period,
        .Identifier,
        .Plus,
        .Minus,
    });
}

test "numbers" {
    try expectTokens(
        \\0.
        \\0,
        \\0.0
        \\0,0
        \\0.0.0
        \\0,0,0
    , &[_]Token.Id{
        .Integer,
        .Period,
        .Nl,
        .Integer,
        .Comma,
        .Nl,
        .Number,
        .Nl,
        .Number,
        .Nl,
        .Number,
        .Period,
        .Integer,
        .Nl,
        .Number,
        .Comma,
        .Integer,
    });
}

test "format string" {
    try expectTokens(
        \\f f"\u{12}{12:12} foo \t\n {f"foo bar" ++ {1:2} as str:3} \x12 " :
    , &[_]Token.Id{
        .Identifier,
        .FormatStart,
        .Integer,
        .Format,
        .Identifier,
        .String,
        .PlusPlus,
        .LBrace,
        .Integer,
        .Colon,
        .Integer,
        .RBrace,
        .Keyword_as,
        .Identifier,
        .FormatEnd,
        .Colon,
    });
}
