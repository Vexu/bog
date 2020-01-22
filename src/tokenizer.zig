const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const unicode = std.unicode;

fn isWhiteSpace(c: u32) bool {
    return switch (c) {
        ' ', '\t',
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
        0xFFA0 => true,
        else => false,
    };
}

fn isIdentifier(c: u32) bool {
    // TODO unicode identifiers
    return switch (c) {
        'a'...'z',
        'A'...'Z',
        '_',
        '0'...'9',
        => true,
        else => false,
    };
}

pub const Token = struct {
    start: usize,
    id: Id,

    const Id = union(enum) {
        Invalid: []const u8,
        Eof,
        Identifier: []const u8,
        String: []const u8,
        Number: []const u8,
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
        Ellipsis,
        Caret,
        CaretEqual,
        Plus,
        PlusEqual,
        Minus,
        MinusEqual,
        Asterisk,
        AsteriskEqual,
        // AsteriskAsterisk,
        // AsteriskAsteriskEqual,
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
    };

    pub const Keyword = struct {
        bytes: []const u8,
        id: Id,
    };

    pub const keywords = [_]Keyword{
        .{ .bytes = "not", .id = .Keyword_not },
        .{ .bytes = "and", .id = .Keyword_and },
        .{ .bytes = "or", .id = .Keyword_or },
        .{ .bytes = "let", .id = .Keyword_let },
        .{ .bytes = "continue", .id = .Keyword_continue },
        .{ .bytes = "break", .id = .Keyword_break },
        .{ .bytes = "return", .id = .Keyword_return },
        .{ .bytes = "if", .id = .Keyword_if },
        .{ .bytes = "else", .id = .Keyword_else },
        .{ .bytes = "false", .id = .Keyword_false },
        .{ .bytes = "true", .id = .Keyword_true },
        .{ .bytes = "for", .id = .Keyword_for },
        .{ .bytes = "while", .id = .Keyword_while },
        .{ .bytes = "match", .id = .Keyword_match },
        .{ .bytes = "catch", .id = .Keyword_catch },
        .{ .bytes = "try", .id = .Keyword_try },
        .{ .bytes = "error", .id = .Keyword_error },
        .{ .bytes = "import", .id = .Keyword_import },
        .{ .bytes = "is", .id = .Keyword_is },
        .{ .bytes = "in", .id = .Keyword_in },
        .{ .bytes = "fn", .id = .Keyword_fn },
    };

    pub fn getKeyword(bytes: []const u8) ?Token.Id {
        for (keywords) |kw| {
            if (mem.eql(u8, kw.bytes, bytes)) {
                return kw.id;
            }
        }
        return null;
    }
};

pub const TokenList = std.SegmentedList(Token, 64);

pub const Tokenizer = struct {
    it: unicode.Utf8Iterator,
    start_index: usize = 0,
    repl: bool = false,

    pub fn next(self: *Tokenizer) Token {
        self.start_index = self.it.i;
        var state: enum {
            Start,
            Cr,
            BackSlash,
            BackSlashCr,
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
            Plus,
            LArr,
            LArrArr,
            RArr,
            RArrArr,
            Caret,
            Period,
            Period2,
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
            FloatFraction,
            FloatExponent,
            FloatExponentDigits,
        } = .Start;
        var res = Token{
            .id = .Eof,
            .start = self.start_index,
        };
        var str_delimit: u32 = undefined;
        var counter: u32 = 0;

        while (self.it.nextCodepoint()) |c| {
            switch (state) {
                .Start => switch (c) {
                    '#' => {
                        state = .LineComment;
                    },
                    '\n' => {
                        res.id = .Nl;
                        break;
                    },
                    '\r' => {
                        state = .Cr;
                    },
                    '"', '\'' => {
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
                        res.id = .LParen;
                        break;
                    },
                    ')' => {
                        res.id = .RParen;
                        break;
                    },
                    '[' => {
                        res.id = .LBracket;
                        break;
                    },
                    ']' => {
                        res.id = .RBracket;
                        break;
                    },
                    ',' => {
                        res.id = .Comma;
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
                        res.id = .LBrace;
                        break;
                    },
                    '}' => {
                        res.id = .RBrace;
                        break;
                    },
                    '~' => {
                        res.id = .Tilde;
                        break;
                    },
                    ':' => {
                        res.id = .Colon;
                        break;
                    },
                    '.' => {
                        state = .Period;
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
                    '\\' => {
                        state = .BackSlash;
                    },
                    else => {
                        if (isWhiteSpace(c)) {
                            self.start_index = self.it.i;
                            res.start = self.start_index;
                        } else if (isIdentifier(c)) {
                            state = .Identifier;
                        } else {
                            res.id = .{ .Invalid = "invalid character" };
                            break;
                        }
                    },
                },
                .Cr => switch (c) {
                    '\n' => {
                        res.id = .Nl;
                        break;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid character" };
                        break;
                    },
                },
                .BackSlash => switch (c) {
                    '\n' => {
                        state = .Start;
                    },
                    '\r' => {
                        state = .BackSlashCr;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid character" };
                        break;
                    },
                },
                .BackSlashCr => switch (c) {
                    '\n' => {
                        state = .Start;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid character" };
                        break;
                    },
                },
                .String => switch (c) {
                    '\\' => {
                        state = .EscapeSequence;
                    },
                    '\n', '\r' => {
                        if (str_delimit == '\'') {
                            res.id = .{ .Invalid = "invalid newline, use'\"' for multiline strings" };
                            break;
                        }
                    },
                    else => {
                        if (c == str_delimit) {
                            res.id = .{ .String = self.it.bytes[self.start_index..self.it.i] };
                            break;
                        }
                    },
                },
                .EscapeSequence => switch (c) {
                    '\'', '"', '\\', 'r', 't', '\n' => {
                        state = .String;
                    },
                    'x' => {
                        counter = 0;
                        state = .HexEscape;
                    },
                    'u' => {
                        state = .UnicodeStart;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid escape sequence" };
                        break;
                    },
                },
                .HexEscape => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        counter += 1;
                        if (counter > 2) {
                            state = .String;
                        }
                    },
                    else => {
                        state = .String;
                    },
                },
                .UnicodeStart => if (c == '{') {
                    counter = 0;
                    state = .UnicodeEscape;
                } else {
                    res.id = .{ .Invalid = "invalid escape sequence" };
                    break;
                },
                .UnicodeEscape => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        counter += 1;
                        if (counter > 6) {
                            state = .UnicodeEnd;
                        }
                    },
                    '}' => {
                        state = .String;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid escape sequence" };
                        break;
                    },
                },
                .UnicodeEnd => if (c == '}') {
                    state = .String;
                } else {
                    res.id = .{ .Invalid = "invalid escape sequence" };
                    break;
                },
                .Identifier => {
                    if (!isIdentifier(c)) {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        const slice = self.it.bytes[self.start_index..self.it.i];
                        res.id = Token.getKeyword(slice) orelse .{ .Identifier = slice };
                        break;
                    }
                },
                .Equal => switch (c) {
                    '=' => {
                        res.id = .EqualEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Equal;
                        break;
                    },
                },
                .Bang => switch (c) {
                    '=' => {
                        res.id = .BangEqual;
                        break;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid character, use 'not' for boolean not" };
                        break;
                    },
                },
                .Pipe => switch (c) {
                    '=' => {
                        res.id = .PipeEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Pipe;
                        break;
                    },
                },
                .Percent => switch (c) {
                    '=' => {
                        res.id = .PercentEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Percent;
                        break;
                    },
                },
                .Asterisk => switch (c) {
                    '=' => {
                        res.id = .AsteriskEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Asterisk;
                        break;
                    },
                },
                .Plus => switch (c) {
                    '=' => {
                        res.id = .PlusEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Plus;
                        break;
                    },
                },
                .LArr => switch (c) {
                    '<' => {
                        state = .LArrArr;
                    },
                    '=' => {
                        res.id = .LArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .LArr;
                        break;
                    },
                },
                .LArrArr => switch (c) {
                    '=' => {
                        res.id = .LArrArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 2;
                        res.id = .LArrArr;
                        break;
                    },
                },
                .RArr => switch (c) {
                    '>' => {
                        state = .RArrArr;
                    },
                    '=' => {
                        res.id = .RArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .RArr;
                        break;
                    },
                },
                .RArrArr => switch (c) {
                    '=' => {
                        res.id = .RArrArrEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 2;
                        res.id = .RArrArr;
                        break;
                    },
                },
                .Caret => switch (c) {
                    '=' => {
                        res.id = .CaretEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Caret;
                        break;
                    },
                },
                .Period => switch (c) {
                    '.' => {
                        state = .Period2;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Period;
                        break;
                    },
                },
                .Period2 => switch (c) {
                    '.' => {
                        res.id = .Ellipsis;
                        break;
                    },
                    else => {
                        res.id = .{ .Invalid = "invalid character" };
                        break;
                    },
                },
                .Minus => switch (c) {
                    '=' => {
                        res.id = .MinusEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Minus;
                        break;
                    },
                },
                .Slash => switch (c) {
                    '/' => {
                        state = .SlashSlash;
                    },
                    '=' => {
                        res.id = .SlashEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Slash;
                        break;
                    },
                },
                .SlashSlash => switch (c) {
                    '=' => {
                        res.id = .SlashSlashEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 2;
                        res.id = .SlashSlash;
                        break;
                    },
                },
                .Ampersand => switch (c) {
                    '=' => {
                        res.id = .AmpersandEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res.id = .Ampersand;
                        break;
                    },
                },
                .LineComment => switch (c) {
                    '\n', '\r' => {
                        self.it.i -= 1;
                        state = .Start;
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
                    '.' => {
                        state = .FloatFraction;
                    },
                    '0'...'9', 'a', 'c'...'f', 'A'...'F' => {
                        res.id = .{ .Invalid = "octal literals start with '0o'" };
                        break;
                    },
                    '_' => {
                        state = .Number;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
                .BinaryNumber => switch (c) {
                    '0', '1', '_' => {},
                    '2'...'9', 'a'...'f', 'A'...'F' => {
                        res.id = .{ .Invalid = "invalid digit in octal number" };
                        break;
                    },
                    '.' => {
                        res.id = .{ .Invalid = "invalid base for floating point number" };
                        break;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
                .OctalNumber => switch (c) {
                    '0'...'7', '_' => {},
                    '8'...'9', 'a'...'f', 'A'...'F' => {
                        res.id = .{ .Invalid = "invalid digit in number" };
                        break;
                    },
                    '.' => {
                        res.id = .{ .Invalid = "invalid base for floating point number" };
                        break;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
                .HexNumber => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    '.' => {
                        res.id = .{ .Invalid = "invalid base for floating point number" };
                        break;
                    },
                    'p', 'P' => {
                        state = .FloatExponent;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
                .Number => switch (c) {
                    '0'...'9', '_' => {},
                    'a'...'d', 'f', 'A'...'F' => {
                        res.id = .{ .Invalid = "invalid digit in hex number" };
                        break;
                    },
                    '.' => {
                        state = .FloatFraction;
                    },
                    'e' => {
                        state = .FloatExponent;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
                .FloatFraction => switch (c) {
                    '0'...'9', '_' => {},
                    'e' => {
                        state = .FloatExponent;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
                .FloatExponent => switch (c) {
                    '+', '-' => {
                        state = .FloatExponentDigits;
                    },
                    else => {
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
                        if (counter == 0) {
                            res.id = .{ .Invalid = "invalid exponent digit" };
                            break;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res.id = .{ .Number = self.it.bytes[self.start_index..self.it.i] };
                        break;
                    },
                },
            }
        } else {
            switch (state) {
                .LineComment, .Start => {},
                .Identifier => {
                    const slice = self.it.bytes[self.start_index..];
                    res.id = Token.getKeyword(slice) orelse .{ .Identifier = slice };
                },

                .FloatFraction,
                .BinaryNumber,
                .OctalNumber,
                .HexNumber,
                .Number,
                .Zero,
                => res.id = .{ .Number = self.it.bytes[self.start_index..] },

                .String => {
                    if (self.repl) {
                        res.start = self.it.i;
                        self.it.i = self.start_index;
                        res.id = .Eof;
                    } else
                        res.id = .{ .Invalid = "unterminated string" };
                },

                .Equal => res.id = .Equal,
                .Minus => res.id = .Minus,
                .Slash => res.id = .Slash,
                .SlashSlash => res.id = .SlashSlash,
                .Ampersand => res.id = .Ampersand,
                .Period => res.id = .Period,
                .Pipe => res.id = .Pipe,
                .RArr => res.id = .RArr,
                .RArrArr => res.id = .RArrArr,
                .LArr => res.id = .LArr,
                .LArrArr => res.id = .LArrArr,
                .Plus => res.id = .Plus,
                .Percent => res.id = .Percent,
                .Caret => res.id = .Caret,
                .Asterisk => res.id = .Asterisk,
                else => {
                    res.id = .{ .Invalid = "unexpected eof" };
                },
            }
        }
        return res;
    }
};

fn expectTokens(source: []const u8, expected_tokens: []const Token.Id) void {
    var tokenizer = Tokenizer{
        .it = .{
            .i = 0,
            .bytes = source,
        },
    };
    for (expected_tokens) |expected_token| {
        const token = tokenizer.next();
        std.testing.expectEqual(expected_token, token.id);
    }
    const last_token = tokenizer.next();
    std.testing.expect(last_token.id == .Eof);
}

test "operators" {
    expectTokens(
        \\!= | |= = ==
        \\( ) { } [ ] . ...
        \\^ ^= + += - -=
        \\* *= % %= / /= // //=
        \\, & &= < <= <<
        \\<<= > >= >> >>= ~
        \\
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
        .Ellipsis,
        .Nl,
        .Caret,
        .CaretEqual,
        .Plus,
        .PlusEqual,
        .Minus,
        .MinusEqual,
        .Nl,
        .Asterisk,
        .AsteriskEqual,
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
        .Nl,
    });
}

test "keywords" {
    expectTokens(
        \\not　and or let continue break return if else false true for while match catch try error import is in fn
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
        .Keyword_while,
        .Keyword_match,
        .Keyword_catch,
        .Keyword_try,
        .Keyword_error,
        .Keyword_import,
        .Keyword_is,
        .Keyword_in,
        .Keyword_fn,
    });
}
