const std = @import("std");
const mem = std.mem;
const math = std.math;
const Allocator = mem.Allocator;
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

fn parseInt(buf: []const u8, radix: u8) Token.Id {
    var x: u64 = 0;

    for (buf) |c| {
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z' => c - 'A' + 10,
            'a'...'z' => c - 'a' + 10,
            '_' => continue,
            else => unreachable,
        };

        x = math.mul(u64, x, radix) catch return .{
            .Invalid = "integer too big",
        };
        x += digit;
    }

    return .{ .Integer = x };
}

pub const TokenIndex = u32;

pub const Token = struct {
    start: u32,
    end: u32,
    id: Id,

    pub const Id = enum {
        Eof,
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
        Ellipsis,
        Caret,
        CaretEqual,
        Plus,
        PlusEqual,
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
        .{ .bytes = "as", .id = .Keyword_as },
        .{ .bytes = "_", .id = .Underscore },
    };

    pub fn getKeyword(bytes: []const u8) ?Token.Id {
        for (keywords) |kw| {
            if (mem.eql(u8, kw.bytes, bytes)) {
                return kw.id;
            }
        }
        return null;
    }

    pub fn string(id: Id) []const u8 {
        return switch (id) {
            .Identifier => "Identifier",
            .Eof => "<EOF>",
            .String => "String",
            .Integer => "Integer",
            .Number => "Number",
            .Nl => "<NL>",
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
            .Ellipsis => "...",
            .Caret => "^",
            .CaretEqual => "^=",
            .Plus => "+",
            .PlusEqual => "+=",
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
        };
    }
};

pub const TokenList = std.SegmentedList(Token, 64);
pub const ErrorList = std.SegmentedList(Error, 0);

const Error = struct {
    msg: []const u8,
    index: u32,
};

pub const Tokenizer = struct {
    it: unicode.Utf8Iterator,
    tokens: TokenList,
    errors: ErrorList,
    start_index: usize = 0,
    level: u32 = 0,
    string: bool = false,
    prev_nl: bool = true,
    no_eof: bool = false,
    repl: bool,

    pub const TokenizeError = error{TokenizeError} || Allocator.Error;

    pub fn init(allocator: *Allocator, repl: bool) Tokenizer {
        return .{
            .it = .{
                .i = 0,
                .bytes = "",
            },
            .tokens = TokenList.init(allocator),
            .repl = repl,
        };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.tokens.deinit();
    }

    pub fn tokenize(self: *Tokenizer, input: []const u8) TokenizeError!bool {
        self.it.bytes = input;
        _ = self.tokens.pop();
        while (true) {
            const tok = try self.tokens.addOne();
            tok.* = self.next();
            if (tok.id == .Eof) {
                return if (self.repl and (self.level != 0 or self.string or self.no_eof))
                    false
                else
                    true;
            }
        }
    }

    fn err(self: *Tokenizer, msg: []const u8) TokenizeError {
        try self.errors.push(.{
            .msg = msg,
            .index = @truncate(u32, self.it.i),
        });
        return error.TokenizeError;
    }

    fn next(self: *Tokenizer) !Token {
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
            AsteriskAsterisk,
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
        var res = Token.Id.Eof;
        var str_delimit: u32 = undefined;
        var counter: u32 = 0;

        while (self.it.nextCodepoint()) |c| {
            switch (state) {
                .Start => switch (c) {
                    '#' => {
                        state = .LineComment;
                    },
                    '\n' => {
                        if (self.prev_nl) {
                            self.start_index = self.it.i;
                        } else {
                            res = .Nl;
                            break;
                        }
                    },
                    '\r' => {
                        state = .Cr;
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
                        self.level += 1;
                        res = .LParen;
                        break;
                    },
                    ')' => {
                        if (self.level == 0) {
                            return self.err("unmatched ')'");
                        }
                        self.level -= 1;
                        res = .RParen;
                        break;
                    },
                    '[' => {
                        self.level += 1;
                        res = .LBracket;
                        break;
                    },
                    ']' => {
                        if (self.level == 0) {
                            return self.err("unmatched ']'");
                        }
                        self.level -= 1;
                        res = .RBracket;
                        break;
                    },
                    ',' => {
                        res = .Comma;
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
                        self.level += 1;
                        res = .LBrace;
                        break;
                    },
                    '}' => {
                        if (self.level == 0) {
                            return self.err("unmatched '}'");
                        }
                        self.level -= 1;
                        res = .RBrace;
                        break;
                    },
                    '~' => {
                        res = .Tilde;
                        break;
                    },
                    ':' => {
                        res = .Colon;
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
                        } else if (isIdentifier(c)) {
                            state = .Identifier;
                        } else {
                            return self.err("invalid character");
                        }
                    },
                },
                .Cr => switch (c) {
                    '\n' => {
                        if (self.prev_nl) {
                            self.start_index = self.it.i;
                        } else {
                            res = .Nl;
                            break;
                        }
                    },
                    else => {
                        return self.err("invalid character");
                    },
                },
                .BackSlash => switch (c) {
                    '\n' => {
                        self.no_eof = true;
                        state = .Start;
                    },
                    '\r' => {
                        state = .BackSlashCr;
                    },
                    else => {
                        return self.err("invalid character");
                    },
                },
                .BackSlashCr => switch (c) {
                    '\n' => {
                        self.no_eof = true;
                        state = .Start;
                    },
                    else => {
                        return self.err("invalid character");
                    },
                },
                .String => switch (c) {
                    '\\' => {
                        state = .EscapeSequence;
                    },
                    '\n', '\r' => {
                        if (str_delimit == '\'') {
                            return self.err("invalid newline, use'\"' for multiline strings");
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
                        return self.err("invalid escape sequence");
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
                    return self.err("invalid escape sequence");
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
                        return self.err("invalid escape sequence");
                    },
                },
                .UnicodeEnd => if (c == '}') {
                    state = .String;
                } else {
                    return self.err("invalid escape sequence");
                },
                .Identifier => {
                    if (!isIdentifier(c)) {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        const slice = self.it.bytes[self.start_index..self.it.i];
                        res = Token.getKeyword(slice) orelse .Identifier;
                        break;
                    }
                },
                .Equal => switch (c) {
                    '=' => {
                        res = .EqualEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
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
                        return self.err("invalid character, use 'not' for boolean not");
                    },
                },
                .Pipe => switch (c) {
                    '=' => {
                        res = .PipeEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 2;
                        res = .AsteriskAsterisk;
                        break;
                    },
                },
                .Plus => switch (c) {
                    '=' => {
                        res = .PlusEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 2;
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
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 2;
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
                        self.it.i = self.start_index + 1;
                        res = .Caret;
                        break;
                    },
                },
                .Period => switch (c) {
                    '.' => {
                        state = .Period2;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
                        res = .Period;
                        break;
                    },
                },
                .Period2 => switch (c) {
                    '.' => {
                        res = .Ellipsis;
                        break;
                    },
                    else => {
                        return self.err("invalid character");
                    },
                },
                .Minus => switch (c) {
                    '=' => {
                        res = .MinusEqual;
                        break;
                    },
                    else => {
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 1;
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
                        self.it.i = self.start_index + 2;
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
                        self.it.i = self.start_index + 1;
                        res = .Ampersand;
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
                        return self.err("octal literals start with '0o'");
                    },
                    '_' => {
                        state = .Number;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .BinaryNumber => switch (c) {
                    '0', '1', '_' => {},
                    '2'...'9', 'a'...'f', 'A'...'F' => {
                        return self.err("invalid digit in octal number");
                    },
                    '.' => {
                        return self.err("invalid base for floating point number");
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .OctalNumber => switch (c) {
                    '0'...'7', '_' => {},
                    '8'...'9', 'a'...'f', 'A'...'F' => {
                        return self.err("invalid digit in number");
                    },
                    '.' => {
                        return self.err("invalid base for floating point number");
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .HexNumber => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    '.' => {
                        return self.err("invalid base for floating point number");
                    },
                    'p', 'P' => {
                        state = .FloatExponent;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
                        break;
                    },
                },
                .Number => switch (c) {
                    '0'...'9', '_' => {},
                    'a'...'d', 'f', 'A'...'F' => {
                        return self.err("invalid digit in hex number");
                    },
                    '.' => {
                        state = .FloatFraction;
                    },
                    'e' => {
                        state = .FloatExponent;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Integer;
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
                        res = .Number;
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
                            return self.err("invalid exponent digit");
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Number;
                        break;
                    },
                },
            }
        } else {
            switch (state) {
                .LineComment, .Start => {},
                .Identifier => {
                    const slice = self.it.bytes[self.start_index..];
                    res = Token.getKeyword(slice) orelse .Identifier;
                },
                .BinaryNumber,
                .OctalNumber,
                .HexNumber,
                .Number,
                .Zero,
                => res = .Integer,

                .String => {
                    if (self.repl) {
                        self.it.i = self.start_index;
                        res = .Eof;
                    } else
                        return self.err("unterminated string");
                },

                .FloatFraction => res = .Number,
                .Equal => res = .Equal,
                .Minus => res = .Minus,
                .Slash => res = .Slash,
                .SlashSlash => res = .SlashSlash,
                .Ampersand => res = .Ampersand,
                .Period => res = .Period,
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
                else => {
                    return self.err("unexpected eof");
                },
            }
        }
        self.prev_nl = res == .Nl or res == .Eof;
        if (state != .Start)
            self.no_eof = false;
        return Token{
            .id = res,
            .start = @truncate(u32, self.start_index),
            .end = @truncate(u32, self.it.i),
        };
    }
};

fn expectTokens(source: []const u8, expected_tokens: []const Token.Id) void {
    var tokenizer = Tokenizer{
        .errors = undefined, // errors are not supposed to happen
        .tokens = undefined, // not used
        .repl = false,
        .it = .{
            .i = 0,
            .bytes = source,
        },
    };
    for (expected_tokens) |expected_token| {
        const token = tokenizer.next() catch unreachable;
        std.testing.expectEqual(expected_token, token.id);
    }
    const last_token = tokenizer.next() catch unreachable;
    std.testing.expect(last_token.id == .Eof);
}

test "operators" {
    expectTokens(
        \\!= | |= = ==
        \\( ) { } [ ] . ...
        \\^ ^= + += - -=
        \\* *= ** **= % %= / /= // //=
        \\, & &= < <= <<
        \\<<= > >= >> >>= ~ _
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
    });
}

test "keywords" {
    expectTokens(
        \\not　and or let continue break return if else false true for while match catch try error import is in fn as
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
        .Keyword_as,
    });
}
