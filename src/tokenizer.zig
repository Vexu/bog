const std = @import("std");
const mem = std.mem;
const math = std.math;
const testing = std.testing;
const unicode = std.unicode;
const lang = @import("lang.zig");
const Tree = lang.Tree;

fn isWhiteSpace(c: u32) bool {
    return switch (c) {
        ' ', '\t', '\r', ';', // lul
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

    pub const List = std.SegmentedList(Token, 64);
    pub const Index = u32;

    pub const Id = enum {
        Eof,
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
        Keyword_const,
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
        .{ .bytes = "const", .id = .Keyword_const },
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
            .Comment => "<Comment>",
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
            .Keyword_const => "const",
        };
    }
};

pub const Tokenizer = struct {
    tree: *Tree,
    it: unicode.Utf8Iterator,
    start_index: usize = 0,
    level: u32 = 0,
    string: bool = false,
    prev_nl: bool = true,
    no_eof: bool = false,
    repl: bool,

    pub const Error = error{TokenizeError} || mem.Allocator.Error;

    pub fn tokenize(tree: *Tree) Error!void {
        var tokenizer = Tokenizer{
            .tree = tree,
            .it = .{
                .i = 0,
                .bytes = tree.source,
            },
            .repl = false,
        };
        while (true) {
            const tok = try tree.tokens.addOne();
            tok.* = try tokenizer.next();
            if (tok.id == .Eof) {
                return;
            }
        }
    }

    pub fn tokenizeRepl(self: *Tokenizer, input: []const u8) Error!bool {
        self.it.bytes = input;
        _ = self.tree.tokens.pop();
        self.tree.source = input;
        while (true) {
            const tok = try self.tree.tokens.addOne();
            tok.* = try self.next();
            if (tok.id == .Eof) {
                return if (self.repl and (self.level != 0 or self.string or self.no_eof))
                    false
                else
                    true;
            }
        }
    }

    fn reportErr(self: *Tokenizer, msg: []const u8, c: u21) Error {
        try self.tree.errors.push(.{
            .index = @truncate(u32, self.it.i - (unicode.utf8CodepointSequenceLength(c) catch unreachable)),
            .kind = .Error,
            .msg = msg,
        });
        self.it.i = self.it.bytes.len;
        return error.TokenizeError;
    }

    fn next(self: *Tokenizer) !Token {
        self.start_index = self.it.i;
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
            NumberDot,
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
                            return self.reportErr("unmatched ')'", c);
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
                            return self.reportErr("unmatched ']'", c);
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
                            return self.reportErr("unmatched '}'", c);
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
                    else => {
                        if (isWhiteSpace(c)) {
                            self.start_index = self.it.i;
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
                        return self.reportErr("invalid escape sequence", c);
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
                        if (counter != 2) {
                            return self.reportErr("\\x pattern must be followed by 2 hex digits", c);
                        }
                        state = .String;
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
                        state = .String;
                    },
                    else => {
                        return self.reportErr("expected hex digits or '}'", c);
                    },
                },
                .UnicodeEnd => if (c == '}') {
                    state = .String;
                } else {
                    return self.reportErr("expected '}'", c);
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
                        return self.reportErr("invalid character, use 'not' for boolean not", c);
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
                        return self.reportErr("invalid character", c);
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
                    '.' => {
                        state = .NumberDot;
                    },
                    '0'...'9', 'a', 'c'...'f', 'A'...'F' => {
                        return self.reportErr("invalid character, octal literals start with '0o'", c);
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
                .NumberDot => switch (c) {
                    '.' => {
                        self.it.i -= 2;
                        res = .Integer;
                        break;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        state = .FloatFraction;
                    },
                },
                .BinaryNumber => switch (c) {
                    '0', '1', '_' => {},
                    '2'...'9', 'a'...'f', 'A'...'F' => {
                        return self.reportErr("invalid digit in binary number", c);
                    },
                    '.' => {
                        return self.reportErr("invalid base for floating point number", c);
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
                        return self.reportErr("invalid digit in octal number", c);
                    },
                    '.' => {
                        return self.reportErr("invalid base for floating point number", c);
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
                        return self.reportErr("invalid base for floating point number", c);
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
                        return self.reportErr("invalid digit in number", c);
                    },
                    '.' => {
                        state = .NumberDot;
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
                            return self.reportErr("invalid exponent digit", c);
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .Number;
                        break;
                    },
                },
            }
        } else {
            switch (state) {
                .Start => {},
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
                        return self.reportErr("unterminated string", 'a');
                },
                .LineComment => res = .Comment,
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
                    return self.reportErr("unexpected EOF", 'a');
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

var test_tree = Tree{
    .errors = lang.ErrorMsg.List.init(std.testing.failing_allocator),

    .tokens = undefined,
    .nodes = undefined,
    .source = undefined,
    .arena_allocator = undefined,
};

fn expectTokens(source: []const u8, expected_tokens: []const Token.Id) void {
    var tokenizer = Tokenizer{
        .tree = &test_tree,
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
        .Comment,
    });
}

test "keywords" {
    expectTokens(
        \\not　and or let continue break return if else false true
        \\for while match catch try error import is in fn as const
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
        .Nl,
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
        .Keyword_const,
    });
}
