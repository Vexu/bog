const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const unicode = std.unicode;

pub const Parser = struct {
    it: unicode.Utf8Iterator,
    indent_char: ?u32,
    chars_per_indent: u8,
    output_stream: var,
};

fn isWhiteSpace(c: u32) bool {
    return switch (c) {
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

fn getIndent(parser: *Parser) u8 {
    var count: u32 = 0;
    while (parser.it.nextCodePoint()) |c| {
        if (parser.indent_char) |some| {} else if (isWhiteSpace(c)) {
            parser.indent_char = c;
            count += 1;
        } else {
            if (count % parser.chars_per_indent) {
                try parser.err_stream.print("invalid indentation\n", .{});
                return error.Invalid;
            } else {
                const levels = @divExact(count, parser.chars_per_indent);
            }
        }
    }
}

pub const Token = union(enum) {
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
    };

    pub const Id = enum {};

    pub fn getKeyword(bytes: []const u8) ?Id {
        for (keywords) |kw| {
            if (mem.eql(u8, kw.bytes, bytes)) {
                return kw.id;
            }
        }
        return null;
    }

    pub fn next(parser: *Parser) !Token {
        var start_index = parser.it.index;
        var result = Token{
            .id = .Eof,
        };
        var state: enum {
            Start,
            Cr,
            BackSlash,
            BackSlashCr,
            StringLiteral,
            AfterStringLiteral,
            EscapeSequence,
            CrEscape,
            HexEscape,
            UnicodeEscape,
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
        var str_delimit: u32 = undefined;
        var string = false;
        var counter: u32 = 0;
        while (parser.it.nextCodePoint()) |c| {
            switch (state) {
                .Start => switch (c) {
                    '#' => {
                        state = .LineComment;
                    },
                    '\n' => {
                        result.id = .Nl;
                        start_index = parser.it.index;
                        break;
                    },
                    '\r' => {
                        state = .Cr;
                    },
                    '"', '\'' => {
                        start_index = parser.it.index;
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
                        result.id = .LParen;
                        break;
                    },
                    ')' => {
                        result.id = .RParen;
                        break;
                    },
                    '[' => {
                        result.id = .LBracket;
                        break;
                    },
                    ',' => {
                        result.id = .Comma;
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
                        result.id = .LBrace;
                        break;
                    },
                    '}' => {
                        result.id = .RBrace;
                        break;
                    },
                    '~' => {
                        result.id = .Tilde;
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
                            start_index = parser.it.index;
                        } else if (isIdentifier(c)) {
                            state = .Identifier;
                        } else {
                            try parser.err_stream.print("invalid character\n", .{});
                            return error.Invalid;
                        }
                    },
                },
                .Cr => switch (c) {
                    '\n' => {
                        result.id = .Nl;
                        start_index = parser.it.index;
                        break;
                    },
                    else => {
                        try parser.err_stream.print("invalid character\n", .{});
                        return error.Invalid;
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
                        try parser.err_stream.print("invalid character\n", .{});
                        return error.Invalid;
                    },
                },
                .BackSlashCr => switch (c) {
                    '\n' => {
                        state = if (string) .AfterStringLiteral else .Start;
                    },
                    else => {
                        try parser.err_stream.print("invalid character\n", .{});
                        return error.Invalid;
                    },
                },
                .String => switch (c) {
                    '\\' => {
                        state = .EscapeSequence;
                    },
                    '\n', '\r' => {
                        try parser.err_stream.print("invalid character\n", .{});
                        return error.Invalid;
                    },
                    else => {
                        if (c == str_delimit) {
                            result.id = .{ .String = parser.it.buffer[start_index..parser.it.index] };
                            break;
                        }
                    },
                },
                .EscapeSequence => switch (c) {
                    '\'', '"', '\\', 'n', 'r', 't', '\n' => {
                        state = .String;
                    },
                    '\r' => {
                        state = .CrEscape;
                    },
                    'x' => {
                        counter = 0;
                        state = .HexEscape;
                    },
                    'u' => {
                        state = .UnicodeStart;
                    },
                    else => {
                        try parser.err_stream.print("invalid escape sequence\n", .{});
                        return error.Invalid;
                    },
                },
                .CrEscape => switch (c) {
                    '\n' => {
                        state = .String;
                    },
                    else => {
                        try parser.err_stream.print("invalid character\n", .{});
                        return error.Invalid;
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
                    try parser.err_stream.print("invalid escape sequence\n", .{});
                    return error.Invalid;
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
                        try parser.err_stream.print("invalid escape sequence\n", .{});
                        return error.Invalid;
                    },
                },
                .UnicodeEnd => if (c == '}') {
                    state = .String;
                } else {
                    try parser.err_stream.print("invalid escape sequence\n", .{});
                    return error.Invalid;
                },
                .Identifier => {
                    if (isIdentifier(c)) {} else {
                        const slice = parser.it.buffer[start_index..parser.it.index];
                        result.id = getKeyword(slice) orelse .{ .Identifier = slice };
                        break;
                    }
                },
                .Equal => switch (c) {
                    '=' => {
                        result.id = .EqualEqual;
                        break;
                    },
                    else => {
                        result.id = .Equal;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .Bang => switch (c) {
                    '=' => {
                        result.id = .BangEqual;
                        break;
                    },
                    else => {
                        try parser.err_stream.print("invalid escape sequence\n", .{});
                        return error.Invalid;
                    },
                },
                .Pipe => switch (c) {
                    '=' => {
                        result.id = .PipeEqual;
                        break;
                    },
                    else => {
                        result.id = .Pipe;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .Percent => switch (c) {
                    '=' => {
                        result.id = .PercentEqual;
                        break;
                    },
                    else => {
                        result.id = .Percent;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .Asterisk => switch (c) {
                    '=' => {
                        result.id = .AsteriskEqual;
                        break;
                    },
                    else => {
                        result.id = .Asterisk;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .Plus => switch (c) {
                    '=' => {
                        result.id = .PlusEqual;
                        parser.it.index = start_index + 1;
                        break;
                    },
                    else => {
                        result.id = .Plus;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .LArr => switch (c) {
                    '<' => {
                        state = .LArrArr;
                    },
                    '=' => {
                        result.id = .LArrEqual;
                        break;
                    },
                    else => {
                        result.id = .LArr;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .LArrArr => switch (c) {
                    '=' => {
                        result.id = .LArrArrEqual;
                        break;
                    },
                    else => {
                        result.id = .LArrArr;
                        parser.it.index = start_index + 2;
                        break;
                    },
                },
                .RArr => switch (c) {
                    '>' => {
                        state = .RArrArr;
                    },
                    '=' => {
                        result.id = .RArrEqual;
                        break;
                    },
                    else => {
                        result.id = .RArr;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .RArrArr => switch (c) {
                    '=' => {
                        result.id = .RArrArrEqual;
                        break;
                    },
                    else => {
                        result.id = .RArrArr;
                        parser.it.index = start_index + 2;
                        break;
                    },
                },
                .Caret => switch (c) {
                    '=' => {
                        result.id = .CaretEqual;
                        break;
                    },
                    else => {
                        result.id = .Caret;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .Period => switch (c) {
                    '.' => {
                        state = .Period2;
                    },
                    else => {
                        result.id = .Period;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .Period2 => switch (c) {
                    '.' => {
                        result.id = .Ellipsis;
                        break;
                    },
                    else => {
                        try parser.err_stream.print("invalid escape sequence\n", .{});
                        return error.Invalid;
                    },
                },
                .Minus => switch (c) {
                    '=' => {
                        result.id = .MinusEqual;
                        break;
                    },
                    else => {
                        result.id = .Minus;
                        parser.it.index = start_index + 2;
                        break;
                    },
                },
                .Slash => switch (c) {
                    '/' => {
                        state = .SlashSlash;
                    },
                    '=' => {
                        result.id = .SlashEqual;
                        break;
                    },
                    else => {
                        result.id = .Slash;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .SlashSlash => switch (c) {
                    '=' => {
                        result.id = .SlashSlashEqual;
                        break;
                    },
                    else => {
                        result.id = .SlashSlash;
                        parser.it.index = start_index + 2;
                        break;
                    },
                },
                .Ampersand => switch (c) {
                    '=' => {
                        result.id = .AmpersandEqual;
                        break;
                    },
                    else => {
                        result.id = .Ampersand;
                        parser.it.index = start_index + 1;
                        break;
                    },
                },
                .LineComment => switch (c) {
                    '\n', '\r' => {
                        parser.it.index -= 1;
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
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        try parser.err_stream.print("octal literals start with '0o'\n", .{});
                        return error.Invalid;
                    },
                    '_' => {
                        state = .Number;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
                .BinaryNumber => switch (c) {
                    '0', '1', '_' => {},
                    '2'...'9', 'a'...'f', 'A'...'F' => {
                        try parser.err_stream.print("invalid digit in octal literal\n", .{});
                        return error.Invalid;
                    },
                    '.' => {
                        try parser.err_stream.print("invalid base for floating point number\n", .{});
                        return error.Invalid;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
                .OctalNumber => switch (c) {
                    '0'...'7', '_' => {},
                    '8'...'9', 'a'...'f', 'A'...'F' => {
                        try parser.err_stream.print("invalid digit in octal literal\n", .{});
                        return error.Invalid;
                    },
                    '.' => {
                        try parser.err_stream.print("invalid base for floating point number\n", .{});
                        return error.Invalid;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
                .HexNumber => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    '.' => {
                        state = .FloatFractionHex;
                    },
                    'p', 'P' => {
                        state = .FloatExponent;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
                .Number => switch (c) {
                    '0'...'9', '_' => {},
                    'a'...'d', 'f', 'A'...'F' => {
                        try parser.err_stream.print("invalid digit in octal literal\n", .{});
                        return error.Invalid;
                    },
                    '.' => {
                        state = .FloatFraction;
                    },
                    'e' => {
                        state = .FloatExponent;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
                .FloatFraction => switch (c) {
                    '0'...'9', '_' => {},
                    'e' => {
                        state = .FloatExponent;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
                .FloatExponent => switch (c) {
                    '+', '-' => {
                        state = .FloatExponentDigits;
                    },
                    else => {
                        parser.it.index = start_index - 1; // TODO
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
                            try parser.err_stream.print("invalid exponent\n", .{});
                            return error.Invalid;
                        }
                        parser.it.index = start_index - 1; // TODO
                        result.id = .{ .Number = parser.it.buffer[start_index..parser.it.index] };
                    },
                },
            }
        } else {
            switch (state) {
                .LineComment, .Start => {},
                .Identifier => {
                    const slice = parser.it.buffer[start_index..];
                    result.id = Token.getKeyword(slice) orelse .{ .Identifier = slice };
                },

                .Cr, .BackSlash, .BackSlashCr, .Period2, .StringLiteral, .EscapeSequence, .CrEscape, .HexEscape, .UnicodeStart, .UnicodeEscape, .UnicodeEnd, .FloatFraction, .FloatExponent, .FloatExponentDigits, .Bang => {
                    try parser.err_stream.print("unexpected eof\n", .{});
                    return error.Invalid;
                },

                .BinaryNumber,
                .OctalNumber,
                .HexNumber,
                .Number,
                .Zero,
                => result.id = .{ .Number = parser.it.buffer[start_index..] },

                .Equal => result.id = .Equal,
                .Minus => result.id = .Minus,
                .Slash => result.id = .Slash,
                .SlashSlash => result.id = .SlashSlash,
                .Ampersand => result.id = .Ampersand,
                .Period => result.id = .Period,
                .Pipe => result.id = .Pipe,
                .RArr => result.id = .RArr,
                .RArrArr => result.id = .RArrArr,
                .LArr => result.id = .LArr,
                .LArrArr => result.id = .LArrArr,
                .Plus => result.id = .Plus,
                .Percent => result.id = .Percent,
                .Caret => result.id = .Caret,
                .Asterisk => result.id = .Asterisk,
            }
        }
        return result;
    }
};
