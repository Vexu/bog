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
        eof,
        indent: u16,
        identifier,
        string,
        integer,
        number,
        nl,
        pipe,
        pipe_equal,
        equal,
        equal_equal,
        bang_equal,
        l_paren,
        r_paren,
        percent,
        percent_equal,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        period,
        equal_rarr,
        caret,
        caret_equal,
        plus,
        plus_equal,
        plus_plus,
        minus,
        minus_equal,
        asterisk,
        asterisk_equal,
        asterisk_asterisk,
        asterisk_asterisk_equal,
        slash,
        slash_equal,
        slash_slash,
        slash_slash_equal,
        comma,
        ampersand,
        ampersand_equal,
        l_arr,
        l_arr_equal,
        l_arr_arr,
        l_arr_arr_equal,
        r_arr,
        r_arr_equal,
        r_arr_arr,
        r_arr_arr_equal,
        tilde,
        colon,
        underscore,
        at,
        format_start,
        format,
        format_end,

        /// keywords
        keyword_and,
        keyword_any,
        keyword_as,
        keyword_break,
        keyword_bool,
        keyword_catch,
        keyword_continue,
        keyword_else,
        keyword_enum,
        keyword_error,
        keyword_export,
        keyword_false,
        keyword_fn,
        keyword_for,
        keyword_if,
        keyword_import,
        keyword_in,
        keyword_int,
        keyword_is,
        keyword_let,
        keyword_match,
        keyword_mut,
        keyword_never,
        keyword_null,
        keyword_num,
        keyword_none,
        keyword_not,
        keyword_or,
        keyword_range,
        keyword_return,
        keyword_str,
        keyword_this,
        keyword_throw,
        keyword_true,
        keyword_try,
        keyword_type,
        keyword_while,
    };

    pub const keywords = std.ComptimeStringMap(Id, .{
        .{ "and", .keyword_and },
        .{ "any", .keyword_any },
        .{ "as", .keyword_as },
        .{ "break", .keyword_break },
        .{ "bool", .keyword_bool },
        .{ "catch", .keyword_catch },
        .{ "continue", .keyword_continue },
        .{ "else", .keyword_else },
        .{ "enum", .keyword_enum },
        .{ "error", .keyword_error },
        .{ "export", .keyword_export },
        .{ "false", .keyword_false },
        .{ "fn", .keyword_fn },
        .{ "for", .keyword_for },
        .{ "if", .keyword_if },
        .{ "import", .keyword_import },
        .{ "in", .keyword_in },
        .{ "int", .keyword_int },
        .{ "is", .keyword_is },
        .{ "let", .keyword_let },
        .{ "match", .keyword_match },
        .{ "mut", .keyword_mut },
        .{ "never", .keyword_never },
        .{ "null", .keyword_null },
        .{ "num", .keyword_num },
        .{ "none", .keyword_none },
        .{ "not", .keyword_not },
        .{ "or", .keyword_or },
        .{ "range", .keyword_range },
        .{ "return", .keyword_return },
        .{ "str", .keyword_str },
        .{ "this", .keyword_this },
        .{ "throw", .keyword_throw },
        .{ "true", .keyword_true },
        .{ "try", .keyword_try },
        .{ "type", .keyword_type },
        .{ "while", .keyword_while },
        .{ "_", .underscore },
    });

    pub fn string(id: std.meta.Tag(Id)) []const u8 {
        return switch (id) {
            .eof => "<EOF>",
            .nl => "<NL>",
            .indent => "<INDENT>",
            .identifier => "Identifier",
            .string => "String",
            .integer => "Integer",
            .number => "Number",
            .pipe => "|",
            .pipe_equal => "|=",
            .equal => "=",
            .equal_equal => "==",
            .bang_equal => "!=",
            .l_paren => "(",
            .r_paren => ")",
            .percent => "%",
            .percent_equal => "%=",
            .l_brace => "{",
            .r_brace => "}",
            .l_bracket => "[",
            .r_bracket => "]",
            .period => ".",
            .equal_rarr => "=>",
            .caret => "^",
            .caret_equal => "^=",
            .plus => "+",
            .plus_equal => "+=",
            .plus_plus => "++",
            .minus => "-",
            .minus_equal => "-=",
            .asterisk => "*",
            .asterisk_equal => "*=",
            .asterisk_asterisk => "**",
            .asterisk_asterisk_equal => "**=",
            .slash => "/",
            .slash_equal => "/=",
            .slash_slash => "//",
            .slash_slash_equal => "//=",
            .comma => ",",
            .ampersand => "&",
            .ampersand_equal => "&=",
            .l_arr => "<",
            .l_arr_equal => "<=",
            .l_arr_arr => "<<",
            .l_arr_arr_equal => "<<=",
            .r_arr => ">",
            .r_arr_equal => ">=",
            .r_arr_arr => ">>",
            .r_arr_arr_equal => ">>=",
            .tilde => "~",
            .colon => ":",
            .underscore => "_",
            .at => "@",
            .format_start, .format, .format_end => "Format string",

            .keyword_and => "and",
            .keyword_any => "any",
            .keyword_as => "as",
            .keyword_break => "break",
            .keyword_bool => "bool",
            .keyword_catch => "catch",
            .keyword_continue => "continue",
            .keyword_else => "else",
            .keyword_enum => "enum",
            .keyword_error => "error",
            .keyword_export => "export",
            .keyword_false => "false",
            .keyword_fn => "fn",
            .keyword_for => "for",
            .keyword_if => "if",
            .keyword_import => "import",
            .keyword_in => "in",
            .keyword_int => "int",
            .keyword_is => "is",
            .keyword_let => "let",
            .keyword_match => "match",
            .keyword_mut => "mut",
            .keyword_never => "never",
            .keyword_null => "null",
            .keyword_num => "num",
            .keyword_none => "none",
            .keyword_not => "not",
            .keyword_or => "or",
            .keyword_range => "range",
            .keyword_return => "return",
            .keyword_str => "str",
            .keyword_this => "this",
            .keyword_throw => "throw",
            .keyword_true => "true",
            .keyword_try => "try",
            .keyword_type => "type",
            .keyword_while => "while",
        };
    }
};

pub fn tokenize(allocator: mem.Allocator, source: []const u8, errors: *Errors) Tokenizer.Error![]const Token {
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
        if (tok.id == .eof) {
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
        if (tok.id == .eof) {
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
                        .identifier,
                        .keyword_this,
                        .string,
                        .integer,
                        .number,
                        .r_brace,
                        .r_bracket,
                        .underscore,
                        .keyword_break,
                        .keyword_continue,
                        .keyword_false,
                        .keyword_true,
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
            .id = .{ .indent = level },
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
            start,
            string,
            escape_sequence,
            hex_escape,
            unicode_start,
            unicode_escape,
            unicode_end,
            identifier,
            equal,
            bang,
            pipe,
            percent,
            asterisk,
            asterisk_asterisk,
            plus,
            l_arr,
            l_arr_arr,
            r_arr,
            r_arr_arr,
            caret,
            minus,
            slash,
            slash_slash,
            ampersand,
            line_comment,
            binary_number,
            octal_number,
            hex_number,
            number,
            zero,
            number_dot,
            float_fraction,
            float_exponent,
            float_exponent_digits,
            f,
            format_string,
            format_brace,
        } = .start;
        var res: Token.Id = .eof;
        var str_delimit: u32 = undefined;
        var counter: u32 = 0;
        var dot_index: ?usize = null;
        var escape_end_state: @TypeOf(state) = .string;

        while (self.it.nextCodepoint()) |c| {
            switch (state) {
                .start => switch (c) {
                    '#' => state = .line_comment,
                    '\n', ';' => {
                        res = .nl;
                        self.expect_indent = true;
                        break;
                    },
                    '"', '\'' => {
                        self.string = true;
                        str_delimit = c;
                        state = .string;
                    },
                    '=' => state = .equal,
                    '!' => state = .bang,
                    '|' => state = .pipe,
                    '(' => {
                        self.paren_level += 1;
                        res = .l_paren;
                        break;
                    },
                    ')' => {
                        if (self.paren_level == 0) {
                            return self.reportErr("unmatched ')'", c);
                        }
                        self.paren_level -= 1;
                        res = .r_paren;
                        break;
                    },
                    '[' => {
                        self.paren_level += 1;
                        res = .l_bracket;
                        break;
                    },
                    ']' => {
                        if (self.paren_level == 0) {
                            return self.reportErr("unmatched ']'", c);
                        }
                        self.paren_level -= 1;
                        res = .r_bracket;
                        break;
                    },
                    ',' => {
                        res = .comma;
                        break;
                    },
                    '@' => {
                        res = .at;
                        break;
                    },
                    '%' => state = .percent,
                    '*' => state = .asterisk,
                    '+' => state = .plus,
                    '<' => state = .l_arr,
                    '>' => state = .r_arr,
                    '^' => state = .caret,
                    '{' => {
                        self.paren_level += 1;
                        res = .l_brace;
                        break;
                    },
                    '}' => {
                        if (self.format_string != 0 and self.format_paren_level == self.paren_level) {
                            escape_end_state = .format_string;
                            state = .format_string;
                            res = .format;
                            continue;
                        }
                        if (self.paren_level == 0) {
                            return self.reportErr("unmatched '}'", c);
                        }
                        self.paren_level -= 1;
                        res = .r_brace;
                        break;
                    },
                    '~' => {
                        res = .tilde;
                        break;
                    },
                    ':' => {
                        if (self.format_string != 0 and self.format_paren_level == self.paren_level) {
                            escape_end_state = .format_string;
                            state = .format_string;
                            res = .format;
                            continue;
                        }
                        res = .colon;
                        break;
                    },
                    '.' => {
                        res = .period;
                        break;
                    },
                    '-' => state = .minus,
                    '/' => state = .slash,
                    '&' => state = .ampersand,
                    '0' => state = .zero,
                    '1'...'9' => state = .number,
                    'f' => if (self.format_string != 0) {
                        state = .identifier;
                    } else {
                        state = .f;
                    },
                    else => if (isWhiteSpace(c)) {
                        start_index = self.it.i;
                    } else if (isIdentifier(c)) {
                        state = .identifier;
                    } else {
                        return self.reportErr("invalid character", c);
                    },
                },
                .string => switch (c) {
                    '\\' => state = .escape_sequence,
                    '\n', '\r' => if (str_delimit == '\'') {
                        return self.reportErr("invalid newline, use'\"' for multiline strings", c);
                    },
                    else => if (c == str_delimit) {
                        self.string = false;
                        res = .string;
                        break;
                    },
                },
                .escape_sequence => switch (c) {
                    '\'', '"', '\\', 'r', 't', 'n' => state = escape_end_state,
                    'x' => {
                        counter = 0;
                        state = .hex_escape;
                    },
                    'u' => state = .unicode_start,
                    else => return self.reportErr("invalid escape sequence", c),
                },
                .hex_escape => switch (c) {
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
                .unicode_start => if (c == '{') {
                    counter = 0;
                    state = .unicode_escape;
                } else {
                    return self.reportErr("expected '{' after '\\u'", c);
                },
                .unicode_escape => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F' => {
                        counter += 1;
                        if (counter > 6) {
                            state = .unicode_end;
                        }
                    },
                    '}' => state = escape_end_state,
                    else => return self.reportErr("expected hex digits or '}'", c),
                },
                .unicode_end => if (c == '}') {
                    state = escape_end_state;
                } else {
                    return self.reportErr("expected '}'", c);
                },
                .f => switch (c) {
                    '\'' => {
                        self.format_string = '\'';
                        self.format_paren_level = self.paren_level;
                        res = .format_start;
                        state = .format_string;
                        escape_end_state = .format_string;
                    },
                    '"' => {
                        self.format_string = '"';
                        self.format_paren_level = self.paren_level;
                        res = .format_start;
                        state = .format_string;
                        escape_end_state = .format_string;
                    },
                    else => {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        state = .identifier;
                    },
                },
                .format_string => {
                    if (c == self.format_string) {
                        if (res == .format_start) {
                            res = .string;
                        } else {
                            res = .format_end;
                        }
                        self.format_string = 0;
                        break;
                    } else if (c == '\\') {
                        state = .escape_sequence;
                    } else if (c == '\n' or c == '\r') {
                        if (self.format_string == '\'') {
                            return self.reportErr("invalid newline, use'\"' for multiline strings", c);
                        }
                    } else if (c == '{') {
                        state = .format_brace;
                    }
                },
                .format_brace => {
                    if (c == '{') {
                        state = .format_string;
                    } else {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        break;
                    }
                },
                .identifier => {
                    if (!isIdentifier(c)) {
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        const slice = self.it.bytes[start_index..self.it.i];
                        res = Token.keywords.get(slice) orelse .identifier;
                        break;
                    }
                },
                .equal => switch (c) {
                    '=' => {
                        res = .equal_equal;
                        break;
                    },
                    '>' => {
                        res = .equal_rarr;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .equal;
                        break;
                    },
                },
                .bang => switch (c) {
                    '=' => {
                        res = .bang_equal;
                        break;
                    },
                    else => {
                        return self.reportErr("invalid character, use 'not' for boolean not", c);
                    },
                },
                .pipe => switch (c) {
                    '=' => {
                        res = .pipe_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .pipe;
                        break;
                    },
                },
                .percent => switch (c) {
                    '=' => {
                        res = .percent_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .percent;
                        break;
                    },
                },
                .asterisk => switch (c) {
                    '=' => {
                        res = .asterisk_equal;
                        break;
                    },
                    '*' => state = .asterisk_asterisk,
                    else => {
                        self.it.i = start_index + 1;
                        res = .asterisk;
                        break;
                    },
                },
                .asterisk_asterisk => switch (c) {
                    '=' => {
                        res = .asterisk_asterisk_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .asterisk_asterisk;
                        break;
                    },
                },
                .plus => switch (c) {
                    '=' => {
                        res = .plus_equal;
                        break;
                    },
                    '+' => {
                        res = .plus_plus;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .plus;
                        break;
                    },
                },
                .l_arr => switch (c) {
                    '<' => state = .l_arr_arr,
                    '=' => {
                        res = .l_arr_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .l_arr;
                        break;
                    },
                },
                .l_arr_arr => switch (c) {
                    '=' => {
                        res = .l_arr_arr_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .l_arr_arr;
                        break;
                    },
                },
                .r_arr => switch (c) {
                    '>' => state = .r_arr_arr,
                    '=' => {
                        res = .r_arr_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .r_arr;
                        break;
                    },
                },
                .r_arr_arr => switch (c) {
                    '=' => {
                        res = .r_arr_arr_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .r_arr_arr;
                        break;
                    },
                },
                .caret => switch (c) {
                    '=' => {
                        res = .caret_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .caret;
                        break;
                    },
                },
                .minus => switch (c) {
                    '=' => {
                        res = .minus_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .minus;
                        break;
                    },
                },
                .slash => switch (c) {
                    '/' => state = .slash_slash,
                    '=' => {
                        res = .slash_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .slash;
                        break;
                    },
                },
                .slash_slash => switch (c) {
                    '=' => {
                        res = .slash_slash_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 2;
                        res = .slash_slash;
                        break;
                    },
                },
                .ampersand => switch (c) {
                    '=' => {
                        res = .ampersand_equal;
                        break;
                    },
                    else => {
                        self.it.i = start_index + 1;
                        res = .ampersand;
                        break;
                    },
                },
                .line_comment => switch (c) {
                    '\n' => state = .start,
                    else => {},
                },
                .zero => switch (c) {
                    'b' => state = .binary_number,
                    'o' => state = .octal_number,
                    'x' => state = .hex_number,
                    '.', ',' => {
                        state = .number_dot;
                        dot_index = self.it.i - 1;
                    },
                    '0'...'7' => {
                        return self.reportErr("invalid character, octal literals start with '0o'", c);
                    },
                    '_' => state = .number,
                    else => {
                        if (isIdentifier(c)) {
                            state = .identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .integer;
                        break;
                    },
                },
                .number_dot => switch (c) {
                    '.' => {
                        self.it.i -= 2;
                        res = .integer;
                        break;
                    },
                    '0'...'9', 'e', 'E', '_' => {
                        self.it.i -= 1;
                        state = .float_fraction;
                    },
                    else => {
                        self.it.i = dot_index.?;
                        res = .integer;
                        break;
                    },
                },
                .binary_number => switch (c) {
                    '0', '1', '_' => {},
                    else => {
                        if (isIdentifier(c)) {
                            state = .identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .integer;
                        break;
                    },
                },
                .octal_number => switch (c) {
                    '0'...'7', '_' => {},
                    else => {
                        if (isIdentifier(c)) {
                            state = .identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .integer;
                        break;
                    },
                },
                .hex_number => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    'p', 'P' => state = .float_exponent,
                    else => {
                        if (isIdentifier(c)) {
                            state = .identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .integer;
                        break;
                    },
                },
                .number => switch (c) {
                    '0'...'9', '_' => {},
                    '.' => {
                        state = .number_dot;
                        dot_index = self.it.i - 1;
                    },
                    'e', 'E' => state = .float_exponent,
                    else => {
                        if (isIdentifier(c)) {
                            state = .identifier;
                            continue;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .integer;
                        break;
                    },
                },
                .float_fraction => switch (c) {
                    '0'...'9', '_' => {},
                    'e', 'E' => state = .float_exponent,
                    '.', ',' => {
                        self.it.i -= 1;
                        res = .number;
                        break;
                    },
                    else => {
                        if (isIdentifier(c)) {
                            self.it.i = dot_index orelse {
                                state = .identifier;
                                continue;
                            };
                            res = .integer;
                            break;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        res = .number;
                        break;
                    },
                },
                .float_exponent => switch (c) {
                    '+', '-' => state = .float_exponent_digits,
                    else => {
                        if (isIdentifier(c)) {
                            self.it.i = dot_index orelse {
                                state = .identifier;
                                continue;
                            };
                            res = .integer;
                            break;
                        }
                        self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                        state = .float_exponent_digits;
                    },
                },
                .float_exponent_digits => switch (c) {
                    '0'...'9' => counter += 1,
                    '_' => {},
                    else => {
                        if (counter != 0 and !isIdentifier(c)) {
                            self.it.i -= unicode.utf8CodepointSequenceLength(c) catch unreachable;
                            res = .number;
                            break;
                        }
                        self.it.i = dot_index orelse {
                            self.it.i = start_index;
                            state = .identifier;
                            continue;
                        };
                        res = .integer;
                        break;
                    },
                },
            }
        } else {
            switch (state) {
                .line_comment, .start => {},
                .identifier => {
                    const slice = self.it.bytes[start_index..];
                    res = Token.keywords.get(slice) orelse .identifier;
                },
                .binary_number,
                .octal_number,
                .hex_number,
                .number,
                .zero,
                => res = .integer,
                .format_brace => {},

                .string, .format_string => if (self.repl) {
                    // if running in repl this might be a multiline string
                    self.it.i = start_index;
                    res = .eof;
                } else {
                    return self.reportErr("unterminated string", 'a');
                },
                .float_fraction => res = .number,
                .equal => res = .equal,
                .minus => res = .minus,
                .slash => res = .slash,
                .slash_slash => res = .slash_slash,
                .ampersand => res = .ampersand,
                .pipe => res = .pipe,
                .r_arr => res = .r_arr,
                .r_arr_arr => res = .r_arr_arr,
                .l_arr => res = .l_arr,
                .l_arr_arr => res = .l_arr_arr,
                .plus => res = .plus,
                .percent => res = .percent,
                .caret => res = .caret,
                .asterisk => res = .asterisk,
                .asterisk_asterisk => res = .asterisk_asterisk,
                .number_dot => {
                    self.it.i -= 1;
                    res = .integer;
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
        try std.testing.expect(last_token.id == .eof);
        return;
    }
    errors.render(source, std.io.getStdErr().writer()) catch {};
    return error.TestFailed;
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
    , &.{
        .bang_equal,
        .pipe,
        .pipe_equal,
        .equal,
        .equal_equal,
        .nl,
        .l_paren,
        .r_paren,
        .l_brace,
        .r_brace,
        .l_bracket,
        .r_bracket,
        .period,
        .at,
        .equal_rarr,
        .nl,
        .caret,
        .caret_equal,
        .plus,
        .plus_equal,
        .plus_plus,
        .minus,
        .minus_equal,
        .nl,
        .asterisk,
        .asterisk_equal,
        .asterisk_asterisk,
        .asterisk_asterisk_equal,
        .percent,
        .percent_equal,
        .slash,
        .slash_equal,
        .slash_slash,
        .slash_slash_equal,
        .nl,
        .comma,
        .ampersand,
        .ampersand_equal,
        .l_arr,
        .l_arr_equal,
        .l_arr_arr,
        .nl,
        .l_arr_arr_equal,
        .r_arr,
        .r_arr_equal,
        .r_arr_arr,
        .r_arr_arr_equal,
        .tilde,
        .underscore,
        .nl,
    });
}

test "keywords" {
    try expectTokens(
        \\not　and or let continue break return if else false true for
        \\while match catch try error import is in fn as this
    , &.{
        .keyword_not,
        .keyword_and,
        .keyword_or,
        .keyword_let,
        .keyword_continue,
        .keyword_break,
        .keyword_return,
        .keyword_if,
        .keyword_else,
        .keyword_false,
        .keyword_true,
        .keyword_for,
        .nl,
        .keyword_while,
        .keyword_match,
        .keyword_catch,
        .keyword_try,
        .keyword_error,
        .keyword_import,
        .keyword_is,
        .keyword_in,
        .keyword_fn,
        .keyword_as,
        .keyword_this,
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
    , &.{
        .keyword_if,
        .nl,
        .{ .indent = 1 },
        .keyword_if,
        .nl,
        .{ .indent = 2 },
        .keyword_if,
        .nl,
        .keyword_if,
        .nl,
        .{ .indent = 1 },
        .keyword_if,
        .nl,
        .{ .indent = 1 },
        .keyword_if,
        .nl,
    });
}

test "identifiers" {
    try expectTokens(
        \\0b1gg
        \\0x1gg
        \\0o1gg
        \\0gg
        \\1gg
    , &.{
        .identifier,
        .nl,
        .identifier,
        .nl,
        .identifier,
        .nl,
        .identifier,
        .nl,
        .identifier,
    });
    try expectTokens(
        \\30.30f
        \\30.30ee
        \\30.30e+12a
        \\30.30e+12-
        \\30.30e+-
    , &.{
        .integer,
        .period,
        .identifier,
        .nl,
        .integer,
        .period,
        .identifier,
        .nl,
        .integer,
        .period,
        .identifier,
        .plus,
        .identifier,
        .nl,
        .number,
        .minus,
        .nl,
        .integer,
        .period,
        .identifier,
        .plus,
        .minus,
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
    , &.{
        .integer,
        .period,
        .nl,
        .integer,
        .comma,
        .nl,
        .number,
        .nl,
        .number,
        .nl,
        .number,
        .period,
        .integer,
        .nl,
        .number,
        .comma,
        .integer,
    });
}

test "format string" {
    try expectTokens(
        \\f f"\u{12}{12:12} foo \t\n {f"foo bar" ++ {1:2} as str:3} \x12 " :
    , &.{
        .identifier,
        .format_start,
        .integer,
        .format,
        .identifier,
        .string,
        .plus_plus,
        .l_brace,
        .integer,
        .colon,
        .integer,
        .r_brace,
        .keyword_as,
        .keyword_str,
        .format_end,
        .colon,
    });
}
