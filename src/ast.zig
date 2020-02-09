const std = @import("std");
const Allocator = std.mem.Allocator;
const TypeId = @import("value.zig").TypeId;

const lang = @import("lang.zig");
const Token = lang.Token;
const TokenIndex = Token.Index;

pub const Tree = struct {
    tokens: Token.List,
    nodes: Node.List,
    errors: ErrorMsg.List,
    source: []const u8,
    arena_allocator: std.heap.ArenaAllocator,

    pub fn deinit(tree: *Tree) void {
        var arena = tree.arena_allocator;
        arena.deinit();
    }

    pub const render = @import("render.zig").render;
    pub const compile = lang.Compiler.compile;
    pub const compileRepl = lang.Compiler.compileRepl;
};

pub const ErrorMsg = struct {
    index: u32,
    kind: Kind,

    pub const List = std.SegmentedList(ErrorMsg, 0);

    pub const Kind = enum {
        UnexpectedToken,
        PrimaryExpr,
        TypeName,
        InvalidBaseReal,
        InvalidNum,
        InvalidHex,
        InvalidOctal,
        InvalidBinary,
        InvalidExponent,
        InvalidCharacter,
        InvalidMultilineStr,
        InvalidEscape,
        InvalidNot,
        InvalidOctalStart,
        UnterminatedString,
        UnexpectedEof,
        UnmatchedBracket,
        Redeclaration,
        AssignToConst,
        Undeclared,
        InvalidLval,
        InvalidEmpty,
        ExpectedBoolean,
        ExpectedInt,
        ExpectedNumeric,
        InvalidAugAssign,
    };

    pub fn string(err: ErrorMsg) []const u8 {
        return switch (err.kind) {
            .UnexpectedToken => "unexpected token",
            .PrimaryExpr => "expected Identifier, String, Number, true, false, '(', '{{', '[', error, import, if, while, for, match.",
            .TypeName => "expected type name",
            .InvalidBaseReal => "invalid base for floating point number",
            .InvalidNum => "invalid digit in number",
            .InvalidHex => "invalid digit in hex number",
            .InvalidOctal => "invalid digit in octal number",
            .InvalidBinary => "invalid digit in binary number",
            .InvalidExponent => "invalid exponent digit",
            .InvalidCharacter => "invalid character",
            .InvalidMultilineStr => "invalid newline, use'\"' for multiline strings",
            .InvalidEscape => "invalid escape sequence",
            .InvalidNot => "invalid character, use 'not' for boolean not",
            .InvalidOctalStart => "octal literals start with '0o'",
            .UnterminatedString => "unterminated string",
            .UnexpectedEof => "unexpected EOF",
            .UnmatchedBracket => "unmatched bracket",
            .Redeclaration => "redeclaration of identifier",
            .AssignToConst => "assignment to constant",
            .Undeclared => "use of undeclared identifier",
            .InvalidLval => "invalid left hand side to assignment",
            .InvalidEmpty => "expected a value",
            .ExpectedBoolean => "expected boolean value",
            .ExpectedInt => "expected integer value",
            .ExpectedNumeric => "expected numeric value",
            .InvalidAugAssign => "invalid left hand side to augmented assignment",
        };
    }
};

pub const Node = struct {
    id: Id,

    pub const List = std.SegmentedList(*Node, 4);

    pub const Id = enum {
        Decl,
        Fn,
        Discard,
        Identifier,
        Prefix,
        Infix,
        TypeInfix,
        Suffix,
        Literal,
        Import,
        Error,
        List,
        Tuple,
        Map,
        Block,
        Grouped,
        MapItem,
        Catch,
        If,
        For,
        While,
        Match,
        MatchCatchAll,
        MatchLet,
        MatchCase,
        Jump,
    };

    pub const Decl = struct {
        base: Node = Node{ .id = .Decl },
        capture: *Node,
        value: *Node,
        let_const: TokenIndex,
        eq_tok: TokenIndex,
    };

    pub const Fn = struct {
        base: Node = Node{ .id = .Fn },
        params: List,
        body: *Node,
        fn_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const SingleToken = struct {
        base: Node,
        tok: TokenIndex,
    };

    pub const Prefix = struct {
        base: Node = Node{ .id = .Prefix },
        op: Op,
        tok: TokenIndex,
        rhs: *Node,

        pub const Op = enum {
            BoolNot,
            BitNot,
            Minus,
            Plus,
            Try,
        };
    };

    pub const TypeInfix = struct {
        base: Node = Node{ .id = .TypeInfix },
        op: enum {
            Is,
            As,
        },
        lhs: *Node,
        tok: TokenIndex,
        type_tok: TokenIndex,
    };

    pub const Infix = struct {
        base: Node = Node{ .id = .Infix },
        op: enum {
            BoolOr,
            BoolAnd,
            LessThan,
            LessThanEqual,
            GreaterThan,
            GreaterThanEqual,
            Equal,
            NotEqual,
            In,
            Range,
            BitAnd,
            BitOr,
            BitXor,
            LShift,
            RShift,
            Add,
            Sub,
            Mul,
            Div,
            DivFloor,
            Mod,
            Pow,

            // assignment ops
            Assign,
            AddAssign,
            SubAssign,
            MulAssign,
            PowAssign,
            DivAssign,
            DivFloorAssign,
            ModAssign,
            LShiftAssign,
            RShfitAssign,
            BitAndAssign,
            BitOrAssign,
            BitXOrAssign,
        },
        tok: TokenIndex,
        lhs: *Node,
        rhs: *Node,
    };

    pub const Suffix = struct {
        base: Node = Node{ .id = .Suffix },
        lhs: *Node,
        op: union(enum) {
            Call: List,
            ArrAccess: *Node,
            Member,
        },
        l_tok: TokenIndex,
        r_tok: TokenIndex,
    };

    pub const Literal = struct {
        base: Node = Node{ .id = .Literal },
        kind: enum {
            Str,
            Num,
            Int,
            True,
            False,
            None,
        },
        tok: TokenIndex,
    };

    pub const Import = struct {
        base: Node = Node{ .id = .Import },
        tok: TokenIndex,
        str_tok: TokenIndex,
    };

    pub const Error = struct {
        base: Node = Node{ .id = .Error },
        tok: TokenIndex,
        value: *Node,
        r_paren: TokenIndex,
    };

    pub const ListTupleMapBlock = struct {
        base: Node,
        values: List,
        l_tok: TokenIndex,
        r_tok: TokenIndex,
    };

    pub const Grouped = struct {
        base: Node = Node{ .id = .Grouped },
        expr: *Node,
        l_tok: TokenIndex,
        r_tok: TokenIndex,
    };

    pub const MapItem = struct {
        base: Node = Node{ .id = .MapItem },
        key: ?*Node,
        colon: ?TokenIndex,
        value: *Node,
    };

    pub const Catch = struct {
        base: Node = Node{ .id = .Catch },
        tok: TokenIndex,
        lhs: *Node,
        capture: ?*Node,
        rhs: *Node,
    };

    pub const If = struct {
        base: Node = Node{ .id = .If },
        cond: *Node,
        if_body: *Node,
        else_body: ?*Node,
        capture: ?*Node,
        eq_tok: ?TokenIndex,
        else_tok: ?TokenIndex,
        if_tok: TokenIndex,
    };

    pub const For = struct {
        base: Node = Node{ .id = .For },
        capture: ?*Node,
        cond: *Node,
        body: *Node,
        in_tok: ?TokenIndex,
        for_tok: TokenIndex,
    };

    pub const While = struct {
        base: Node = Node{ .id = .While },
        cond: *Node,
        body: *Node,
        capture: ?*Node,
        eq_tok: ?TokenIndex,
        while_tok: TokenIndex,
    };

    pub const Match = struct {
        base: Node = Node{ .id = .Match },
        expr: *Node,
        body: List,
        match_tok: TokenIndex,
        body_r_brace: TokenIndex,
    };

    pub const MatchCatchAll = struct {
        base: Node = Node{ .id = .MatchCatchAll },
        tok: TokenIndex,
        expr: *Node,
    };

    pub const MatchLet = struct {
        base: Node = Node{ .id = .MatchLet },
        capture: *Node,
        expr: *Node,
    };

    pub const MatchCase = struct {
        base: Node = Node{ .id = .MatchCase },
        lhs: List,
        expr: *Node,
        colon: TokenIndex,
    };

    pub const Jump = struct {
        base: Node = Node{ .id = .Jump },
        tok: TokenIndex,
        op: union(enum) {
            Break: ?*Node,
            Continue,
            Return: ?*Node,
        },
    };
};
