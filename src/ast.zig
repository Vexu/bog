const std = @import("std");
const TokenIndex = @import("tokenizer.zig").TokenIndex;
const TypeId = @import("value.zig").TypeId;

pub const NodeList = std.SegmentedList(*Node, 4);

pub const Node = struct {
    id: Id,

    pub const Id = enum {
        Let,
        Fn,
        Unwrap,
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

    pub const Let = struct {
        base: Node = Node{ .id = .Let },
        unwrap: *Node,
        body: *Node,
        let_tok: TokenIndex,
        eq_tok: TokenIndex,
    };

    pub const Fn = struct {
        base: Node = Node{ .id = .Fn },
        params: NodeList,
        body: *Node,
        fn_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const Unwrap = struct {
        base: Node = Node{ .id = .Unwrap },
        op: union(enum) {
            Error: *Node,
            Map: NodeList,
            List: ListTuple,
            Tuple: ListTuple,
        },
        const ListTuple = struct {
            from: enum {
                Front,
                Back,
                All,
            },
            unwraps: NodeList,
        };
        l_tok: TokenIndex,
        r_tok: TokenIndex,
    };

    pub const SingleToken = struct {
        base: Node,
        tok: TokenIndex,
    };

    pub const Prefix = struct {
        base: Node = Node{ .id = .Prefix },
        op: enum {
            BoolNot,
            BitNot,
            Minus,
            Plus,
            Try,
        },
        tok: TokenIndex,
        rhs: *Node,
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
            Call: NodeList,
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
            Bool,
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
        values: NodeList,
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
        unwrap: ?*Node,
        colon: ?TokenIndex,
        rhs: *Node,
    };

    pub const If = struct {
        base: Node = Node{ .id = .If },
        if_tok: TokenIndex,
        cond: *Node,
        if_body: *Node,
        else_body: ?*Node,
        else_tok: ?TokenIndex,
        r_paren: TokenIndex,
    };

    pub const For = struct {
        base: Node = Node{ .id = .For },
        unwrap: ?*Node,
        cond: *Node,
        body: *Node,
        in_tok: ?TokenIndex,
        for_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const While = struct {
        base: Node = Node{ .id = .While },
        cond: *Node,
        body: *Node,
        while_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const Match = struct {
        base: Node = Node{ .id = .Match },
        expr: *Node,
        body: NodeList,
        match_tok: TokenIndex,
        r_paren: TokenIndex,
        body_r_brace: TokenIndex,
    };

    pub const MatchCatchAll = struct {
        base: Node = Node{ .id = .MatchCatchAll },
        tok: TokenIndex,
        expr: *Node,
    };

    pub const MatchLet = struct {
        base: Node = Node{ .id = .MatchLet },
        unwrap: *Node.Unwrap,
        expr: *Node,
    };

    pub const MatchCase = struct {
        base: Node = Node{ .id = .MatchCase },
        lhs: NodeList,
        expr: *Node,
        colon: TokenIndex,
    };

    pub const Jump = struct {
        base: Node = Node{ .id = .Jump },
        tok: TokenIndex,
        op: union(enum) {
            Break,
            Continue,
            Return: *Node,
        },
    };
};
