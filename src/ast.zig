const std = @import("std");
const Allocator = std.mem.Allocator;
const TypeId = @import("value.zig").TypeId;

const lang = @import("lang.zig");
const Token = lang.Token;
const TokenIndex = Token.Index;

pub const Tree = struct {
    tokens: Token.List,
    nodes: Node.List,
    errors: lang.Error.List,
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

    pub fn firstToken(node: *Node) TokenIndex {
        return switch (node.id) {
            .Decl => @fieldParentPtr(Node.Decl, "base", node).let_const,
            .Fn => @fieldParentPtr(Node.Fn, "base", node).fn_tok,
            .Identifier, .Discard => @fieldParentPtr(Node.SingleToken, "base", node).tok,
            .Prefix => @fieldParentPtr(Node.Prefix, "base", node).tok,
            .Infix => @fieldParentPtr(Node.Infix, "base", node).lhs.firstToken(),
            .TypeInfix => @fieldParentPtr(Node.TypeInfix, "base", node).lhs.firstToken(),
            .Suffix => @fieldParentPtr(Node.Suffix, "base", node).l_tok,
            .Literal => {
                const lit = @fieldParentPtr(Node.Literal, "base", node);
                return if (lit.kind != .None) lit.tok else lit.tok - 1;
            },
            .Import => @fieldParentPtr(Node.Import, "base", node).tok,
            .Error => @fieldParentPtr(Node.Error, "base", node).tok,
            .List, .Tuple, .Map, .Block => @fieldParentPtr(Node.ListTupleMapBlock, "base", node).l_tok,
            .Grouped => @fieldParentPtr(Node.Grouped, "base", node).l_tok,
            .MapItem => {
                const map = @fieldParentPtr(Node.MapItem, "base", node);

                if (map.key) |some| return some.firstToken();
                return map.value.firstToken();
            },
            .Catch => @fieldParentPtr(Node.Catch, "base", node).lhs.firstToken(),
            .If => @fieldParentPtr(Node.If, "base", node).if_tok,
            .For => @fieldParentPtr(Node.For, "base", node).for_tok,
            .While => @fieldParentPtr(Node.While, "base", node).while_tok,
            .Match => @fieldParentPtr(Node.Match, "base", node).match_tok,
            .MatchCatchAll => @fieldParentPtr(Node.Jump, "base", node).tok,
            .MatchLet => @fieldParentPtr(Node.MatchLet, "base", node).let_const,
            .MatchCase => @fieldParentPtr(Node.MatchCase, "base", node).lhs.at(0).*.firstToken(),
            .Jump => @fieldParentPtr(Node.Jump, "base", node).tok,
        };
    }

    pub fn lastToken(node: *Node) TokenIndex {
        return switch (node.id) {
            .Decl => @fieldParentPtr(Node.Decl, "base", node).value.lastToken(),
            .Fn => @fieldParentPtr(Node.Fn, "base", node).body.lastToken(),
            .Identifier, .Discard => @fieldParentPtr(Node.SingleToken, "base", node).tok,
            .Prefix => @fieldParentPtr(Node.Prefix, "base", node).rhs.lastToken(),
            .Infix => @fieldParentPtr(Node.Infix, "base", node).rhs.lastToken(),
            .TypeInfix => @fieldParentPtr(Node.TypeInfix, "base", node).type_tok,
            .Suffix => @fieldParentPtr(Node.Suffix, "base", node).r_tok,
            .Literal => @fieldParentPtr(Node.Literal, "base", node).tok,
            .Import => @fieldParentPtr(Node.Import, "base", node).str_tok + 1,
            .Error => @fieldParentPtr(Node.Error, "base", node).r_paren,
            .List, .Tuple, .Map, .Block => @fieldParentPtr(Node.ListTupleMapBlock, "base", node).r_tok,
            .Grouped => @fieldParentPtr(Node.Grouped, "base", node).r_tok,
            .MapItem => @fieldParentPtr(Node.MapItem, "base", node).value.lastToken(),
            .Catch => @fieldParentPtr(Node.Catch, "base", node).rhs.lastToken(),
            .If => {
                const if_node = @fieldParentPtr(Node.If, "base", node);
                if (if_node.else_body) |some| return some.lastToken();
                return if_node.if_body.lastToken();
            },
            .For => @fieldParentPtr(Node.For, "base", node).body.lastToken(),
            .While => @fieldParentPtr(Node.While, "base", node).body.lastToken(),
            .Match => @fieldParentPtr(Node.Match, "base", node).body_r_brace,
            .MatchCatchAll => @fieldParentPtr(Node.MatchCatchAll, "base", node).expr.lastToken(),
            .MatchLet => @fieldParentPtr(Node.MatchLet, "base", node).expr.lastToken(),
            .MatchCase => @fieldParentPtr(Node.MatchCase, "base", node).expr.lastToken(),
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);
                switch (jump.op) {
                    .Break, .Return => |n| return if (n) |some| some.lastToken() else jump.tok,
                    .Continue => return jump.tok,
                }
            },
        };
    }

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
        body_l_brace: TokenIndex,
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
        let_const: TokenIndex,
    };

    pub const MatchCase = struct {
        base: Node = Node{ .id = .MatchCase },
        lhs: List,
        expr: *Node,
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
