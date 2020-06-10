const std = @import("std");
const Allocator = std.mem.Allocator;

const bog = @import("bog.zig");
const Token = bog.Token;
const TokenIndex = Token.Index;

pub const Tree = struct {
    tokens: []const Token,
    nodes: []const *Node,

    /// not owned by the tree
    source: []const u8,

    arena: std.heap.ArenaAllocator.State,
    gpa: *Allocator,

    pub fn deinit(self: *Tree) void {
        self.gpa.free(self.tokens);
        self.arena.promote(self.gpa).deinit();
    }

    pub const render = @import("render.zig").render;
};

pub const Node = struct {
    id: Id,

    pub const Id = enum {
        Decl,
        Fn,
        Discard,
        Identifier,
        Primitive,
        This,
        Prefix,
        Infix,
        Suffix,
        Literal,
        Import,
        Native,
        Error,
        List,
        Tuple,
        Map,
        Block,
        Grouped,
        ListType,
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
        TypeDecl,
        Union,
        Case,
        Param,
    };

    pub fn firstToken(node: *Node) TokenIndex {
        return switch (node.id) {
            .Decl => @fieldParentPtr(Node.Decl, "base", node).let_const,
            .TypeDecl => @fieldParentPtr(Node.TypeDecl, "base", node).tok,
            .Fn => @fieldParentPtr(Node.Fn, "base", node).fn_tok,
            .Param => {
                const param = @fieldParentPtr(Node.Union, "base", node);
                return param.capture.firstToken();
            },
            .Case => {
                const case = @fieldParentPtr(Node.Case, "base", node);
                if (case.pipe) |some| return some;
                if (case.name) |some| return some;
                return case.expr.firstToken();
            },
            .Primitive, .Identifier, .Discard, .This => @fieldParentPtr(Node.SingleToken, "base", node).tok,
            .Prefix => @fieldParentPtr(Node.Prefix, "base", node).tok,
            .Infix => @fieldParentPtr(Node.Infix, "base", node).lhs.firstToken(),
            .Suffix => @fieldParentPtr(Node.Suffix, "base", node).l_tok,
            .Literal => {
                const lit = @fieldParentPtr(Node.Literal, "base", node);
                // TODO: cannot use -1
                return if (lit.kind != .none) lit.tok else lit.tok - 1;
            },
            .Import => @fieldParentPtr(Node.Import, "base", node).tok,
            .Native => @fieldParentPtr(Node.Native, "base", node).tok,
            .Error => @fieldParentPtr(Node.Error, "base", node).tok,
            .List, .Tuple, .Map => @fieldParentPtr(Node.ListTupleMap, "base", node).l_tok,
            .Union, .Block => @fieldParentPtr(Node.Block, "base", node).stmts[0].firstToken(),
            .ListType, .Grouped => @fieldParentPtr(Node.Grouped, "base", node).l_tok,
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
            .MatchCase => @fieldParentPtr(Node.MatchCase, "base", node).lhs[0].firstToken(),
            .Jump => @fieldParentPtr(Node.Jump, "base", node).tok,
        };
    }

    pub fn lastToken(node: *Node) TokenIndex {
        return switch (node.id) {
            .Decl => @fieldParentPtr(Node.Decl, "base", node).value.lastToken(),
            .TypeDecl => @fieldParentPtr(Node.TypeDecl, "base", node).expr.lastToken(),
            .Fn => {
                const fn_node = @fieldParentPtr(Node.Fn, "base", node);
                if (fn_node.body) |some| return some.lastToken();
                if (fn_node.ret_type) |some| return some.lastToken();
                return fn_node.r_paren;
            },
            .Param => {
                const param = @fieldParentPtr(Node.Param, "base", node);
                if (param.param_type) |some|  return some.lastToken();
                return param.capture.lastToken();
            },
            .Case => return @fieldParentPtr(Node.Case, "base", node).expr.lastToken(),
            .Primitive, .Identifier, .Discard, .This => @fieldParentPtr(Node.SingleToken, "base", node).tok,
            .Prefix => @fieldParentPtr(Node.Prefix, "base", node).rhs.lastToken(),
            .Infix => @fieldParentPtr(Node.Infix, "base", node).rhs.lastToken(),
            .Suffix => @fieldParentPtr(Node.Suffix, "base", node).r_tok,
            .Literal => @fieldParentPtr(Node.Literal, "base", node).tok,
            .Import => @fieldParentPtr(Node.Import, "base", node).r_paren,
            .Native => @fieldParentPtr(Node.Native, "base", node).r_paren,
            .Error => @fieldParentPtr(Node.Error, "base", node).r_paren,
            .List, .Tuple, .Map => @fieldParentPtr(Node.ListTupleMap, "base", node).r_tok,
            .Union, .Block => {
                const block = @fieldParentPtr(Node.Block, "base", node);
                return block.stmts[block.stmts.len - 1].lastToken();
            },
            .ListType, .Grouped => @fieldParentPtr(Node.Grouped, "base", node).r_tok,
            .MapItem => @fieldParentPtr(Node.MapItem, "base", node).value.lastToken(),
            .Catch => @fieldParentPtr(Node.Catch, "base", node).rhs.lastToken(),
            .If => {
                const if_node = @fieldParentPtr(Node.If, "base", node);
                if (if_node.else_body) |some| return some.lastToken();
                return if_node.if_body.lastToken();
            },
            .For => @fieldParentPtr(Node.For, "base", node).body.lastToken(),
            .While => @fieldParentPtr(Node.While, "base", node).body.lastToken(),
            .Match => {
                const match = @fieldParentPtr(Node.Match, "base", node);
                return match.cases[match.cases.len - 1].lastToken();
            },
            .MatchCatchAll => @fieldParentPtr(Node.MatchCatchAll, "base", node).expr.lastToken(),
            .MatchLet => @fieldParentPtr(Node.MatchLet, "base", node).expr.lastToken(),
            .MatchCase => @fieldParentPtr(Node.MatchCase, "base", node).expr.lastToken(),
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);
                switch (jump.op) {
                    .Return => |n| return if (n) |some| some.lastToken() else jump.tok,
                    .Break, .Continue => return jump.tok,
                }
            },
        };
    }

    pub const Decl = struct {
        base: Node = Node{ .id = .Decl },
        capture: *Node,
        value: *Node,
        type_expr: ?*Node,
        let_const: TokenIndex,
        eq_tok: TokenIndex,
    };

    pub const TypeDecl = struct {
        base: Node = Node{ .id = .TypeDecl },
        tok: TokenIndex,
        name: TokenIndex,
        expr: *Node,
    };

    pub const Case = struct {
        base: Node = Node{ .id = .Case },
        pipe: ?TokenIndex,
        name: ?TokenIndex,
        expr: *Node,
    };

    pub const Fn = struct {
        base: Node = Node{ .id = .Fn },
        params: []*Node,
        body: ?*Node,
        fn_tok: TokenIndex,
        r_paren: TokenIndex,
        ret_type: ?*Node,
    };

    pub const Param = struct {
        base: Node = Node{ .id = .Param },
        capture: *Node,
        param_type: ?*Node,
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
            boolNot,
            bitNot,
            minus,
            plus,
            Try,
        };
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
            RShiftAssign,
            BitAndAssign,
            BitOrAssign,
            BitXOrAssign,

            is,
            as,
        },
        tok: TokenIndex,
        lhs: *Node,
        rhs: *Node,
    };

    pub const Suffix = struct {
        base: Node = Node{ .id = .Suffix },
        lhs: *Node,
        op: union(enum) {
            call: []*Node,
            subscript: *Node,
            member,
        },
        l_tok: TokenIndex,
        r_tok: TokenIndex,
    };

    pub const Literal = struct {
        base: Node = Node{ .id = .Literal },
        kind: enum {
            str,
            num,
            int,
            True,
            False,
            none,
        },
        tok: TokenIndex,
    };

    pub const Import = struct {
        base: Node = Node{ .id = .Import },
        tok: TokenIndex,
        str_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const Native = struct {
        base: Node = Node{ .id = .Native },
        tok: TokenIndex,
        str_tok: TokenIndex,
        r_paren: TokenIndex,
        type_expr: ?*Node,
    };

    pub const Error = struct {
        base: Node = Node{ .id = .Error },
        tok: TokenIndex,
        value: *Node,
        r_paren: TokenIndex,
    };

    pub const ListTupleMap = struct {
        base: Node,
        values: []*Node,
        l_tok: TokenIndex,
        r_tok: TokenIndex,
    };

    pub const Block = struct {
        base: Node,
        stmts: []*Node,
    };

    pub const Grouped = struct {
        base: Node,
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
        let_const: ?TokenIndex,
    };

    pub const If = struct {
        base: Node = Node{ .id = .If },
        cond: *Node,
        if_body: *Node,
        else_body: ?*Node,
        capture: ?*Node,
        let_const: ?TokenIndex,
        eq_tok: ?TokenIndex,
        else_tok: ?TokenIndex,
        if_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const For = struct {
        base: Node = Node{ .id = .For },
        capture: ?*Node,
        cond: *Node,
        body: *Node,
        let_const: ?TokenIndex,
        in_tok: ?TokenIndex,
        for_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const While = struct {
        base: Node = Node{ .id = .While },
        cond: *Node,
        body: *Node,
        capture: ?*Node,
        let_const: ?TokenIndex,
        eq_tok: ?TokenIndex,
        while_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const Match = struct {
        base: Node = Node{ .id = .Match },
        expr: *Node,
        cases: []*Node,
        match_tok: TokenIndex,
        r_paren: TokenIndex,
    };

    pub const MatchCatchAll = struct {
        base: Node = Node{ .id = .MatchCatchAll },
        expr: *Node,
        tok: TokenIndex,
        colon: TokenIndex,
    };

    pub const MatchLet = struct {
        base: Node = Node{ .id = .MatchLet },
        capture: *Node,
        expr: *Node,
        let_const: TokenIndex,
        colon: TokenIndex,
    };

    pub const MatchCase = struct {
        base: Node = Node{ .id = .MatchCase },
        lhs: []*Node,
        expr: *Node,
        colon: TokenIndex,
    };

    pub const Jump = struct {
        base: Node = Node{ .id = .Jump },
        tok: TokenIndex,
        op: union(enum) {
            Break,
            Continue,
            Return: ?*Node,
        },
    };
};
