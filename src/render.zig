const std = @import("std");
const bog = @import("bog.zig");
const Tree = bog.Tree;
const Node = bog.Node;
const TokenList = bog.Token.List;
const TokenIndex = bog.Token.Index;

pub fn render(tree: *Tree, stream: var) @TypeOf(stream).Child.Error!void {
    var renderer = Renderer{
        .source = tree.source,
        .tokens = &tree.tokens,
    };
    {
        var it = tree.tokens.iterator(0);
        while (it.next()) |tok| {
            if (tok.id == .Nl) continue;
            if (tok.id == .Comment)
                try renderer.renderToken(@truncate(u32, it.index - 1), stream, 0, .Newline);
            break;
        }
    }
    {
        var it = tree.nodes.iterator(0);
        while (it.next()) |node| {
            try renderer.renderNode(node.*, stream, 0, .Newline);
        }
    }
}

const Renderer = struct {
    source: []const u8,
    tokens: *TokenList,

    const indent_delta = 4;

    fn prevToken(self: *Renderer, tok: TokenIndex) TokenIndex {
        var it = self.tokens.iterator(tok);
        while (it.prev()) |some| {
            switch (some.id) {
                .Comment, .Nl, .End, .Begin => {},
                else => break,
            }
        }
        return @truncate(TokenIndex, it.index);
    }

    fn nextToken(self: *Renderer, tok: TokenIndex) TokenIndex {
        var it = self.tokens.iterator(tok + 1);
        while (it.next()) |some| {
            switch (some.id) {
                .Comment, .Nl, .End, .Begin => {},
                else => break,
            }
        }
        return @truncate(TokenIndex, it.index - 1);
    }

    fn renderNode(self: *Renderer, node: *Node, stream: var, indent: u32, space: Space) @TypeOf(stream).Child.Error!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                if (literal.kind == .None) {
                    try self.renderToken(self.prevToken(literal.tok), stream, indent, .None);
                }

                return self.renderToken(literal.tok, stream, indent, space);
            },
            .Infix => {
                const infix = @fieldParentPtr(Node.Infix, "base", node);

                const after_tok_space = if (infix.op == .Range) .None else Space.Space;

                try self.renderNode(infix.lhs, stream, indent, after_tok_space);
                try self.renderToken(infix.tok, stream, indent, after_tok_space);
                return self.renderNode(infix.rhs, stream, indent, space);
            },
            .Prefix => {
                const prefix = @fieldParentPtr(Node.Prefix, "base", node);

                switch (prefix.op) {
                    .BoolNot, .Try => try self.renderToken(prefix.tok, stream, indent, .Space),
                    .BitNot, .Minus, .Plus => try self.renderToken(prefix.tok, stream, indent, .None),
                }
                return self.renderNode(prefix.rhs, stream, indent, space);
            },
            .Grouped => {
                const grouped = @fieldParentPtr(Node.Grouped, "base", node);
                const after_tok_space = if (self.tokens.at(grouped.l_tok).id == .LBrace) .Space else Space.None;

                try self.renderToken(grouped.l_tok, stream, indent, after_tok_space);
                try self.renderNode(grouped.expr, stream, indent, after_tok_space);
                return self.renderToken(grouped.r_tok, stream, indent, space);
            },
            .TypeInfix => {
                const type_infix = @fieldParentPtr(Node.TypeInfix, "base", node);

                try self.renderNode(type_infix.lhs, stream, indent, .Space);
                try self.renderToken(type_infix.tok, stream, indent, .Space);
                return self.renderToken(type_infix.type_tok, stream, indent, space);
            },
            .Discard, .Identifier => {
                const single = @fieldParentPtr(Node.SingleToken, "base", node);

                return self.renderToken(single.tok, stream, indent, space);
            },
            .Suffix => {
                const suffix = @fieldParentPtr(Node.Suffix, "base", node);

                try self.renderNode(suffix.lhs, stream, indent, .None);
                try self.renderToken(suffix.l_tok, stream, indent, .None);
                switch (suffix.op) {
                    .Call => |*params| {
                        var it = params.iterator(0);
                        const prev = self.tokens.at(suffix.r_tok - 1).id;
                        if (prev == .Comma or prev == .Nl or prev == .End) {
                            try stream.writeByte('\n');
                            const new_indet = indent + indent_delta;
                            while (it.next()) |param| {
                                try stream.writeByteNTimes(' ', new_indet);
                                try self.renderNode(param.*, stream, new_indet, .None);
                                const comma = self.nextToken(param.*.lastToken());
                                if (self.tokens.at(comma).id == .Comma)
                                    try self.renderToken(comma, stream, indent, .Newline)
                                else
                                    try stream.write(",\n");
                            }
                        } else {
                            while (it.next()) |param| {
                                if (it.peek() == null) {
                                    try self.renderNode(param.*, stream, indent, .None);
                                    break;
                                }
                                try self.renderNode(param.*, stream, indent, .None);
                                try self.renderToken(self.nextToken(param.*.lastToken()), stream, indent, .Space);
                            }
                        }
                    },
                    .Subscript => |arr_node| try self.renderNode(arr_node, stream, indent, .None),
                    .Member => {},
                }
                return self.renderToken(suffix.r_tok, stream, indent, space);
            },
            .Decl => {
                const decl = @fieldParentPtr(Node.Decl, "base", node);

                try self.renderToken(decl.let_const, stream, indent, .Space);
                try self.renderNode(decl.capture, stream, indent, .Space);
                try self.renderToken(decl.eq_tok, stream, indent, .Space);
                return self.renderNode(decl.value, stream, indent, space);
            },
            .Import => {
                const import = @fieldParentPtr(Node.Import, "base", node);

                try self.renderToken(import.tok, stream, indent, .None);
                try self.renderToken(self.nextToken(import.tok), stream, indent, .None);
                try self.renderToken(import.str_tok, stream, indent, .None);
                return self.renderToken(import.r_paren, stream, indent, space);
            },
            .Native => {
                const native = @fieldParentPtr(Node.Native, "base", node);

                try self.renderToken(native.tok, stream, indent, .None);
                try self.renderToken(self.nextToken(native.tok), stream, indent, .None);
                try self.renderToken(native.str_tok, stream, indent, .None);
                return self.renderToken(native.r_paren, stream, indent, space);
            },
            .Error => {
                const err = @fieldParentPtr(Node.Error, "base", node);

                try self.renderToken(err.tok, stream, indent, .None);
                try self.renderToken(self.nextToken(err.tok), stream, indent, .None);
                try self.renderNode(err.value, stream, indent, .None);
                return self.renderToken(err.r_paren, stream, indent, space);
            },
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);

                switch (jump.op) {
                    .Return => |expr| {
                        const after_tok_space = if (expr != null) .Space else space;
                        try self.renderToken(jump.tok, stream, indent, after_tok_space);
                        if (expr) |some| try self.renderNode(some, stream, indent, space);
                    },
                    .Continue, .Break => try self.renderToken(jump.tok, stream, indent, space),
                }
            },
            .While => {
                const while_expr = @fieldParentPtr(Node.While, "base", node);

                try self.renderToken(while_expr.while_tok, stream, indent, .Space);
                try self.renderToken(self.nextToken(while_expr.while_tok), stream, indent, .None);
                if (while_expr.capture) |some| {
                    try self.renderToken(self.nextToken(while_expr.while_tok), stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(while_expr.eq_tok.?, stream, indent, .Space);
                }
                try self.renderNode(while_expr.cond, stream, indent, .None);
                try self.renderToken(while_expr.r_paren, stream, indent, .Space);

                return self.renderNode(while_expr.body, stream, indent, space);
            },
            .For => {
                const for_expr = @fieldParentPtr(Node.For, "base", node);

                try self.renderToken(for_expr.for_tok, stream, indent, .Space);
                try self.renderToken(self.nextToken(for_expr.for_tok), stream, indent, .None);
                if (for_expr.capture) |some| {
                    try self.renderToken(self.nextToken(self.nextToken(for_expr.for_tok)), stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(for_expr.in_tok.?, stream, indent, .Space);
                }
                try self.renderNode(for_expr.cond, stream, indent, .None);
                try self.renderToken(for_expr.r_paren, stream, indent, .Space);

                return self.renderNode(for_expr.body, stream, indent, space);
            },
            .Fn => {
                const fn_expr = @fieldParentPtr(Node.Fn, "base", node);

                try self.renderToken(fn_expr.fn_tok, stream, indent, .None);
                try self.renderToken(self.nextToken(fn_expr.fn_tok), stream, indent, .None);

                var it = fn_expr.params.iterator(0);
                const prev = self.tokens.at(fn_expr.r_paren - 1).id;
                if (prev == .Comma or prev == .Nl or prev == .End) {
                    try stream.writeByte('\n');
                    const new_indet = indent + indent_delta;
                    while (it.next()) |param| {
                        try stream.writeByteNTimes(' ', new_indet);
                        try self.renderNode(param.*, stream, new_indet, .None);
                        const comma = self.nextToken(param.*.lastToken());
                        if (self.tokens.at(comma).id == .Comma)
                            try self.renderToken(comma, stream, indent, .Newline)
                        else
                            try stream.write(",\n");
                    }
                } else {
                    while (it.next()) |param| {
                        if (it.peek() == null) {
                            try self.renderNode(param.*, stream, indent, .None);
                            break;
                        }
                        try self.renderNode(param.*, stream, indent, .None);
                        try self.renderToken(self.nextToken(param.*.lastToken()), stream, indent, .Space);
                    }
                }

                try self.renderToken(fn_expr.r_paren, stream, indent, .Space);
                return self.renderNode(fn_expr.body, stream, indent, space);
            },
            .List, .Tuple, .Map => {
                const ltm = @fieldParentPtr(Node.ListTupleMap, "base", node);

                try self.renderToken(ltm.l_tok, stream, indent, .None);

                var it = ltm.values.iterator(0);
                const prev = self.tokens.at(ltm.r_tok - 1).id;
                if (prev == .Comma or prev == .Nl or prev == .End) {
                    try stream.writeByte('\n');
                    const new_indet = indent + indent_delta;
                    while (it.next()) |param| {
                        try stream.writeByteNTimes(' ', new_indet);
                        try self.renderNode(param.*, stream, new_indet, .None);
                        const comma = self.nextToken(param.*.lastToken());
                        if (self.tokens.at(comma).id == .Comma)
                            try self.renderToken(comma, stream, indent, .Newline)
                        else
                            try stream.write(",\n");
                    }
                    try stream.writeByteNTimes(' ', indent);
                } else {
                    while (it.next()) |param| {
                        if (it.peek() == null) {
                            try self.renderNode(param.*, stream, indent, .None);
                            break;
                        }
                        try self.renderNode(param.*, stream, indent, .None);
                        try self.renderToken(self.nextToken(param.*.lastToken()), stream, indent, .Space);
                    }
                }

                return self.renderToken(ltm.r_tok, stream, indent, space);
            },
            .Block => {
                const blk = @fieldParentPtr(Node.Block, "base", node);

                const new_indet = indent + indent_delta;
                var it = blk.stmts.iterator(0);
                while (it.next()) |stmt| {
                    try stream.writeByteNTimes(' ', new_indet);
                    try self.renderNode(stmt.*, stream, new_indet, .Newline);
                }
            },
            .MapItem => {
                const item = @fieldParentPtr(Node.MapItem, "base", node);

                if (item.key) |some| {
                    try self.renderNode(some, stream, indent, .None);
                    try self.renderToken(item.colon.?, stream, indent, .Space);
                }

                return self.renderNode(item.value, stream, indent, space);
            },
            .Catch => {
                const catch_expr = @fieldParentPtr(Node.Catch, "base", node);

                try self.renderNode(catch_expr.lhs, stream, indent, .Space);
                try self.renderToken(catch_expr.tok, stream, indent, .Space);

                if (catch_expr.capture) |some| {
                    try self.renderToken(self.nextToken(catch_expr.tok), stream, indent, .None);
                    try self.renderToken(catch_expr.let_const.?, stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .None);
                    try self.renderToken(self.nextToken(some.lastToken()), stream, indent, .Space);
                }
                return self.renderNode(catch_expr.rhs, stream, indent, space);
            },
            .If => {
                const if_expr = @fieldParentPtr(Node.If, "base", node);

                try self.renderToken(if_expr.if_tok, stream, indent, .Space);
                try self.renderToken(self.nextToken(if_expr.if_tok), stream, indent, .None);
                if (if_expr.capture) |some| {
                    try self.renderToken(self.nextToken(self.nextToken(if_expr.if_tok)), stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(if_expr.eq_tok.?, stream, indent, .Space);
                }

                try self.renderNode(if_expr.cond, stream, indent, .None);
                try self.renderToken(if_expr.r_paren, stream, indent, .Space);

                if (if_expr.else_body) |some| {
                    try self.renderNode(if_expr.if_body, stream, indent, .Space);
                    if (if_expr.if_body.id == .Block)
                        try stream.writeByteNTimes(' ', indent);

                    try self.renderToken(if_expr.else_tok.?, stream, indent, .Space);
                    return self.renderNode(some, stream, indent, space);
                } else {
                    return self.renderNode(if_expr.if_body, stream, indent, space);
                }
            },
            .Match => {
                const match_expr = @fieldParentPtr(Node.Match, "base", node);

                try self.renderToken(match_expr.match_tok, stream, indent, .Space);
                try self.renderToken(self.nextToken(match_expr.match_tok), stream, indent, .None);
                try self.renderNode(match_expr.expr, stream, indent, .None);
                try self.renderToken(match_expr.r_paren, stream, indent, .Newline);

                const new_indet = indent + indent_delta;
                var it = match_expr.body.iterator(0);
                while (it.next()) |n| {
                    try stream.writeByteNTimes(' ', new_indet);
                    try self.renderNode(n.*, stream, new_indet, .Newline);
                }
                try stream.writeByteNTimes(' ', indent);
            },
            .MatchCatchAll => {
                const case = @fieldParentPtr(Node.MatchCatchAll, "base", node);

                if (self.tokens.at(case.tok).id != .Underscore) {
                    try self.renderToken(case.tok, stream, indent, .Space);
                    try self.renderToken(self.nextToken(case.tok), stream, indent, .None);
                } else {
                    try self.renderToken(case.tok, stream, indent, .None);
                }
                try self.renderToken(case.colon, stream, indent, .Space);
                return self.renderNode(case.expr, stream, indent, space);
            },
            .MatchLet => {
                const case = @fieldParentPtr(Node.MatchLet, "base", node);

                try self.renderToken(case.let_const, stream, indent, .Space);
                try self.renderNode(case.capture, stream, indent, .None);
                try self.renderToken(case.colon, stream, indent, .Space);
                return self.renderNode(case.expr, stream, indent, space);
            },
            .MatchCase => {
                const case = @fieldParentPtr(Node.MatchCase, "base", node);

                var it = case.lhs.iterator(0);
                // TODO trailing comma
                while (it.next()) |param| {
                    if (it.peek() == null) {
                        try self.renderNode(param.*, stream, indent, .None);
                        break;
                    }
                    try self.renderNode(param.*, stream, indent, .None);
                    try self.renderToken(self.nextToken(param.*.lastToken()), stream, indent, .Space);
                }
                try self.renderToken(case.colon, stream, indent, .Space);

                return self.renderNode(case.expr, stream, indent, space);
            },
        }
    }

    const Space = enum {
        None,
        Newline,
        Space,
    };

    fn renderToken(self: *Renderer, token: TokenIndex, stream: var, indent: u32, space: Space) !void {
        var tok = self.tokens.at(token);
        try stream.write(self.source[tok.start..tok.end]);
        switch (space) {
            .None => if (tok.id == .Comment) try stream.writeByte('\n'),
            .Newline => try stream.writeByte('\n'),
            .Space => if (self.tokens.len > token + 2 and self.tokens.at(token + 2).id == .Begin) {
                try stream.writeByte('\n');
            } else {
                try stream.writeByte(' ');
            },
        }
        var i = token;
        tok = self.tokens.at(i);
        while (true) {
            i += 1;
            tok = self.tokens.at(i);
            switch (tok.id) {
                .Nl, .End, .Begin => continue,
                .Comment => {},
                else => break,
            }

            try stream.write(self.source[tok.start..tok.end]);
            try stream.writeByte('\n');
            if (space != .Newline)
                try stream.writeByteNTimes(' ', indent + indent_delta);
        }
    }
};
