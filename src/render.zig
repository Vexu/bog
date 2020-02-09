const std = @import("std");
const lang = @import("lang.zig");
const Tree = lang.Tree;
const Node = lang.Node;
const TokenList = lang.Token.List;
const TokenIndex = lang.Token.Index;

pub fn render(tree: *Tree, stream: var) @TypeOf(stream).Child.Error!void {
    var it = tree.nodes.iterator(0);
    var renderer = Renderer{
        .source = tree.source,
        .tokens = &tree.tokens,
    };
    while (it.next()) |node| {
        try renderer.renderNode(node.*, stream, 0, .Newline);
    }
}

const Renderer = struct {
    source: []const u8,
    tokens: *TokenList,

    const indent_delta = 4;

    fn renderNode(self: *Renderer, node: *Node, stream: var, indent: u32, space: Space) @TypeOf(stream).Child.Error!void {
        switch (node.id) {
            .Literal => {
                const literal = @fieldParentPtr(Node.Literal, "base", node);
                if (literal.kind == .None) {
                    try self.renderToken(literal.tok - 1, stream, indent, .None);
                }

                return self.renderToken(literal.tok, stream, indent, space);
            },
            .Infix => {
                const infix = @fieldParentPtr(Node.Infix, "base", node);

                try self.renderNode(infix.lhs, stream, indent, .Space);
                try self.renderToken(infix.tok, stream, indent, .Space);
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
                try self.renderToken(type_infix.tok - 1, stream, indent, .Space);
                return self.renderToken(type_infix.tok, stream, indent, space);
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
                        if (prev == .Comma or prev == .Nl) {
                            try stream.writeByte('\n');
                            const new_indet = indent + indent_delta;
                            while (it.next()) |param| {
                                try stream.writeByteNTimes(' ', new_indet);
                                try self.renderNode(param.*, stream, new_indet, .None);
                                try stream.write(",\n");
                            }
                        } else {
                            while (it.next()) |param| {
                                if (it.peek() == null) {
                                    try self.renderNode(param.*, stream, indent, .None);
                                    break;
                                }
                                try self.renderNode(param.*, stream, indent, .None);
                                try stream.write(", ");
                            }
                        }
                    },
                    .ArrAccess => |arr_node| try self.renderNode(arr_node, stream, indent, .None),
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
                try self.renderToken(import.tok + 1, stream, indent, .None);
                try self.renderToken(import.str_tok, stream, indent, .None);
                return self.renderToken(import.str_tok + 1, stream, indent, space);
            },
            .Error => {
                const err = @fieldParentPtr(Node.Error, "base", node);

                try self.renderToken(err.tok, stream, indent, .None);
                try self.renderToken(err.tok + 1, stream, indent, .None);
                try self.renderNode(err.value, stream, indent, .None);
                return self.renderToken(err.r_paren, stream, indent, space);
            },
            .Jump => {
                const jump = @fieldParentPtr(Node.Jump, "base", node);

                switch (jump.op) {
                    .Return, .Break => |expr| {
                        const after_tok_space = if (expr != null) .Space else space;
                        try self.renderToken(jump.tok, stream, indent, after_tok_space);
                        if (expr) |some| try self.renderNode(some, stream, indent, space);
                    },
                    .Continue => try self.renderToken(jump.tok, stream, indent, space),
                }
            },
            .While => {
                const while_expr = @fieldParentPtr(Node.While, "base", node);

                try self.renderToken(while_expr.while_tok, stream, indent, .Space);
                if (while_expr.capture) |some| {
                    try self.renderToken(while_expr.while_tok + 1, stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(while_expr.eq_tok.?, stream, indent, .Space);
                }
                try self.renderNode(while_expr.cond, stream, indent, .Space);
                return self.renderNode(while_expr.body, stream, indent, space);
            },
            .For => {
                const for_expr = @fieldParentPtr(Node.For, "base", node);

                try self.renderToken(for_expr.for_tok, stream, indent, .Space);
                if (for_expr.capture) |some| {
                    try self.renderToken(for_expr.for_tok + 1, stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(for_expr.in_tok.?, stream, indent, .Space);
                }
                try self.renderNode(for_expr.cond, stream, indent, .Space);
                return self.renderNode(for_expr.body, stream, indent, space);
            },
            .Fn => {
                const fn_expr = @fieldParentPtr(Node.Fn, "base", node);

                try self.renderToken(fn_expr.fn_tok, stream, indent, .None);
                try self.renderToken(fn_expr.fn_tok + 1, stream, indent, .None);

                var it = fn_expr.params.iterator(0);
                const prev = self.tokens.at(fn_expr.r_paren - 1).id;
                if (prev == .Comma or prev == .Nl) {
                    try stream.writeByte('\n');
                    const new_indet = indent + indent_delta;
                    while (it.next()) |param| {
                        try stream.writeByteNTimes(' ', new_indet);
                        try self.renderNode(param.*, stream, new_indet, .None);
                        try stream.write(",\n");
                    }
                } else {
                    while (it.next()) |param| {
                        if (it.peek() == null) {
                            try self.renderNode(param.*, stream, indent, .None);
                            break;
                        }
                        try self.renderNode(param.*, stream, indent, .None);
                        try stream.write(", ");
                    }
                }

                try self.renderToken(fn_expr.r_paren, stream, indent, .Space);
                return self.renderNode(fn_expr.body, stream, indent, space);
            },
            .List, .Tuple, .Map, .Block => {
                const ltmb = @fieldParentPtr(Node.ListTupleMapBlock, "base", node);

                try self.renderToken(ltmb.l_tok, stream, indent, .None);

                var it = ltmb.values.iterator(0);
                const prev = self.tokens.at(ltmb.r_tok - 1).id;
                if (prev == .Comma or prev == .Nl or (node.id == .Block and ltmb.values.len != 1)) {
                    try stream.writeByte('\n');
                    const new_indet = indent + indent_delta;
                    while (it.next()) |param| {
                        try stream.writeByteNTimes(' ', new_indet);
                        try self.renderNode(param.*, stream, new_indet, .None);
                        if (node.id != .Block)
                            try stream.write(",\n")
                        else
                            try stream.writeByte('\n');
                    }
                    try stream.writeByteNTimes(' ', indent);
                } else {
                    while (it.next()) |param| {
                        if (it.peek() == null) {
                            try self.renderNode(param.*, stream, indent, .None);
                            break;
                        }
                        try self.renderNode(param.*, stream, indent, .None);
                        try stream.write(", ");
                    }
                }

                return self.renderToken(ltmb.r_tok, stream, indent, space);
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
                    try self.renderToken(catch_expr.tok + 1, stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                }
                return self.renderNode(catch_expr.rhs, stream, indent, space);
            },
            .If => {
                const if_expr = @fieldParentPtr(Node.If, "base", node);

                try self.renderToken(if_expr.if_tok, stream, indent, .Space);
                if (if_expr.capture) |some| {
                    try self.renderToken(if_expr.if_tok + 1, stream, indent, .Space);
                    try self.renderNode(some, stream, indent, .Space);
                    try self.renderToken(if_expr.eq_tok.?, stream, indent, .Space);
                }

                try self.renderNode(if_expr.cond, stream, indent, .Space);

                if (if_expr.else_body) |some| {
                    try self.renderNode(if_expr.if_body, stream, indent, .Space);

                    try self.renderToken(if_expr.else_tok.?, stream, indent, .Space);
                    return self.renderNode(some, stream, indent, space);
                } else {
                    return self.renderNode(if_expr.if_body, stream, indent, space);
                }
            },
            .Match => {
                const match_expr = @fieldParentPtr(Node.Match, "base", node);

                try self.renderToken(match_expr.match_tok, stream, indent, .Space);
                try self.renderNode(match_expr.expr, stream, indent, .Space);

                try self.renderToken(match_expr.body_l_brace, stream, indent, .Newline);
                const new_indet = indent + indent_delta;
                var it = match_expr.body.iterator(0);
                while (it.next()) |n| {
                    try stream.writeByteNTimes(' ', new_indet);
                    try self.renderNode(n.*, stream, new_indet, .Newline);
                }
                try stream.writeByteNTimes(' ', indent);

                return self.renderToken(match_expr.body_r_brace, stream, indent, space);
            },
            .MatchCatchAll => {
                const case = @fieldParentPtr(Node.MatchCatchAll, "base", node);

                if (self.tokens.at(case.tok).id == .Identifier) {
                    try self.renderToken(case.tok - 1, stream, indent, .Space);
                }
                try self.renderToken(case.tok, stream, indent, .Space);
                return self.renderNode(case.expr, stream, indent, space);
            },
            .MatchLet => {
                const case = @fieldParentPtr(Node.MatchLet, "base", node);

                try self.renderToken(case.let_const, stream, indent, .Space);
                try self.renderNode(case.capture, stream, indent, .Space);
                return self.renderNode(case.expr, stream, indent, space);
            },
            .MatchCase => {
                const case = @fieldParentPtr(Node.MatchCase, "base", node);


                var it = case.lhs.iterator(0);
                while (it.next()) |param| {
                    if (it.peek() == null) {
                        try self.renderNode(param.*, stream, indent, .Space);
                        break;
                    }
                    try self.renderNode(param.*, stream, indent, .None);
                    try stream.write(", ");
                }

                return self.renderNode(case.expr, stream, indent, space);
            },
        }
    }

    const Space = enum {
        None,
        Newline,
        Space,
    };

    // TODO keep comments
    fn renderToken(self: *Renderer, token: TokenIndex, stream: var, indent: u32, space: Space) !void {
        const tok = self.tokens.at(token);
        try stream.write(self.source[tok.start..tok.end]);
        switch (space) {
            .None => {},
            .Newline => try stream.writeByte('\n'),
            .Space => try stream.writeByte(' '),
        }
    }
};
