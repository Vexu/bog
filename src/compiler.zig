const std = @import("std");
const mem = std.mem;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const TokenList = tokenizer.TokenList;
const TokenIndex = tokenizer.TokenIndex;
const Allocator = mem.Allocator;
const TypeId = @import("value.zig").TypeId;
const ast = @import("ast.zig");
const Node = ast.Node;

pub const Compiler = struct {
    builder: *Builder,
    tokens: TokenList,

    const Value = union(enum) {
        None,

        /// value of `continue`
        Empty,
        Rt,
        Int: i64,
        Num: f64,
        Bool: bool,
        Str: []const u8,
    };

    fn makeRuntime(self: *Compiler, res: RegRef, val: Value) !void {
        return switch (val) {
            .Empty => unreachable,
            .Rt => |v| {
                std.debug.assert(v == res);
                return v;
            },
            .None => self.builder.constNone(res),
            .Int => |v| self.builder.constInt(res, v),
            .Num => |v| self.builder.constNum(res, v),
            .Bool => |v| self.builder.constBool(res, v),
            .Str => |v| self.builder.constStr(res, v),
        };
    }

    fn genNode(self: *Compiler, node: *Node, res: RegRef) !Value {
        switch (node.id) {
            .Grouped => return self.genNode(@fieldParentPtr(Node.Grouped, "base", node).expr, res),
            .Literal => return self.genLiteral(@fieldParentPtr(Node.Literal, "base", node), res),
            .Let => @panic("TODO: Let"),
            .Fn => @panic("TODO: Fn"),
            .Discard => @panic("TODO: Discard"),
            .Identifier => @panic("TODO: Identifier"),
            .Prefix => @panic("TODO: Prefix"),
            .Infix => @panic("TODO: Infix"),
            .TypeInfix => @panic("TODO: TypeInfix"),
            .Suffix => @panic("TODO: Suffix"),
            .Literal => @panic("TODO: Literal"),
            .Import => @panic("TODO: Import"),
            .Error => @panic("TODO: Error"),
            .List => @panic("TODO: List"),
            .Tuple => @panic("TODO: Tuple"),
            .Map => @panic("TODO: Map"),
            .Block => @panic("TODO: Block"),
            .Catch => @panic("TODO: Catch"),
            .If => @panic("TODO: If"),
            .For => @panic("TODO: For"),
            .While => @panic("TODO: While"),
            .Match => @panic("TODO: Match"),
            .Jump => @panic("TODO: Jump"),
            .MapItem,
            .MatchCatchAll,
            .MatchLet,
            .MatchCase,
            .Unwrap,
            => unreachable,
        }
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, _: RegRef) !Value {
        return switch (node.kind) {
            .Int => Value{ .Int = try self.parseInt(node.tok) },
            .True => Value{ .Bool = true },
            .False => Value{ .Bool = false },
            .None => Value.None,
            .Str => @panic("TODO: genStr"),
            .Num => @panic("TODO: genNum"),
        };
    }

    fn tokenSlice(self: *Compiler, token: TokenIndex) Token.Id {
        const tok = self.tokens.at(token);
        return self.source[tok.start..tok.end];
    }

    fn parseInt(self: *Compiler, tok: TokenIndex) Token.Id {
        var buf = self.tokenSlice(tok);
        var radix: u8 = if (buf.len > 2) switch (buf[2]) {
            'x' => 16,
            'b' => 2,
            'o' => 8,
            else => 10,
        } else 10;
        if (radix != 10) buf = buf[2..];
        var x: i64 = 0;

        for (buf) |c| {
            const digit = switch (c) {
                '0'...'9' => c - '0',
                'A'...'Z' => c - 'A' + 10,
                'a'...'z' => c - 'a' + 10,
                '_' => continue,
                else => unreachable,
            };

            x = math.mul(i64, x, radix) catch return .{
                .Invalid = "integer too big",
            };
            x += digit;
        }

        return .{ .Integer = x };
    }
};
