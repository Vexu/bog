const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const TypeId = @import("value.zig").TypeId;
const ast = @import("ast.zig");
const Node = ast.Node;
const Tree = ast.Tree;
const TokenList = ast.TokenList;
const TokenIndex = ast.TokenIndex;
const bytecode = @import("bytecode.zig");
const Builder = bytecode.Builder;
const RegRef = bytecode.RegRef;

pub fn compile(builder: *Builder, tree: *Tree, start_index: usize) Error!void {
    var compiler = Compiler{
        .builder = builder,
        .tree = tree,
    };
    var it = tree.nodes.iterator(start_index);
    while (it.next()) |n| {
        const val = try compiler.genNode(n.*, .Value);
        if (val == .Rt) {
            // discard unused runtime value
            try builder.discard(val.Rt);
        } else if (it.peek() == null and val != .Empty) {
            const reg = builder.cur_func.registerAlloc(.Temp) catch return error.CompileError;
            try compiler.makeRuntime(reg, val);
            try builder.discard(reg);
        }
    }
}

pub const Error = error{CompileError} || Allocator.Error;

pub const Compiler = struct {
    builder: *Builder,
    tree: *Tree,

    const Value = union(enum) {
        /// result of continue, break, return and assignmnet; cannot exist at runtime
        Empty,
        Rt: RegRef,

        None,
        Int: i64,
        Num: f64,
        Bool: bool,
        Str: []const u8,
    };

    fn makeRuntime(self: *Compiler, res: RegRef, val: Value) !void {
        return switch (val) {
            .Empty => unreachable,
            .Rt => |v| std.debug.assert(v == res),
            .None => try self.builder.constNone(res),
            .Int => |v| try self.builder.constInt(res, v),
            .Num => |v| try self.builder.constNum(res, v),
            .Bool => |v| try self.builder.constBool(res, v),
            .Str => |v| try self.builder.constStr(res, v),
        };
    }

    const Result = union(enum) {
        /// A runtime value is expected
        Rt: RegRef,

        /// Something assignable is expected
        Lval,

        /// A value, runtime or constant, is expected
        Value,
    };

    fn genNode(self: *Compiler, node: *Node, res: Result) Error!Value {
        switch (node.id) {
            .Grouped => return self.genNode(@fieldParentPtr(Node.Grouped, "base", node).expr, res),
            .Literal => return self.genLiteral(@fieldParentPtr(Node.Literal, "base", node), res),
            .Block => return self.genBlock(@fieldParentPtr(Node.ListTupleMapBlock, "base", node), res),
            .Prefix => return self.genPrefix(@fieldParentPtr(Node.Prefix, "base", node), res),
            .Let => @panic("TODO: Let"),
            .Fn => @panic("TODO: Fn"),
            .Identifier => @panic("TODO: Identifier"),
            .Infix => @panic("TODO: Infix"),
            .TypeInfix => @panic("TODO: TypeInfix"),
            .Suffix => @panic("TODO: Suffix"),
            .Import => @panic("TODO: Import"),
            .Error => @panic("TODO: Error"),
            .List => @panic("TODO: List"),
            .Tuple => @panic("TODO: Tuple"),
            .Map => @panic("TODO: Map"),
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
            .Discard,
            => unreachable,
        }
    }

    fn genBlock(self: *Compiler, node: *Node.ListTupleMapBlock, res: Result) !Value {
        if (res == .Lval) {
            // try adderr("cannot assign to block")
            return error.CompileError;
        }
        var it = node.values.iterator(0);
        while (it.next()) |n| {
            if (it.peek() == null) {
                return self.genNode(n.*, res);
            }
            const val = try self.genNode(n.*, .Value);
            if (val == .Rt) {
                // discard unused runtime value
                try self.builder.discard(val.Rt);
            }
        }
        unreachable;
    }

    fn assertNotLval(self: *Compiler, res: Result) !void {
        if (res == .Lval) {
            // try adderr("invalid left hand side to assignment")
            return error.CompileError;
        }
    }

    fn assertNotEmpty(self: *Compiler, val: Value) !void {
        if (val == .Empty) {
            // try adderr("expected value")
            return error.CompileError;
        }
    }

    fn assertBool(self: *Compiler, val: Value) !void {
        if (val != .Bool) {
            // try adderr("expected boolean value")
            return error.CompileError;
        }
    }

    fn assertInt(self: *Compiler, val: Value) !void {
        if (val != .Int) {
            // try adderr("expected integer value")
            return error.CompileError;
        }
    }

    fn assertNumeric(self: *Compiler, val: Value) !void {
        if (val != .Int and val != .Num) {
            // try adderr("expected numeric value")
            return error.CompileError;
        }
    }

    fn genPrefix(self: *Compiler, node: *Node.Prefix, res: Result) !Value {
        try self.assertNotLval(res);
        const r_val = try self.genNode(node.rhs, .Value);
        try self.assertNotEmpty(r_val);
        if (r_val == .Rt) {
            const result_loc = if (res == .Rt) res.Rt else @panic("TODO: create result loc");
            try self.builder.prefix(result_loc, node.op, r_val.Rt);
            return Value{ .Rt = result_loc };
        }
        const ret_val = switch (node.op) {
            .BoolNot => blk: {
                try self.assertBool(r_val);
                break :blk Value{ .Bool = !r_val.Bool };
            },
            .BitNot => blk: {
                try self.assertInt(r_val);
                break :blk Value{ .Int = ~r_val.Int };
            },
            .Minus => blk: {
                try self.assertNumeric(r_val);
                if (r_val == .Int) {
                    break :blk Value{ .Int = -r_val.Int };
                } else {
                    break :blk Value{ .Num = -r_val.Num };
                }
            },
            .Plus => blk: {
                try self.assertNumeric(r_val);
                break :blk r_val;
            },
            // errors are runtime only currently, so ret_val does not need to be checked
            // TODO should this be an error?
            .Try => r_val,
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .NoVal or .Value nothing needs to be done
        return ret_val;
    }

    fn genLiteral(self: *Compiler, node: *Node.Literal, res: Result) !Value {
        try self.assertNotLval(res);
        const ret_val: Value = switch (node.kind) {
            .Int => .{ .Int = try self.parseInt(node.tok) },
            .True => .{ .Bool = true },
            .False => .{ .Bool = false },
            .None => .None,
            .Str => @panic("TODO: genStr"),
            .Num => @panic("TODO: genNum"),
        };
        if (res == .Rt) {
            try self.makeRuntime(res.Rt, ret_val);
            return Value{ .Rt = res.Rt };
        }
        // if res == .NoVal or .Value nothing needs to be done
        return ret_val;
    }

    fn tokenSlice(self: *Compiler, token: TokenIndex) []const u8 {
        const tok = self.tree.tokens.at(token);
        return self.tree.source[tok.start..tok.end];
    }

    fn parseInt(self: *Compiler, tok: TokenIndex) !i64 {
        var buf = self.tokenSlice(tok);
        var radix: u8 = if (buf.len > 2) switch (buf[2]) {
            'x' => @as(u8, 16),
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

            x = std.math.mul(i64, x, radix) catch {
                // try self.adderr();
                return error.CompileError;
            };
            // why is this cast needed?
            x += @intCast(i32, digit);
        }

        return x;
    }
};
