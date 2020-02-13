const std = @import("std");

const tokenizer = @import("tokenizer.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;
pub const tokenize = Tokenizer.tokenize;

const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const parse = Parser.parse;

const ast = @import("ast.zig");
pub const Tree = ast.Tree;
pub const Node = ast.Node;

const compiler = @import("compiler.zig");
pub const Compiler = compiler.Compiler;

const value = @import("value.zig");
pub const Value = value.Value;
pub const Ref = value.Ref;

const vm = @import("vm.zig");
pub const Vm = vm.Vm;

const bytecode = @import("bytecode.zig");
pub const Op = bytecode.Op;
pub const Module = bytecode.Module;
pub const RegRef = bytecode.RegRef;

pub const Error = struct {
    index: u32,
    kind: Kind,
    msg: []const u8,

    const Kind = enum {
        Error,
        Note,
        Trace,
    };

    pub const List = std.SegmentedList(Error, 0);
};
