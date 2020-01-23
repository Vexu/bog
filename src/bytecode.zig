const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;

pub const Op = enum(u8) {
    Move,
    Push,
    Pop,
    Call,

    /// A = A // B
    DivFloor = 10,

    /// A = A / B
    Div = 11,

    /// A = A * B
    Mul = 12,

    /// A = A % B
    Mod = 13,

    /// A = A + B
    Add = 14,

    /// A = A - B
    Sub = 15,

    /// A = A << B
    LShift = 16,

    /// A = A << B
    RShift = 17,

    /// A = A & B
    BinAnd = 18,

    /// A = A | B
    BinOr = 19,

    /// A = ~A
    BinNot = 20,

    /// A = not A
    Not = 21,

    /// A = A and B
    And = 22,

    /// A = A or B
    Or = 23,

    Jump,

    JumpTrue,

    Return,

    Break,
    EndBreak,

    Try,

    _,
};

pub const Builder = struct {
    regs: RegRef = 0,
    pub const RegRef = u32;

    pub fn init(allocator: *Allocator) Builder {
        return .{};
    }

    pub fn deinit(self: *Builder) void {}

    pub fn discard(self: *Builder, reg: RegRef) anyerror!void {
        defer self.registerFree(reg);
        std.debug.warn("discard #{}\n", .{reg});
    }

    pub fn move(self: *Builder, from: RegRef, to: RegRef) anyerror!void {
        defer self.registerFree(from);
        std.debug.warn("move #{} to #{}\n", .{ from, to });
    }

    pub fn jumpFalse(self: *Builder, reg: RegRef) anyerror!usize {
        std.debug.warn("jumpFalse #{}\n", .{reg});
        return 1;
    }

    pub fn jumpNotErr(self: *Builder, reg: RegRef) anyerror!usize {
        std.debug.warn("jumpNotErr #{}\n", .{reg});
        return 1;
    }

    pub fn finishJump(self: *Builder, jump: usize) void {
        std.debug.warn("#finishJump #{}\n", .{jump});
    }

    fn registerAlloc(self: *Builder) RegRef {
        // TODO push if > 8
        const reg = self.regs;
        self.regs += 1;
        if (self.regs > 7) @panic("TODO register overflow");
        return reg;
    }

    fn registerFree(self: *Builder, reg: RegRef) void {
        std.debug.assert(self.regs != 0);
        self.regs -= 1;
    }

    pub fn constant(self: *Builder, tok: *Token) anyerror!RegRef {
        const reg = self.registerAlloc();
        std.debug.warn("#{} constant {}\n", .{ reg, tok });
        return reg;
    }

    pub fn declRef(self: *Builder, tok: *Token) anyerror!RegRef {
        const reg = self.registerAlloc();
        std.debug.warn("#{} declref {}\n", .{ reg, tok });
        return reg;
    }

    pub fn buildErr(self: *Builder, tok: *Token, val: RegRef) anyerror!RegRef {
        const reg = self.registerAlloc();
        std.debug.warn("buildErr {}\n", .{ val });
        return reg;
    }

    pub fn buildList(self: *Builder, tok: *Token) anyerror!usize {
        // todo allocate register here?
        const list = 1;
        std.debug.warn("buildList {}\n", .{ list });
        return list;
    }

    pub fn finishList(self: *Builder, tok: *Token, list: usize) anyerror!RegRef {
        const reg = self.registerAlloc();
        std.debug.warn("#finishList {}\n", .{ list });
        return reg;
    }

    pub fn listPush(self: *Builder, val: RegRef) anyerror!void {
        defer self.registerFree(val);
        std.debug.warn("listPush {}\n", .{ val });
    }

    pub fn import(self: *Builder, tok: *Token, str: RegRef) anyerror!RegRef {
        const reg = self.registerAlloc();
        std.debug.warn("import {}\n", .{ str });
        return reg;
    }

    pub fn prefix(self: *Builder, tok: *Token, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} {}\n", .{ tok, rhs });
        return rhs;
    }

    pub fn infix(self: *Builder, lhs: RegRef, tok: *Token, rhs: RegRef) anyerror!RegRef {
        defer self.registerFree(rhs);
        std.debug.warn("#{} {} #{}\n", .{ lhs, tok, rhs });
        return lhs;
    }

    pub fn assign(self: *Builder, lhs: RegRef, tok: *Token, rhs: RegRef) anyerror!void {
        defer self.registerFree(rhs);
        defer self.registerFree(lhs);
        std.debug.warn("#{} {} #{}\n", .{ lhs, tok, rhs });
    }
};
