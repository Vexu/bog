const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

pub const Op = enum(u8) {
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
    pub const RegRef = usize;

    pub fn init(allocator: *Allocator) Builder {
        return .{};
    }

    pub fn discard(self: *Builder, reg: RegRef) anyerror!void {
        std.debug.warn("discard #{}\n", .{reg});
    }

    pub fn move(self: *Builder, from: RegRef, to: RegRef) anyerror!void {
        std.debug.warn("move #{} to #{}\n", .{ from, to });
    }

    pub fn jumpFalse(self: *Builder, reg: RegRef) anyerror!usize {
        std.debug.warn("jumpFalse #{}\n", .{reg});
        return 1;
    }

    pub fn finishJump(self: *Builder, jump: usize) void {
        std.debug.warn("finishJump #{}\n", .{jump});
    }

    pub fn isErr(self: *Builder, reg: RegRef) anyerror!RegRef {
        std.debug.warn("isErr {}\n", .{ reg });
        return reg;
    }

    // exprs

    pub fn boolOr(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} or {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn boolAnd(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} and {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn lessThan(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} < {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn lessThanEqual(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} <= {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn greaterThan(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} > {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn greaterThanEqual(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} >= {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn equal(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} == {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn notEqual(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} != {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn in(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} in {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn is(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} is {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn range(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} ... {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn bitAnd(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} & {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn bitOr(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} | {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn bitXor(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} ^ {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn leftShift(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} << {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn rightShift(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} >> {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn sub(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} - {}\n", .{ lhs, rhs });
        return lhs;
    }

    pub fn add(self: *Builder, lhs: RegRef, rhs: RegRef) anyerror!RegRef {
        std.debug.warn("{} + {}\n", .{ lhs, rhs });
        return lhs;
    }
};
