const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ast = @import("ast.zig");
const Node = ast.Node;
const TypeId = @import("value.zig").TypeId;

// TODO give these numbers once they are more stable
pub const Op = enum(u8) {
    /// A <- B
    Move,

    /// CALL(A)
    Call,

    /// STACK(A, B) = C
    PushStack,

    /// DISCARD(A)
    Discard,

    /// A = BOOL(B)
    ConstBool,

    /// A = ()
    ConstNone,

    /// A = B // C
    DivFloor,

    /// A = B / C
    Div,

    /// A = B * C
    Mul,

    /// A = B ** C
    Pow,

    /// A = B % C
    Mod,

    /// A = B + C
    Add,

    /// A = B - C
    Sub,

    /// A = B << C
    LShift,

    /// A = B << C
    RShift,

    /// A = B & C
    BinAnd,

    /// A = B | C
    BinOr,

    /// A //= B
    DirectDivFloor,

    /// A /= B
    DirectDiv,

    /// A *= B
    DirectMul,

    /// A **= B
    DirectPow,

    /// A %= B
    DirectMod,

    /// A += B
    DirectAdd,

    /// A -= B
    DirectSub,

    /// A <<= B
    DirectLShift,

    /// A >>= B
    DirectRShift,

    /// A &= B
    DirectBinAnd,

    /// A |= B
    DirectBinOr,

    /// A = B and C
    And,

    /// A = B or C
    Or,

    /// A = not B
    Not,

    /// A = ~B
    BinNot,

    /// A = -B
    Negate,

    /// IF (A==error) RET A
    Try,

    /// A = B as TYPEID(C)
    Cast,

    Return,

    /// A = error(A)
    BuildError,

    // all ops below have args
    const HasArg = 120;

    /// ip = arg1
    Jump = HasArg,

    /// if (A) ip = arg1
    JumpTrue,

    /// if (not A) ip = arg1
    JumpFalse,

    /// if (not A is error) ip = arg1
    JumpNotErr,

    /// A = arg1
    ConstSmallInt,

    /// A = STRING(arg1)
    ConstString,

    /// A = NUM(arg1)
    ConstNum,

    /// A = IMPORT(arg1)
    Import,

    // _,

    pub fn hasArg(op: Op) bool {
        return @enumToInt(op) >= HasArg;
    }
};

pub const Module = struct {
    name: []const u8,
    code: []const u8,
    strings: []const u8,
    start_index: u32,

    pub fn read(src: []const u8) Module {
        @panic("TODO");
    }

    pub fn write(module: Module, stream: var) @TypeOf(stream).Error!void {
        @panic("TODO");
    }
};
