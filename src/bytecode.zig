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

    /// A = PRIMTIVE(B)
    /// 0 = ()
    /// 1 = false
    /// 2 = true
    ConstPrimitive,

    /// A = arg1
    ConstInt8,
    ConstInt32,
    ConstInt64,

    /// A = STRING(arg1)
    ConstString,

    /// A = NUM(arg1)
    ConstNum,

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
    BitAnd,

    /// A = B | C
    BitOr,

    /// A = B ^ C
    BitXor,

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
    DirectBitAnd,

    /// A |= B
    DirectBitOr,

    /// A ^= C
    DirectBitXor,

    /// A = B and C
    And,

    /// A = B or C
    Or,

    /// A = not B
    BoolNot,

    /// A = ~B
    BitNot,

    /// A = -B
    Negate,

    /// IF (A==error) RET A
    Try,

    /// A = B as TYPEID(C)
    Cast,

    Return,

    /// A = error(A)
    BuildError,

    /// A = (arg1, ... argB)
    BuildTuple,

    /// ip = arg1
    Jump,

    /// if (A) ip = arg1
    JumpTrue,

    /// if (not A) ip = arg1
    JumpFalse,

    /// if (not A is error) ip = arg1
    JumpNotErr,

    /// A = IMPORT(arg1)
    Import,

    // _,
};

// TODO optimize size of this
pub const RegRef = u16;

pub const Module = struct {
    name: []const u8,
    code: []const u8,
    strings: []const u8,
    start_index: u32,
    // debug_info,

    pub fn read(src: []const u8) Module {
        @panic("TODO");
    }

    pub fn write(module: Module, stream: var) @TypeOf(stream).Error!void {
        @panic("TODO");
    }
};
