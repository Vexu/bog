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

    // /// STACK(A, B) = C
    // PushStack,

    /// DISCARD(A)
    Discard,

    /// A = PRIMTIVE(arg1)
    /// 0 = ()
    /// 1 = false
    /// 2 = true
    ConstPrimitive,

    /// A = INT(arg1)
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

    /// A = B == C
    Equal,

    /// A = B != C
    NotEqual,

    /// A = B < C
    LessThan,

    /// A = B <= C
    LessThanEqual,

    /// A = B > C
    GreaterThan,

    /// A = B >= C
    GreaterThanEqual,

    /// A = B in C
    In,

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

    /// IF (B==error) RET B ELSE A = B
    Try,

    /// A = B[C]
    Subscript,

    /// A = error(B)
    BuildError,

    /// A = (B, B + 1, ... B + N)
    BuildTuple,

    /// A = [B, B + 1, ... B + N]
    BuildList,

    /// A = Fn(arg_count, offset)
    BuildFn,

    /// ip = arg1
    Jump,

    /// if (A) ip = arg1
    JumpTrue,

    /// if (not A) ip = arg1
    JumpFalse,

    /// if (not A is error) ip = arg1
    JumpNotError,

    /// A = IMPORT(arg1)
    Import,

    /// A = NATIVE(arg1)
    Native,

    /// A = NATIVE(arg1, arg2)
    NativeExtern,

    /// A = B is TYPEID
    Is,

    /// A = B as TYPEID
    As,

    /// B = A(B, B + 1, ... B + N)
    Call,

    /// 0 = A
    Return,

    /// 0 = ()
    ReturnNone,

    /// TODO better debug info
    LineInfo,

    _,
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
