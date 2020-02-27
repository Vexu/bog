const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ast = @import("ast.zig");
const Node = ast.Node;
const TypeId = @import("value.zig").Value.TypeId;

// TODO give these numbers once they are more stable
pub const Op = enum(u8) {
    /// A <- B
    Move,

    /// A = COPY(B)
    Copy,

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

    /// A = B and C
    BoolAnd,

    /// A = B or C
    BoolOr,

    /// A = not B
    BoolNot,

    /// A = ~B
    BitNot,

    /// A = -B
    Negate,

    /// IF (B==error) RET B ELSE A = B
    Try,

    /// A = B[C]
    Get,

    /// A[B] = C
    Set,

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

    /// A = B is TYPEID
    Is,

    /// A = B as TYPEID
    As,

    /// A = B(C, C + 1, ... C + N)
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

    pub fn dump(module: Module, stream: var) @TypeOf(stream).Child.Error!void {
        var ip: usize = 0;
        while (ip < module.code.len) {
            if (ip == module.start_index) {
                try stream.write("\nentry:");
            }
            const this_offset = ip;
            const op = module.getArg(Op, &ip);
            if (op != .LineInfo) {
                try stream.print("\n {: <5} {} ", .{ this_offset, @tagName(op) });
            }
            switch (op) {
                // A i8
                .ConstPrimitive, .ConstInt8 => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(i8, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A u32
                .JumpTrue, .JumpFalse, .JumpNotError => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(u32, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A i32
                .ConstInt32 => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(i32, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A i64
                .ConstInt64 => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(i64, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A STRING(arg1)
                .Import, .Native, .ConstString => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const offset = module.getArg(u32, &ip);

                    const len = @ptrCast(*align(1) const u32, module.strings[offset..].ptr).*;
                    const slice = module.strings[offset + @sizeOf(u32) ..][0..len];
                    try stream.print(" #{} \"{}\"\n", .{ arg_1, slice });
                },

                // A f64
                .ConstNum => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(f64, &ip);
                    try stream.print(" #{} {}\n", .{ arg_1, arg_2 });
                },

                // A B C
                .DivFloor,
                .Div,
                .Mul,
                .Pow,
                .Mod,
                .Add,
                .Sub,
                .LShift,
                .RShift,
                .BitAnd,
                .BitOr,
                .BitXor,
                .Equal,
                .NotEqual,
                .LessThan,
                .LessThanEqual,
                .GreaterThan,
                .GreaterThanEqual,
                .In,
                .BoolAnd,
                .BoolOr,
                .Get,
                .Set,
                => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(RegRef, &ip);
                    try stream.print(" #{} #{} #{}\n", .{ arg_1, arg_2, arg_3 });
                },

                // A B
                .Move,
                .Copy,
                .BoolNot,
                .BitNot,
                .Negate,
                .Try,
                .BuildError,
                => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    try stream.print(" #{} #{}\n", .{ arg_1, arg_2 });
                },

                // A = (B, B + 1, ... B + N)
                .BuildTuple,
                .BuildList,
                => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(u16, &ip);
                    try stream.print(" #{} #{} arg_count:{}\n", .{ arg_1, arg_2, arg_3 });
                },

                // A = B(C, C + 1, ... C + N)
                .Call => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(RegRef, &ip);
                    const arg_4 = module.getArg(u16, &ip);
                    try stream.print(" #{} #{} #{} arg_count:{}\n", .{ arg_1, arg_2, arg_3, arg_4 });
                },

                // A = Fn(arg_count, offset)
                .BuildFn => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(u8, &ip);
                    const arg_3 = module.getArg(u32, &ip);
                    try stream.print(" #{} arg_count:{} offset:{}\n", .{ arg_1, arg_2, arg_3 });
                },

                // i32
                .Jump => {
                    const arg_1 = module.getArg(i32, &ip);
                    try stream.print(" {}\n", .{arg_1});
                },

                // u32
                .LineInfo => {
                    const arg_1 = module.getArg(u32, &ip);
                },

                // A B TYPEID
                .Is, .As => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    const arg_2 = module.getArg(RegRef, &ip);
                    const arg_3 = module.getArg(TypeId, &ip);
                    try stream.print(" #{} #{} id:{}\n", .{ arg_1, arg_2, @tagName(arg_3) });
                },

                // A
                .Discard, .Return => {
                    const arg_1 = module.getArg(RegRef, &ip);
                    try stream.print(" #{}\n", .{arg_1});
                },

                .ReturnNone => try stream.writeByte('\n'),
                _ => unreachable,
            }
        }
    }

    fn getArg(module: Module, comptime T: type, ip: *usize) T {
        const val = @ptrCast(*align(1) const T, module.code[ip.*..].ptr).*;
        ip.* += @sizeOf(T);
        return val;
    }
};
