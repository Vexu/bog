const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;

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

pub const Instruction = packed struct {
    op: Op,
    A: RegRef = 0,
    B: RegRef = 0,
    C: RegRef = 0,
};

pub const Code = std.ArrayList(u32);

pub const Module = struct {
    sect_funcs: []const u32,
    sect_values: []const u64,
    sect_strings: []const u8,
};

pub const RegRef = u8;

const FuncState = struct {
    frame_size: u8 = 0,
    regs: [250]RegState = [_]RegState{.Free} ** 250,
    // parent_ref: ?u8,
    code: Code,

    const RegState = enum {
        Free,
        Local,
        Temp,
    };

    fn init(allocator: *Allocator) FuncState {
        return .{
            .code = Code.init(allocator),
        };
    }

    fn deinit(self: *FuncState) void {
        self.code.deinit();
    }

    fn registerAlloc(self: *FuncState, kind: RegState) !RegRef {
        for (self.regs) |r, i| {
            if (r == .Free) {
                self.regs[i] = kind;
                if (i > self.frame_size) {
                    self.frame_size = @truncate(u8, i);
                }
                return @truncate(RegRef, i);
            }
        }
        return error.StackOverflow;
    }

    fn freeRegisterIfTemp(self: *FuncState, reg: RegRef) void {
        if (self.regs[reg] == .Temp)
            self.regs[reg] = .Free;
    }

    fn emitInstruction(self: *FuncState, inst: Instruction, arg: ?u32) !void {
        try self.code.append(@bitCast(u32, inst));
        if (arg) |some| {
            try self.code.append(some);
        }
    }
};

const FuncList = std.SegmentedList(FuncState, 4);

pub const Builder = struct {
    funcs: FuncList,
    syms: SymbolList,
    jumps: JumpList,
    cur_func: *FuncState,

    const Symbol = struct {
        name: []const u8,
        func: *FuncState,
        reg: RegRef,
    };

    const SymbolList = std.SegmentedList(Symbol, 8);
    const JumpList = std.SegmentedList(usize, 8);

    pub fn init(builder: *Builder, allocator: *Allocator) !void {
        // https://github.com/ziglang/zig/issues/2765 pls
        builder.funcs = FuncList.init(allocator);
        builder.cur_func = try builder.funcs.addOne();
        builder.cur_func.* = FuncState.init(allocator);
        builder.syms = SymbolList.init(allocator);
        builder.jumps = JumpList.init(allocator);
    }

    pub fn deinit(self: *Builder) void {
        self.syms.deinit();
        var it = self.funcs.iterator(0);
        while (it.next()) |f| f.deinit();
        self.funcs.deinit();
    }

    fn putString(builder: *Builder, tok: *Token) !u32 {
        return error.Unimplemented;
    }

    pub fn discard(self: *Builder, reg: RegRef) !void {
        defer self.cur_func.freeRegisterIfTemp(reg);
        try self.cur_func.emitInstruction(.{
            .op = .Discard,
            .A = reg,
        }, null);
    }

    pub fn move(self: *Builder, from: RegRef, to: RegRef) !void {
        defer self.cur_func.freeRegisterIfTemp(from);
        try self.cur_func.emitInstruction(.{
            .op = .Move,
            .A = to,
            .B = from,
        }, null);
    }

    pub fn jumpFalse(self: *Builder, reg: RegRef) !void {
        try self.cur_func.emitInstruction(.{
            .op = .JumpFalse,
        }, 0);
        try self.jumps.push(self.cur_func.code.len - 1);
    }

    pub fn jumpTrue(self: *Builder, reg: RegRef) !void {
        try self.cur_func.emitInstruction(.{
            .op = .JumpTrue,
        }, 0);
        try self.jumps.push(self.cur_func.code.len - 1);
    }

    pub fn jumpNotErr(self: *Builder, reg: RegRef) !void {
        try self.cur_func.emitInstruction(.{
            .op = .JumpNotErr,
        }, 0);
        try self.jumps.push(self.cur_func.code.len - 1);
    }

    pub fn finishJumps(self: *Builder, count: u32) void {
        var i = count;
        while (i != 0) : (i -= 1) {
            const index = self.jumps.pop().?;
            const diff = @truncate(u32, self.cur_func.code.len - index);
            self.cur_func.code.toSlice()[index] = diff;
        }
    }

    pub fn constant(self: *Builder, tok: *Token) !RegRef {
        const reg = try self.cur_func.registerAlloc(.Temp);
        var arg: ?u32 = null;
        var breg: RegRef = 0;
        const op: Op = switch (tok.id) {
            .String => blk: {
                arg = try self.putString(tok);
                break :blk .ConstString;
            },
            .Number => return error.Unimplemented,
            .Keyword_false, .Keyword_true => blk: {
                breg = @boolToInt(tok.id == .Keyword_true);
                break :blk .ConstBool;
            },
            .Integer => |val| if (val <= std.math.maxInt(u32)) blk: {
                // fits in u32
                arg = @truncate(u32, val);
                break :blk .ConstSmallInt;
            } else {
                return error.Unimplemented;
            },
            .RParen => .ConstNone,
            else => unreachable,
        };
        try self.cur_func.emitInstruction(.{
            .op = op,
            .A = reg,
            .B = breg,
        }, arg);
        return reg;
    }

    pub fn declRef(self: *Builder, tok: *Token) !RegRef {
        const name = tok.id.Identifier;
        var it = self.syms.iterator(self.syms.len);
        const found = while (it.prev()) |sym| {
            if (mem.eql(u8, sym.name, name))
                break sym;
        } else {
            return error.UndeclaredIdentifier;
        };
        if (found.func != self.cur_func) {
            // TODO pushStack
            return error.Unimplemented;
        } else {
            return found.reg;
        }
    }

    pub fn buildErr(self: *Builder, tok: *Token, val: RegRef) !RegRef {
        try self.cur_func.emitInstruction(.{
            .op = .BuildError,
            .A = val,
        }, null);
        return val;
    }

    pub fn buildList(self: *Builder, tok: *Token) !usize {
        // todo allocate register here?
        const list = 1;
        std.debug.warn("buildList {}\n", .{list});
        return error.Unimplemented;
        // return list;
    }

    pub fn finishList(self: *Builder, tok: *Token, list: usize) !RegRef {
        const reg = 1; //self.registerAlloc();
        std.debug.warn("#finishList {}\n", .{list});
        return error.Unimplemented;
        // return reg;
    }

    pub fn listPush(self: *Builder, val: RegRef) !void {
        // defer self.registerFree(val);
        std.debug.warn("listPush {}\n", .{val});
        return error.Unimplemented;
    }

    pub fn import(self: *Builder, tok: *Token, str: *Token) !RegRef {
        const reg = try self.cur_func.registerAlloc(.Temp);
        try self.cur_func.emitInstruction(.{
            .op = .Import,
            .A = reg,
        }, try self.putString(str));
        return reg;
    }

    pub fn prefix(self: *Builder, tok: *Token, rhs: RegRef) !RegRef {
        const loc = try self.cur_func.registerAlloc(.Temp);
        try self.cur_func.emitInstruction(.{
            .op = switch (tok.id) {
                .Keyword_not => .Not,
                .Tilde => .BinNot,
                .Minus => .Negate,
                .Keyword_try => .Try,
                // TODO maybe don't no-op this
                .Plus => return rhs,
                else => unreachable,
            },
            .A = loc,
            .B = rhs,
        }, null);
        self.cur_func.freeRegisterIfTemp(rhs);
        return rhs;
    }

    pub fn cast(self: *Builder, lhs: RegRef, tok: *Token) !RegRef {
        std.debug.warn("#{} as {}\n", .{ lhs, tok.id.Identifier });
        return error.Unimplemented;
    }

    pub fn infix(self: *Builder, lhs: RegRef, tok: *Token, rhs: RegRef) !RegRef {
        const loc = try self.cur_func.registerAlloc(.Temp);
        try self.cur_func.emitInstruction(.{
            .op = switch (tok.id) {
                .SlashSlash => .DivFloor,
                .Slash => .Div,
                .Asterisk => .Mul,
                .AsteriskAsterisk => .Pow,
                .Percent => .Mod,
                .Plus => .Add,
                .Minus => .Sub,
                .LArrArr => .LShift,
                .RArrArr => .RShift,
                .Keyword_and => .And,
                .Keyword_or => .Or,
                .Ampersand => .BinAnd,
                .Pipe => .BinOr,
                // .LArr, // TODO
                // .LArrEqual,
                // .RArr,
                // .RArrEqual,
                // .EqualEqual,
                // .BangEqual,
                // .Keyword_in,
                // .Keyword_is,
                else => unreachable,
            },
            .A = loc,
            .B = lhs,
            .C = rhs,
        }, null);
        self.cur_func.freeRegisterIfTemp(rhs);
        self.cur_func.freeRegisterIfTemp(lhs);
        return loc;
    }

    pub fn assign(self: *Builder, lhs: RegRef, tok: *Token, rhs: RegRef) !void {
        // defer self.registerFree(rhs);
        // defer self.registerFree(lhs);
        std.debug.warn("#{} {} #{}\n", .{ lhs, tok, rhs });
        return error.Unimplemented;
    }
};
