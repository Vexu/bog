const Op = enum(u8) {
    /// No-op
    Nop = 0,

    /// A = LOAD(A)
    Load = 1,

    /// A = LOAD(A, B) 
    LoadMemb = 2,

    /// B = A
    Store = 3,

    /// A = DECL(A)
    Decl = 4,

    /// A = DECL(A, B)
    DeclMemb = 5,

    PushRegs,
    PopRes,
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

    _,
};
