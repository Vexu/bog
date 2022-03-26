const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

const List = @This();

inner: extern struct {
    len: u32 align(@alignOf(Value)),
    capacity: u32,
},

pub fn items(l: *const List) []Value {
    return (@ptrCast([*]Value, l.trailing[0]) + 1)[0..l.len];
}