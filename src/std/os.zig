const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub const name = Value.string(@tagName(@import("builtin").os.tag));
