test "closures" {
    try expectOutput(
        \\let x = 2
        \\const foo = fn()
        \\    return x + 5
        \\return foo()
    ,
        \\7
    );
    // TODO multilevel captures
    // try expectOutput(
    //     \\let x = 2
    //     \\const foo = fn()
    //     \\    return fn()
    //     \\        return x + 5
    //     \\
    //     \\const bar = foo()
    //     \\return bar()
    // ,
    //     \\7
    // );
}

test "map" {
    try expectOutput(
        \\let y = 2
        \\const map = {1: 2, y}
        \\map["foo"] = "bar"
        \\return map
    ,
        \\{"y": 2, "foo": "bar", 1: 2}
    );
    try expectOutput(
        \\let y = 2
        \\const map = {1: 2, x: y}
        \\const {x} = map
        \\const {x: foo} = map
        \\return x == foo
    ,
        \\true
    );
}

test "property access of list" {
    try expectOutput(
        \\const list = [1, true, "hello"]
        \\return list.len
    ,
        \\3
    );
    try expectOutput(
        \\let y = [1,2,3]
        \\y[-1] = 4
        \\y["len"]
        \\return y
    ,
        \\[1, 2, 4]
    );
}

test "string for iter" {
    try expectOutput(
        \\let sum = 0
        \\for (let c in "hellö wörld")
        \\    if (c == "h") sum += 1
        \\    else if (c == "e") sum += 2
        \\    else if (c == "l") sum += 3
        \\    else if (c == "ö") sum += 4
        \\    else if (c == "w") sum += 5
        \\    else if (c == "d") sum += 6
        \\
        \\return sum
    ,
        \\31
    );
}

test "for loops" {
    try expectOutput(
        \\let sum = 0
        \\for (let x in [1, 2, 3])
        \\    sum += x
        \\
        \\return sum
    ,
        \\6
    );
    try expectOutput(
        \\let sum = 0
        \\for (let (x,y) in [(1,2), (2,3), (5,6)])
        \\    sum += x * y
        \\
        \\return sum
    ,
        \\38
    );
}

test "error destructure" {
    try expectOutput(
        \\const err = fn() error(2)
        \\
        \\let error(y) = err()
        \\return y + 2
    ,
        \\4
    );
}

test "catch capture" {
    try expectOutput(
        \\const err = fn() error(2)
        \\
        \\return err() catch (let foo) foo
    ,
        \\2
    );
}

test "while let" {
    try expectOutput(
        \\const getSome = fn(val)  if (val != 0) val - 1
        \\
        \\let val = 10
        \\while (let newVal = getSome(val))
        \\    val = newVal
        \\return val
    ,
        \\0
    );
}

test "if let" {
    try expectOutput(
        \\const maybeInc = fn(val)
        \\    if (let y = val)
        \\        return y + 4
        \\    return 2
        \\
        \\return maybeInc(()) + maybeInc(4)
    ,
        \\10
    );
}

test "fibonacci" {
    try expectOutput(
        \\const fib = fn(n)
        \\    if (n < 2) return n
        \\    return fib(n - 1) + fib(n-2)
        \\
        \\return fib(10)
    ,
        \\55
    );
}

test "const value not modified by function" {
    try expectOutput(
        \\const x = 2
        \\const inc = fn(n) n += 1
        \\inc(x)
        \\return x
    ,
        \\2
    );
}

test "in" {
    try expectOutput(
        \\let y = [1,2,3]
        \\if (not true in y)
        \\    y[-2] = false
        \\return y == [1, false, 3]
    ,
        \\true
    );
}

test "get/set" {
    try expectOutput(
        \\let y = [1,2,3]
        \\y[-2] = true
        \\return y[1]
    ,
        \\true
    );
}

test "mixed num and int" {
    try expectOutput(
        \\let y = 2
        \\y /= 5
        \\return y
    ,
        \\0.4
    );
    try expectOutput(
        \\let y = 2
        \\y **= 0.5
        \\return y
    ,
        \\1.4142135623730951
    );
}

test "copy on assign" {
    try expectOutput(
        \\const x = 2
        \\let y = x
        \\y += 2
        \\return x
    ,
        \\2
    );
    try expectOutput(
        \\let y = 2
        \\const inc = fn (a) a+=2
        \\inc(y)
        \\return y
    ,
        \\4
    );
}

test "try" {
    try expectOutput(
        \\const err = fn() error("foo")
        \\try err()
    ,
        \\error("foo")
    );
}

test "catch" {
    try expectOutput(
        \\const err = fn() error("foo")
        \\return err() catch "success"
    ,
        \\"success"
    );
}

test "strigs" {
    try expectOutput(
        \\const a = "hello"
        \\return if (a == "world") 2 as str else 1.5 as str
    ,
        \\"1.5"
    );
}

test "comparision" {
    try expectOutput(
        \\let a = 0
        \\while (a != 1000)
        \\    a += 1
        \\return a
    ,
        \\1000
    );
}

test "while loop" {
    try expectOutput(
        \\const cond = true
        \\while (cond)
        \\    if (not cond)
        \\        break
        \\    else
        \\        let x = 2
        \\    break
        \\return true
    ,
        \\true
    );
}

test "subscript" {
    try expectOutput(
        \\const y = (1,2)
        \\return y[-1]
    ,
        \\2
    );
}

test "assert" {
    try expectOutput(
        \\const assert = fn (ok) if (not ok) error(false)
        \\return assert(not false)
    ,
        \\()
    );
}

test "functions" {
    try expectOutput(
        \\const add = fn ((a,b)) a + b
        \\const tuplify = fn (a,b) (a,b)
        \\return add(tuplify(1,2))
    ,
        \\3
    );
    try expectOutput(
        \\const add = fn (a,b) a + b
        \\const sub = fn (a,b) a - b
        \\return sub(add(3,4), add(1,2))
    ,
        \\4
    );
}

test "type casting" {
    try expectOutput(
        \\return 1 as none
    ,
        \\()
    );
    try expectOutput(
        \\return 1 as bool
    ,
        \\true
    );
    try expectOutput(
        \\let y = 2.5
        \\return y as int
    ,
        \\2
    );
}

test "type checking" {
    try expectOutput(
        \\return 1 is int
    ,
        \\true
    );
    try expectOutput(
        \\return 1 is num
    ,
        \\false
    );
    try expectOutput(
        \\return (1,) is tuple
    ,
        \\true
    );
}

test "tuple destructuring" {
    // TODO should destructuring different sized tuples be an error?
    try expectOutput(
        \\let (a, b, _, c) = (1, 2, 3, 4, 5)
        \\return (a + b) * c
    ,
        \\12
    );
}

test "tuple" {
    try expectOutput(
        \\return (1, 2, 3, 4, 22.400)
    ,
        \\(1, 2, 3, 4, 22.4)
    );
}

test "bool if" {
    try expectOutput(
        \\const x = not false
        \\return 3 + if (not x) 2 else if (x) 4 else 9
    ,
        \\7
    );
}

test "assignment" {
    try expectOutput(
        \\let x = 2
        \\let y = -3
        \\x **= -y
        \\return x
    ,
        \\8
    );
}

test "basic math" {
    try expectOutput(
        \\let x = 2
        \\let y = x * 5
        \\let z = 90
        \\return y // x * z
    ,
        \\450
    );
}

test "basic variables" {
    try expectOutput(
        \\let x = 12
        \\return x
    ,
        \\12
    );
    try expectOutput(
        \\let x = true
        \\return not x
    ,
        \\false
    );
}

test "number literals" {
    try expectOutput(
        \\return 12
    ,
        \\12
    );
    try expectOutput(
        \\return 0x12
    ,
        \\18
    );
    try expectOutput(
        \\return 0o12
    ,
        \\10
    );
}

test "constant values" {
    try expectOutput(
        \\return true
    ,
        \\true
    );
    try expectOutput(
        \\return not true
    ,
        \\false
    );
    try expectOutput(
        \\return -12
    ,
        \\-12
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const testing = std.testing;
const bog = @import("bog");
const Vm = bog.Vm;

var buffer: [16 * 1024]u8 = undefined;

fn expectOutput(source: []const u8, expected: []const u8) !void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var vm = try Vm.init(alloc, .{});
    const res = run(alloc, source, &vm) catch |e| switch (e) {
        else => return e,
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            try vm.errors.render(source, std.io.getStdErr().outStream());
            return e;
        },
    };
    if (res) |some| {
        var out_buf = std.ArrayList(u8).init(alloc);
        try some.dump(out_buf.outStream(), 2);
        const result = out_buf.items;
        if (!mem.eql(u8, result, expected)) {
            warn("\n---expected----\n{}\n-----found-----\n{}\n---------------\n", .{ expected, result });
            return error.TestFailed;
        }
    } else {
        return error.TestFailed;
    }
}

fn run(alloc: *mem.Allocator, source: []const u8, vm: *Vm) !?*bog.Value {
    var module = try bog.compile(alloc, source, &vm.errors);

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.entry;
    return try vm.exec(module);
}
