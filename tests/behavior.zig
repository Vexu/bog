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
    // TODO should be able to refer to fib directly
    try expectOutput(
        \\const fib = fn(f,n)
        \\    if (n < 2) return n
        \\    return f(f, n - 1) + f(f, n-2)
        \\
        \\return fib(fib, 10)
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

var buffer: [10 * 1024]u8 = undefined;

fn expectOutput(source: []const u8, expected: []const u8) !void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var vm = Vm.init(alloc, false);
    const res = run(alloc, source, &vm) catch |e| switch (e) {
        else => return e,
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            const stream = &std.io.getStdErr().outStream().stream;
            try vm.errors.render(source, stream);
            return e;
        },
    };
    if (res) |some| {
        var out_buf = try std.Buffer.initSize(alloc, 0);
        var out_stream = std.io.BufferOutStream.init(&out_buf);
        try some.dump(&out_stream.stream, 2);
        const result = out_buf.toSliceConst();
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
    vm.ip = module.start_index;
    return try vm.exec(&module);
}
