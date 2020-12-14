test "continue" {
    expectOutput(
        \\let i = 0
        \\while (i < 1)
        \\    i += 1
        \\    continue
    , "()");
    expectOutput(
        \\for (let i in 0:1)
        \\    continue
    , "()");
}

test "std.gc" {
    if (std.builtin.os.tag == .windows) {
        // TODO this gives a different result on windows
        return error.SkipZigTest;
    }
    expectOutput(
        \\const {collect} = import("std.gc")
        \\const json = import("std.json")
        \\
        \\const makeGarbage = fn()
        \\    json.stringify({"a": [2, "foo", ()]})
        \\
        \\for (0:5) makeGarbage()
        \\return collect()
    ,
        \\42
    );
}

test "std.json" {
    expectOutput(
        \\const json = import("std.json")
        \\return json.parse("{\"a\":[2,\"foo\",null]}")
    ,
        \\{"a": [2, "foo", ()]}
    );
    expectOutput(
        \\const json = import("std.json")
        \\return json.stringify({"a": [2, "foo", ()]})
    ,
        \\"{\"a\":[2,\"foo\",null]}"
    );
}

test "boolean short-circuit" {
    expectOutput(
        \\const foo = fn() true
        \\const bar = fn() error("should not be evaluated")
        \\return foo() or bar()
    ,
        \\true
    );
    expectOutput(
        \\const foo = fn() false
        \\const bar = fn() error("should not be evaluated")
        \\return foo() and bar()
    ,
        \\false
    );
}

test "match" {
    expectOutput(
        \\const getNum = fn (arg)
        \\    return match (arg)
        \\        1 => 69
        \\        12 => 42
        \\        10004 => 17
        \\        _ => 0
        \\
        \\let arr = []
        \\arr ++ getNum(1)
        \\arr ++ getNum(12)
        \\arr ++ getNum(10004)
        \\arr ++ getNum(9)
        \\return arr
    ,
        \\[69, 42, 17, 0]
    );
    expectOutput(
        \\const getNum = fn() 42
        \\
        \\match (getNum())
        \\    1 => error
        \\    let val => return val
    ,
        \\42
    );
}

test "try catch" {
    expectOutput(
        \\const fails_on_1 = fn(arg) if (arg == 1) error(69)
        \\const fails_on_2 = fn(arg) if (arg == 2) error(42)
        \\const fails_on_3 = fn(arg) if (arg == 3) error(17)
        \\
        \\const foo = fn(arg)
        \\    try
        \\        fails_on_1(arg)
        \\        fails_on_2(arg)
        \\        fails_on_3(arg)
        \\    catch (let err)
        \\        return err
        \\
        \\    return 99
        \\
        \\return for (let i in 0:4) foo(i)
    ,
        \\[99, 69, 42, 17]
    );
}

test "string join" {
    expectOutput(
        \\return ",".join([1 as str, "bar", [2] as str])
    ,
        \\"1,bar,[2]"
    );
}

test "format string" {
    expectOutput(
        \\return f"foo{255:X}bar"
    ,
        \\"fooFFbar"
    );
    expectOutput(
        \\return "foo{X}bar".format((255,))
    ,
        \\"fooFFbar"
    );
}

test "concatting" {
    expectOutput(
        \\let x = "foo"
        \\return x ++ "bar" ++ "baz"
    ,
        \\"foobarbaz"
    );
    expectOutput(
        \\let x = []
        \\return x ++ 1 ++ "bar" ++ 2
    ,
        \\[1, "bar", 2]
    );
    expectOutput(
        \\return [[1] as str]
    ,
        \\["[1]"]
    );
    expectOutput(
        \\let x = "foo"
        \\x ++ "bar" ++ "baz"
        \\return x
    ,
        \\"foobarbaz"
    );
}

test "comma decimals" {
    expectOutput(
        \\return 0,5 + 0,2;
    ,
        \\0.7
    );
}

test "range" {
    expectOutput(
        \\return for (let i in 0:7:2) i
    ,
        \\[0, 2, 4, 6]
    );
    expectOutput(
        \\return 1 in 0:2
    ,
        \\true
    );
    expectOutput(
        \\return 1 in 0:2:2
    ,
        \\false
    );
}

test "list comprehension as function argument" {
    expectOutput(
        \\const map = {1: 2, 3: 4, 5: 6}
        \\return (fn(l)l)(for (let (k, v) in map) {key: k, val: v})
    ,
        \\[{"key": 1, "val": 2}, {"key": 3, "val": 4}, {"key": 5, "val": 6}]
    );
}

test "map iterator" {
    expectOutput(
        \\const map = {1: 2, 3: 4, 5: 6}
        \\let sum = 0
        \\for (let (k, v) in map)
        \\    sum += k
        \\    sum *= v
        \\return sum
    ,
        \\150
    );
}

test "list comprehension" {
    expectOutput(
        \\return for (const c in "hello") c
    ,
        \\["h", "e", "l", "l", "o"]
    );
    expectOutput(
        \\let i = 0
        \\return while (i < 10)
        \\    i += 1
        \\    i
    ,
        \\[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    );
}

test "list.append" {
    expectOutput(
        \\let list = [1, 2]
        \\list.append(3)
        \\return list
    ,
        \\[1, 2, 3]
    );
}

test "std.map" {
    expectOutput(
        \\let val = {foo: 2, bar: 3, 0: 515, [1]: [2]}
        \\const map = import("std.map")
        \\const {assert} = import("std.debug")
        \\const keys = map.keys(val)
        \\assert(keys is list and keys.len == 4)
        \\const values = map.values(val)
        \\assert(values is list and values.len == 4)
        \\const entries = map.entries(val)
        \\assert(entries is list and entries.len == 4)
        \\const entry = entries[0]
        \\assert(entry is map and map.size(entry) == 2)
    ,
        \\()
    );
}

test "collections copy hold values" {
    expectOutput(
        \\let x = [0]
        \\let y = [x]
        \\x[0] = 1
        \\return y
    ,
        \\[[0]]
    );
    expectOutput(
        \\const foo = [1]
        \\const bar = fn(list) list[0] = 2
        \\bar(foo)
        \\return foo
    ,
        \\[1]
    );
}

test "tagged values" {
    expectOutput(
        \\return @something{foo: 69}
    ,
        \\@something{"foo": 69}
    );
    expectOutput(
        \\const foo = @foo
        \\const bar = @bar
        \\return foo == bar
    ,
        \\false
    );
    expectOutput(
        \\const foo = @foo[1]
        \\const bar = @foo[1]
        \\return foo == bar
    ,
        \\true
    );
    expectOutput(
        \\const foo = @foo[1, 2]
        \\const @foo[bar, baz] = foo
        \\return bar + baz
    ,
        \\3
    );
}

test "error map" {
    expectOutput(
        \\const foo = error{a: 2}
        \\const error{a: bar} = foo
        \\return bar * 2
    ,
        \\4
    );
}

test "call bog function" {
    expectCallOutput(
        \\return {
        \\    foo: 2,
        \\    doTheTest: fn(num) this.foo + num
        \\}
    , .{1},
        \\3
    );
}

test "containers do not overwrite memoized values" {
    expectOutput(
        \\let x = [true]
        \\x[0] = 1
        \\return true
    ,
        \\true
    );
}

test "this" {
    expectOutput(
        \\let x = {
        \\    a: 69,
        \\    y: 420,
        \\    foo: fn() (
        \\        [0][0] # last_get is now referencing this list
        \\        return this.a * this.y
        \\    ),
        \\
        \\}
        \\return x.foo()
    ,
        \\28980
    );
}

test "closures" {
    expectOutput(
        \\let x = 2
        \\const foo = fn() x + 5
        \\return foo()
    ,
        \\7
    );
    expectOutput(
        \\let x = 2
        \\const foo = fn()
        \\    return fn()
        \\        return x + 5
        \\
        \\const bar = foo()
        \\return bar()
    ,
        \\7
    );
}

test "map" {
    expectOutput(
        \\let y = 2
        \\const map = {1: 2, y}
        \\map["foo"] = "bar"
        \\return map
    ,
        \\{1: 2, "y": 2, "foo": "bar"}
    );
    expectOutput(
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
    expectOutput(
        \\const list = [1, true, "hello"]
        \\return list.len
    ,
        \\3
    );
    expectOutput(
        \\let y = [1,2,3]
        \\y[-1] = 4
        \\y["len"]
        \\return y
    ,
        \\[1, 2, 4]
    );
}

test "string for iter" {
    expectOutput(
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
    expectOutput(
        \\let sum = 0
        \\for (let x in [1, 2, 3])
        \\    sum += x
        \\
        \\return sum
    ,
        \\6
    );
    expectOutput(
        \\let sum = 0
        \\for (let (x,y) in [(1, 2), (2, 3), (5, 6)])
        \\    sum += x * y
        \\
        \\return sum
    ,
        \\38
    );
}

test "error destructure" {
    expectOutput(
        \\const err = fn() error(2)
        \\
        \\let error(y) = err()
        \\return y + 2
    ,
        \\4
    );
}

test "while let" {
    expectOutput(
        \\const getSome = fn(val) if (val != 0) val - 1
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
    expectOutput(
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
    expectOutput(
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
    expectOutput(
        \\const x = 2
        \\const inc = fn(n) n += 1
        \\inc(x)
        \\return x
    ,
        \\2
    );
}

test "in" {
    expectOutput(
        \\let y = [1, 2, 3]
        \\if (not true in y)
        \\    y[-2] = false
        \\return y == [1, false, 3]
    ,
        \\true
    );
}

test "get/set" {
    expectOutput(
        \\let y = [1, 2, 3]
        \\y[-2] = true
        \\return y[1]
    ,
        \\true
    );
}

test "mixed num and int" {
    expectOutput(
        \\let y = 2
        \\y /= 5
        \\return y
    ,
        \\0.4
    );
    expectOutput(
        \\let y = 2
        \\y **= 0.5
        \\return y
    ,
        \\1.4142135623730951
    );
}

test "copy on assign" {
    expectOutput(
        \\const x = 2
        \\let y = x
        \\y += 2
        \\return x
    ,
        \\2
    );
    expectOutput(
        \\let y = 2
        \\const inc = fn (a) a+=2
        \\inc(y)
        \\return y
    ,
        \\4
    );
}

test "strings" {
    expectOutput(
        \\const a = "hello"
        \\return if (a == "world") 2 as str else 1.5 as str
    ,
        \\"1.5"
    );
}

test "comparison" {
    expectOutput(
        \\let a = 0
        \\while (a != 1000)
        \\    a += 1
        \\return a
    ,
        \\1000
    );
}

test "while loop" {
    expectOutput(
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
    expectOutput(
        \\const y = (1, 2)
        \\return y[-1]
    ,
        \\2
    );
}

test "assert" {
    expectOutput(
        \\const assert = fn (ok) if (not ok) error(false)
        \\return assert(not false)
    ,
        \\()
    );
}

test "functions" {
    expectOutput(
        \\const add = fn ((a,b)) a + b
        \\const tuplify = fn (a,b) (a,b)
        \\return add(tuplify(1, 2))
    ,
        \\3
    );
    expectOutput(
        \\const add = fn (a,b) a + b
        \\const sub = fn (a,b) a - b
        \\return sub(add(3, 4), add(1,2))
    ,
        \\4
    );
}

test "type casting" {
    expectOutput(
        \\return 1 as none
    ,
        \\()
    );
    expectOutput(
        \\return 1 as bool
    ,
        \\true
    );
    expectOutput(
        \\let y = 2.5
        \\return y as int
    ,
        \\2
    );
    expectOutput(
        \\let x = 2.5 as int
        \\let y = x as num
        \\return y
    ,
        \\2
    );
}

test "type checking" {
    expectOutput(
        \\return 1 is int
    ,
        \\true
    );
    expectOutput(
        \\return 1 is num
    ,
        \\false
    );
    expectOutput(
        \\return (1,) is tuple
    ,
        \\true
    );
}

test "tuple destructuring" {
    // TODO should destructuring different sized tuples be an error?
    expectOutput(
        \\let (a, b, _, c) = (1, 2, 3, 4, 5)
        \\return (a + b) * c
    ,
        \\12
    );
}

test "tuple" {
    expectOutput(
        \\return (1, 2, 3, 4, 22.400)
    ,
        \\(1, 2, 3, 4, 22.4)
    );
}

test "bool if" {
    expectOutput(
        \\const x = not false
        \\return 3 + if (not x) 2 else if (x) 4 else 9
    ,
        \\7
    );
}

test "assignment" {
    expectOutput(
        \\let x = 2
        \\let y = -3
        \\x **= -y
        \\return x
    ,
        \\8
    );
}

test "basic math" {
    expectOutput(
        \\let x = 2
        \\let y = x * 5
        \\let z = 90
        \\return y // x * z
    ,
        \\450
    );
}

test "basic variables" {
    expectOutput(
        \\let x = 12
        \\return x
    ,
        \\12
    );
    expectOutput(
        \\let x = true
        \\return not x
    ,
        \\false
    );
}

test "number literals" {
    expectOutput(
        \\return 12
    ,
        \\12
    );
    expectOutput(
        \\return 0x12
    ,
        \\18
    );
    expectOutput(
        \\return 0o12
    ,
        \\10
    );
}

test "constant values" {
    expectOutput(
        \\return true
    ,
        \\true
    );
    expectOutput(
        \\return not true
    ,
        \\false
    );
    expectOutput(
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

fn expectCallOutput(source: []const u8, args: anytype, expected: []const u8) void {
    var vm = Vm.init(std.testing.allocator, .{});
    defer vm.deinit();
    var module: *bog.Module = undefined;
    const res = run(&module, source, &vm) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            vm.errors.render(source, std.io.getStdErr().outStream()) catch {};
            @panic("test failure");
        },
    };
    defer module.deinit(std.testing.allocator);

    const call_res = vm.call(res, "doTheTest", args) catch |e| switch (e) {
        else => @panic("test failure"),
        error.RuntimeError => {
            vm.errors.render(source, std.io.getStdErr().outStream()) catch {};
            @panic("test failure");
        },
    };
    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    call_res.dump(out_buf.outStream(), 2) catch @panic("test failure");
    testing.expectEqualStrings(expected, out_buf.items);
}

fn expectOutput(source: []const u8, expected: []const u8) void {
    var vm = Vm.init(std.testing.allocator, .{});
    defer vm.deinit();
    vm.addStd() catch unreachable;
    var module: *bog.Module = undefined;
    const res = run(&module, source, &vm) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
            vm.errors.render(source, std.io.getStdErr().outStream()) catch {};
            @panic("test failure");
        },
    };
    defer module.deinit(std.testing.allocator);

    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    res.dump(out_buf.outStream(), 2) catch @panic("test failure");
    testing.expectEqualStrings(expected, out_buf.items);
}

fn run(mp: **bog.Module, source: []const u8, vm: *Vm) !*bog.Value {
    const module = try bog.compile(std.testing.allocator, source, &vm.errors);
    errdefer module.deinit(std.testing.allocator);
    mp.* = module;

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.entry;
    return try vm.exec(module);
}
