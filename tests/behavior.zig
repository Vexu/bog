test "suspend/resume" {
    try expectOutput(
        \\let mut count = 0
        \\let foo = fn()
        \\    count += 1
        \\    suspend
        \\    count += 1
        \\
        \\let foo_frame = async foo()
        \\if count != 1 throw "bad"
        \\resume foo_frame
        \\if count != 2 throw "bad"
    , "null");
}

test "simple values are duped before being added to an aggregate value" {
    try expectOutput(
        \\let b = []
        \\for 0:2
        \\    let mut a = 1
        \\    b.append(a)
        \\    a += 1
        \\return b
    , "[1, 1]");
}

test "cur_fn == null doesn't imply that identifier should be made global" {
    try expectOutput(
        \\let x = for let v in "oo" v
        \\let b = fn()
        \\    let v = 1
        \\    return v
        \\return b()
    , "1");
}

test "spread" {
    try expectOutput(
        \\let foo = [1, 2, 3]
        \\let bar = [...foo, 4, 5, 6]
        \\return bar
    , "[1, 2, 3, 4, 5, 6]");

    try expectOutput(
        \\let strs = ["foo", "bar"]
        \\let join1 = "".join(...strs)
        \\let join2 = "".join("foo", "bar")
        \\return join1 == join2
    , "true");

    try expectOutput(
        \\let foo = [1, 2, 3]
        \\let bar = fn(a, b, c...) [a, b, ...c]
        \\if bar(...foo) != [1, 2, 3] throw "bad"
        \\if bar(4, ...foo) != [4, 1, 2, 3] throw "bad"
        \\if bar(4, 5, ...foo) != [4, 5, 1, 2, 3] throw "bad"
        \\if bar(4, 5, 6, ...foo) != [4, 5, 6, 1, 2, 3] throw "bad"
        \\if bar(4, 5, 6, 7, ...foo) != [4, 5, 6, 7, 1, 2, 3] throw "bad"
    , "null");

    try expectOutput(
        \\let foo = [1, 2, 3]
        \\let str = "{} + {} = {}"
        \\return str.format(...foo)
    , "\"1 + 2 = 3\"");

    try expectOutput(
        \\let a = [1, 2, 3, 4, 5]
        \\let [b, c, ...rest] = a
        \\return rest
    , "[3, 4, 5]");
}

test "variadic functions" {
    try expectOutput(
        \\let foo = fn(_, args...) args
        \\return foo(1, 2, 3, 4, 5)
    , "[2, 3, 4, 5]");
}

test "capture" {
    try expectOutput(
        \\let foo = fn()
        \\    let a = 1
        \\    return fn() 
        \\        return a + 1
        \\
        \\return foo()()
    , "2");
}

test "nested for loop" {
    try expectOutput(
        \\let mut i = 0
        \\for 1:5
        \\    for 1:5
        \\        i += 1
        \\return i
    , "16");
}

test "range destructuring" {
    try expectOutput(
        \\let (start:end:step) = 1:2:3
        \\return start+end+step
    , "6");
}

test "continue" {
    try expectOutput(
        \\let mut i = 0
        \\while i < 1
        \\    i += 1
        \\    continue
    , "null");
    try expectOutput(
        \\for let i in 0:1
        \\    continue
    , "null");
}

test "std.gc" {
    if (@import("builtin").os.tag == .windows) {
        // TODO this gives a different result on windows
        return error.SkipZigTest;
    }
    try expectOutput(
        \\let {collect} = import("std.gc")
        \\let json = import("std.json")
        \\
        \\let makeGarbage = fn()
        \\    json.stringify({"a" = [2, "foo", null]})
        \\
        \\for 0:5 makeGarbage()
        \\return collect()
    ,
        \\53
    );
}

test "std.json" {
    try expectOutput(
        \\let json = import("std.json")
        \\return json.parse("{\"a\":[2,\"foo\",null]}")
    ,
        \\{"a" = [2, "foo", null]}
    );
    try expectOutput(
        \\let json = import("std.json")
        \\return json.stringify({"a" = [2, "foo", null]})
    ,
        \\"{\"a\":[2,\"foo\",null]}"
    );
}

test "boolean short-circuit" {
    try expectOutput(
        \\let foo = fn() true
        \\let bar = fn() error("should not be evaluated")
        \\return foo() or bar()
    ,
        \\true
    );
    try expectOutput(
        \\let foo = fn() false
        \\let bar = fn() error("should not be evaluated")
        \\return foo() and bar()
    ,
        \\false
    );
}

test "match" {
    try expectOutput(
        \\let getNum = fn (arg)
        \\    return match (arg)
        \\        1, 2 => 69
        \\        12 => 42
        \\        10004 => 17
        \\        _ => 0
        \\
        \\let arr = []
        \\arr.append(getNum(1))
        \\arr.append(getNum(2))
        \\arr.append(getNum(12))
        \\arr.append(getNum(10004))
        \\arr.append(getNum(9))
        \\return arr
    ,
        \\[69, 69, 42, 17, 0]
    );
    try expectOutput(
        \\let getNum = fn() 42
        \\
        \\match getNum()
        \\    1 => error
        \\    let val => return val
    ,
        \\42
    );
}

test "try catch" {
    try expectOutput(
        \\let fails_on_1 = fn(arg) if (arg == 1) error(69)
        \\let fails_on_2 = fn(arg) if (arg == 2) error(42)
        \\let fails_on_3 = fn(arg) if (arg == 3) error(17)
        \\
        \\let foo = fn(arg)
        \\    try
        \\        fails_on_1(arg)
        \\        fails_on_2(arg)
        \\        fails_on_3(arg)
        \\    catch let err
        \\        return err
        \\
        \\    return 99
        \\
        \\return for let i in 0:4 foo(i)
    ,
        \\[99, 69, 42, 17]
    );
}

test "string join" {
    try expectOutput(
        \\return ",".join(1 as str, "bar", [2] as str)
    ,
        \\"1,bar,[2]"
    );
}

test "format string" {
    try expectOutput(
        \\return f"foo{255:X}bar"
    ,
        \\"fooFFbar"
    );
    try expectOutput(
        \\return "foo{X}bar".format(255)
    ,
        \\"fooFFbar"
    );
}

test "range" {
    try expectOutput(
        \\return for let i in 0:7:2 i
    ,
        \\[0, 2, 4, 6]
    );
    try expectOutput(
        \\return 1 in 0:2
    ,
        \\true
    );
    try expectOutput(
        \\return 1 in 0:2:2
    ,
        \\false
    );
}

test "list comprehension as function argument" {
    try expectOutput(
        \\let map = {1 = 2, 3 = 4, 5 = 6}
        \\return (fn(l)l)(for let (k, v) in map {key = k, val = v})
    ,
        \\[{"key" = 1, "val" = 2}, {"key" = 3, "val" = 4}, {"key" = 5, "val" = 6}]
    );
}

test "map iterator" {
    try expectOutput(
        \\let map = {1 = 2, 3 = 4, 5 = 6}
        \\let mut sum = 0
        \\for let (k, v) in map
        \\    sum += k
        \\    sum *= v
        \\return sum
    ,
        \\150
    );
}

test "list comprehension" {
    try expectOutput(
        \\return for let c in "hello" c
    ,
        \\["h", "e", "l", "l", "o"]
    );
    try expectOutput(
        \\let mut i = 0
        \\return while (i < 10)
        \\    i += 1
        \\    i
    ,
        \\[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    );
}

test "list.append" {
    try expectOutput(
        \\let list = [1, 2]
        \\list.append(3)
        \\return list
    ,
        \\[1, 2, 3]
    );
}

test "std.map" {
    try expectOutput(
        \\let val = {foo = 2, bar = 3, 0 = 515, [1] = [2]}
        \\let map = import("std.map")
        \\let {assert} = import("std.debug")
        \\let keys = map.keys(val)
        \\assert(keys is list and keys.len == 4)
        \\let values = map.values(val)
        \\assert(values is list and values.len == 4)
        \\let entries = map.entries(val)
        \\assert(entries is list and entries.len == 4)
        \\let entry = entries[0]
        \\assert(entry is map and map.size(entry) == 2)
    ,
        \\null
    );
}

test "modifying collections" {
    try expectOutput(
        \\let x = [0]
        \\let y = [x]
        \\x[0] = 1
        \\return y
    ,
        \\[[1]]
    );
    try expectOutput(
        \\let foo = [1]
        \\let bar = fn(list) list[0] = 2
        \\bar(foo)
        \\return foo
    ,
        \\[2]
    );
}

test "tagged values" {
    try expectOutput(
        \\return @something{foo = 69}
    ,
        \\@something{"foo" = 69}
    );
    try expectOutput(
        \\let foo = @foo
        \\let bar = @bar
        \\return foo == bar
    ,
        \\false
    );
    try expectOutput(
        \\let foo = @foo[1]
        \\let bar = @foo[1]
        \\return foo == bar
    ,
        \\true
    );
    try expectOutput(
        \\let foo = @foo[1, 2]
        \\let @foo[bar, baz] = foo
        \\return bar + baz
    ,
        \\3
    );
}

test "error map" {
    try expectOutput(
        \\let foo = error{a = 2}
        \\let error{a = bar} = foo
        \\return bar * 2
    ,
        \\4
    );
}

test "call bog function" {
    try expectCallOutput(
        \\return {
        \\    foo = 2,
        \\    doTheTest = fn(num) this.foo + num
        \\}
    , .{1},
        \\3
    );
}

test "containers do not overwrite memoized values" {
    try expectOutput(
        \\let x = [true]
        \\x[0] = 1
        \\return true
    ,
        \\true
    );
}

test "this" {
    try expectOutput(
        \\let x = {
        \\    a = 69,
        \\    y = 420,
        \\    foo = fn() (
        \\        [0][0]
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
    try expectOutput(
        \\let x = 2
        \\let foo = fn() x + 5
        \\return foo()
    ,
        \\7
    );
    try expectOutput(
        \\let x = 2
        \\let foo = fn()
        \\    return fn()
        \\        return x + 5
        \\
        \\let bar = foo()
        \\return bar()
    ,
        \\7
    );
}

test "map" {
    try expectOutput(
        \\let y = 2
        \\let map = {1 = 2, y}
        \\map["foo"] = "bar"
        \\return map
    ,
        \\{1 = 2, "y" = 2, "foo" = "bar"}
    );
    try expectOutput(
        \\let y = 2
        \\let map = {1 = 2, x = y}
        \\let {x} = map
        \\let {x = foo} = map
        \\return x == foo
    ,
        \\true
    );
}

test "property access of list" {
    try expectOutput(
        \\let list = [1, true, "hello"]
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
        \\let mut sum = 0
        \\for let c in "hellö wörld"
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
        \\let mut sum = 0
        \\for let x in [1, 2, 3]
        \\    sum += x
        \\
        \\return sum
    ,
        \\6
    );
    try expectOutput(
        \\let mut sum = 0
        \\for let (x,y) in [(1, 2), (2, 3), (5, 6)]
        \\    sum += x * y
        \\
        \\return sum
    ,
        \\38
    );
}

test "error destructure" {
    try expectOutput(
        \\let err = fn() error(2)
        \\
        \\let error(y) = err()
        \\return y + 2
    ,
        \\4
    );
}

test "while let" {
    try expectOutput(
        \\let getSome = fn(val) if val != 0 val - 1
        \\
        \\let mut val = 10
        \\while let newVal = getSome(val)
        \\    val = newVal
        \\return val
    ,
        \\0
    );
}

test "if let" {
    try expectOutput(
        \\let maybeInc = fn(val)
        \\    if let y = val
        \\        return y + 4
        \\    return 2
        \\
        \\return maybeInc(null) + maybeInc(4)
    ,
        \\10
    );
}

test "fibonacci" {
    try expectOutput(
        \\let fib = fn(n)
        \\    if n < 2 return n
        \\    return fib(n - 1) + fib(n-2)
        \\
        \\return fib(10)
    ,
        \\55
    );
}

test "const value not modified by function" {
    try expectOutput(
        \\let x = 2
        \\let inc = fn(mut n)
        \\    n += 1
        \\inc(x)
        \\return x
    ,
        \\2
    );
    try expectOutput(
        \\let x = [2]
        \\let inc = fn(l) l.append(1)
        \\inc(x)
        \\return x
    ,
        \\[2, 1]
    );
}

test "in" {
    try expectOutput(
        \\let y = [1, 2, 3]
        \\if not true in y
        \\    y[-2] = false
        \\return y == [1, false, 3]
    ,
        \\true
    );
}

test "get/set" {
    try expectOutput(
        \\let y = [1, 2, 3]
        \\y[-2] = true
        \\return y[1]
    ,
        \\true
    );
}

test "mixed num and int" {
    try expectOutput(
        \\let mut y = 2
        \\y /= 5
        \\return y
    ,
        \\0.4
    );
    try expectOutput(
        \\let mut y = 2
        \\y **= 0.5
        \\return y
    ,
        \\1.4142135623730951
    );
}

test "copy on assign" {
    try expectOutput(
        \\let x = 2
        \\let mut y = x
        \\y += 2
        \\return x
    ,
        \\2
    );
    try expectOutput(
        \\let mut y = 2
        \\let inc = fn (mut a)
        \\    a += 2
        \\inc(y)
        \\return y
    ,
        \\2
    );
}

test "strings" {
    try expectOutput(
        \\let a = "hello"
        \\return if a == "world" 2 as str else 1.5 as str
    ,
        \\"1.5"
    );
}

test "comparison" {
    try expectOutput(
        \\let mut a = 0
        \\while a != 1000
        \\    a += 1
        \\return a
    ,
        \\1000
    );
}

test "while loop" {
    try expectOutput(
        \\let cond = true
        \\while cond
        \\    if not cond
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
        \\let y = (1, 2)
        \\return y[-1]
    ,
        \\2
    );
}

test "assert" {
    try expectOutput(
        \\let assert = fn (ok) if not ok error(false)
        \\return assert(not false)
    ,
        \\null
    );
}

test "functions" {
    try expectOutput(
        \\let add = fn ((a,b)) a + b
        \\let tuplify = fn (a,b) (a,b)
        \\return add(tuplify(1, 2))
    ,
        \\3
    );
    try expectOutput(
        \\let add = fn (a,b) a + b
        \\let sub = fn (a,b) a - b
        \\return sub(add(3, 4), add(1,2))
    ,
        \\4
    );
}

test "type casting" {
    try expectOutput(
        \\return 1 as null
    ,
        \\null
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
    try expectOutput(
        \\let x = 2.5 as int
        \\let y = x as num
        \\return y
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
    try expectOutput(
        \\let (a, b, _, c, _) = (1, 2, 3, 4, 5)
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
        \\let x = not false
        \\return 3 + if not x 2 else if x 4 else 9
    ,
        \\7
    );
}

test "assignment" {
    try expectOutput(
        \\let mut x = 2
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

fn expectCallOutput(source: []const u8, args: anytype, expected: []const u8) !void {
    const alloc = std.testing.allocator;
    var vm = Vm.init(alloc, .{});
    var mod = bog.compile(alloc, source, "<test buf>", &vm.errors) catch |e| switch (e) {
        else => return e,
        error.TokenizeError, error.ParseError, error.CompileError => {
            vm.errors.render(std.io.getStdErr().writer()) catch {};
            return error.TestFailed;
        },
    };
    mod.debug_info.source = "";
    defer mod.deinit(alloc);
    defer vm.deinit();
    vm.addStd() catch unreachable;

    var frame = bog.Vm.Frame{
        .mod = &mod,
        .body = mod.main,
        .caller_frame = null,
        .module_frame = undefined,
        .captures = &.{},
        .params = 0,
    };
    defer frame.deinit(&vm);
    frame.module_frame = &frame;

    vm.gc.stack_protect_start = @frameAddress();

    var frame_val = try vm.gc.alloc(.frame);
    frame_val.* = .{ .frame = &frame };
    defer frame_val.* = .{ .int = 0 }; // clear frame

    const res = vm.run(&frame) catch |e| switch (e) {
        else => return e,
        error.FatalError => {
            vm.errors.render(std.io.getStdErr().writer()) catch {};
            return error.TestFailed;
        },
    };

    if (true) return error.SkipZigTest;

    const call_res = vm.call(res, "doTheTest", args) catch |e| switch (e) {
        else => return e,
        error.FatalError => {
            vm.errors.render(std.io.getStdErr().writer()) catch {};
            return error.TestFailed;
        },
    };
    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    try call_res.dump(out_buf.writer(), 2);
    try testing.expectEqualStrings(expected, out_buf.items);
}

fn expectOutput(source: []const u8, expected: []const u8) !void {
    const alloc = std.testing.allocator;
    var vm = Vm.init(alloc, .{});
    var mod = bog.compile(alloc, source, "<test buf>", &vm.errors) catch |e| switch (e) {
        else => return e,
        error.TokenizeError, error.ParseError, error.CompileError => {
            vm.errors.render(std.io.getStdErr().writer()) catch {};
            return error.TestFailed;
        },
    };
    mod.debug_info.source = "";
    defer mod.deinit(alloc);
    defer vm.deinit();
    vm.addStd() catch unreachable;

    var frame = bog.Vm.Frame{
        .mod = &mod,
        .body = mod.main,
        .caller_frame = null,
        .module_frame = undefined,
        .captures = &.{},
        .params = 0,
    };
    defer frame.deinit(&vm);
    frame.module_frame = &frame;

    vm.gc.stack_protect_start = @frameAddress();

    var frame_val = try vm.gc.alloc(.frame);
    frame_val.* = .{ .frame = &frame };
    defer frame_val.* = .{ .int = 0 }; // clear frame

    const res = vm.run(&frame) catch |e| switch (e) {
        else => return e,
        error.FatalError => {
            vm.errors.render(std.io.getStdErr().writer()) catch {};
            return error.TestFailed;
        },
    };

    var out_buf = std.ArrayList(u8).init(alloc);
    defer out_buf.deinit();
    try res.dump(out_buf.writer(), 2);
    try testing.expectEqualStrings(expected, out_buf.items);
}
