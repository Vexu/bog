test "variadic functions" {
    try testCanonical(
        \\let foo = fn() null
        \\let bar = fn(a...) a
        \\let baz = fn(a, b...) a
        \\baz(1, ..."foo")
        \\
    );
}

test "format string" {
    try testTransform(
        \\f"foo {{1:2}:32} bar { 2
        \\*
        \\3:4} baz \t"
        \\
        \\
    ,
        \\f"foo {{1:2}:32} bar {2 *
        \\    3:4} baz \t"
        \\
    );
}

test "try-catch" {
    try testCanonical(
        \\try
        \\    assert(x == y)
        \\    assert(x != z)
        \\catch "assertion failure"
        \\    print("assertion failure")
        \\catch 1
        \\    print("got one")
        \\catch let err
        \\    print(err)
        \\
        \\
        \\
    );
    try testCanonical(
        \\try assert(x == y) catch print("assertion failure")
        \\
    );
    try testCanonical(
        \\try
        \\    assert(x == y)
        \\catch
        \\    print("them all")
        \\
        \\
        \\
    );
}

test "ranges" {
    try testCanonical(
        \\1:2:3
        \\1::3
        \\1:2
        \\1:
        \\
    );
}

test "ignore comments in indent blocks" {
    if (true) return error.SkipZigTest;
    try testTransform(
        \\const foo = fn()
        \\                #quux
        \\#foo bar
        \\                #quux
        \\#foo bar
        \\                #quux
        \\    return 2
    , // TODO improve comment rendering
        \\const foo = fn()
        \\#quux
        \\#foo bar
        \\#quux
        \\#foo bar
        \\#quux
        \\    return 2
        \\
        \\
        \\
    );
}

test "tag" {
    try testCanonical(
        \\@foo
        \\@bar(foo)
        \\@baz(2.4, "foo")
        \\@qux[1, 2]
        \\@quux{foo = bar}
        \\
    );
}

test "different error initializations" {
    try testCanonical(
        \\error
        \\error(foo)
        \\error(2.4, "foo")
        \\error[1, 2]
        \\error{foo = bar}
        \\
    );
}

test "nested blocks and matches" {
    try testCanonical(
        \\if false
        \\    if true
        \\        match 2
        \\            true => a
        \\            false => b
        \\
        \\
        \\    else
        \\        2
        \\
        \\
        \\
    );
}

test "comments after expression" {
    if (true) return error.SkipZigTest;
    try testCanonical(
        \\a
        \\#foo
        \\#bar
        \\
    );
}

test "two empty lines after block" {
    try testTransform(
        \\let foo = fn(a)
        \\    a * 4
        \\let bar = 2
    ,
        \\let foo = fn(a)
        \\    a * 4
        \\
        \\
        \\let bar = 2
        \\
    );
}

test "respect new lines" {
    if (true) return error.SkipZigTest;
    try testCanonical(
        \\const foo = 1
        \\
        \\const bar = 2
        \\
    );
    try testTransform(
        \\const foo = 1
        \\
        \\
        \\const bar = 2
    ,
        \\const foo = 1
        \\
        \\const bar = 2
        \\
    );
}

test "nested blocks" {
    try testCanonical(
        \\if false
        \\    if false
        \\        3
        \\    else if true
        \\        4
        \\    else
        \\        5
        \\
        \\
        \\
    );
}

test "preserve comment after comma" {
    if (true) return error.SkipZigTest;
    try testTransform(
        \\(1, #hello world
        \\    2)
        \\
    ,
        \\(
        \\    1, #hello world
        \\    2,
        \\)
        \\
    );
    try testTransform(
        \\(1#hello world
        \\    , 2)
        \\
    ,
        \\(
        \\    1, #hello world
        \\    2,
        \\)
        \\
    );
}

test "preserve comments" {
    if (true) return error.SkipZigTest;
    try testCanonical(
        \\#some comment
        \\123 +
        \\    #another comment
        \\    #third comment
        \\    2
        \\#fourth comment
        \\#fifth comment
        \\
    );
}

test "match" {
    try testCanonical(
        \\match 2
        \\    let (x, 2) => x + 4
        \\    2, 3 => 1
        \\    _ => null
        \\
        \\
        \\
    );
}

test "if" {
    try testCanonical(
        \\if foo bar else baz
        \\if let foo = bar() baz
        \\
    );
}

test "tuples, lists, maps" {
    try testCanonical(
        \\(a, b)
        \\[a, b]
        \\{a = b, c = d}
        \\
    );
    try testTransform(
        \\(a,b,c,)
    ,
        \\(
        \\    a,
        \\    b,
        \\    c,
        \\)
        \\
    );
}

test "functions" {
    try testCanonical(
        \\let foo = fn(arg1, arg2, _, arg3) (arg1, arg2, arg3)
        \\let bar = fn(val)
        \\    val * 45
        \\
        \\
        \\
    );
}

test "unicode identifiers" {
    try testTransform(
        \\öäöäö;öö
    ,
        \\öäöäö
        \\öö
        \\
    );
}

test "trailing comma in call" {
    try testCanonical(
        \\foo(2, 3)
        \\bar(
        \\    2,
        \\    3,
        \\)
        \\
    );
    try testTransform(
        \\foo(2, 3,)
        \\bar(
        \\    2,
        \\    3
        \\)
        \\
    ,
        \\foo(
        \\    2,
        \\    3,
        \\)
        \\bar(
        \\    2,
        \\    3,
        \\)
        \\
    );
}

test "loops" {
    try testCanonical(
        \\while true break
        \\return 123 // 4
        \\for let foo in arr foo + 2
        \\for 1:3 continue
        \\
    );
}

test "declarations" {
    try testCanonical(
        \\let bar = import "args"
        \\let foo = bar + 2
        \\let err = error(foo)
        \\
    );
}

test "suffix ops" {
    try testCanonical(
        \\foo[2].bar(2).baz[5 + 5]
        \\
    );
}

test "prefix ops" {
    try testCanonical(
        \\not true
        \\-2
        \\
    );
}

test "infix ops" {
    try testCanonical(
        \\123 + 2 * 3 / (4 as num) + null
        \\
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const bog = @import("bog");

fn testTransform(source: []const u8, expected: []const u8) !void {
    _ = bog.Vm; // avoid false dependency loop
    var errors = bog.Errors.init(std.testing.allocator);
    defer errors.deinit();
    var tree = bog.parse(std.testing.allocator, source, "<test buf>", &errors) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError => {
            errors.render(std.io.getStdErr().writer()) catch {};
            @panic("test failure");
        },
    };
    defer tree.deinit(std.testing.allocator);

    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    _ = tree.render(out_buf.writer()) catch @panic("test failure");
    try std.testing.expectEqualStrings(expected, out_buf.items);
}

fn testCanonical(source: []const u8) !void {
    try testTransform(source, source);
}
