test "numbers" {
    testTransform(
        \\[0,0]
        \\[0.0]
        \\[0,0,0]
    ,
        \\[0,0]
        \\[0.0]
        \\[0,0, 0]
        \\
    );
}

test "ranges" {
    testCanonical(
        \\1:2:3
        \\:2:3
        \\::3
        \\1:2:
        \\1::
        \\1::3
        \\1:2
        \\1:
        \\:2
        \\:
        \\
    );
}

test "ignore comments in indent blocks" {
    testTransform(
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
    testCanonical(
        \\@foo
        \\@bar(foo)
        \\@baz(2.4, "foo")
        \\@qux[1, 2]
        \\@quux{foo: bar}
        \\
    );
}

test "different error initializations" {
    testCanonical(
        \\error
        \\error(foo)
        \\error(2.4, "foo")
        \\error[1, 2]
        \\error{foo: bar}
        \\
    );
}

test "nested blocks and matches" {
    testCanonical(
        \\if (false)
        \\    if (true)
        \\        match (2)
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
    testCanonical(
        \\a
        \\#foo
        \\#bar
        \\
    );
}

test "two empty lines after block" {
    testTransform(
        \\const foo = fn(a)
        \\    a * 4
        \\const bar = 2
    ,
        \\const foo = fn(a)
        \\    a * 4
        \\
        \\
        \\const bar = 2
        \\
    );
}

test "respect new lines" {
    testCanonical(
        \\const foo = 1
        \\
        \\const bar = 2
        \\
    );
    testTransform(
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
    testCanonical(
        \\if (false)
        \\    if (false)
        \\        3
        \\    else if (true)
        \\        4
        \\    else
        \\        5
        \\
        \\
        \\
    );
}

test "preserve comment after comma" {
    testTransform(
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
    testTransform(
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
    testCanonical(
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
    testCanonical(
        \\match (2)
        \\    let (x, 2) => x + 4
        \\    2, 3 => 1
        \\    _ => ()
        \\
        \\
        \\
    );
}

test "if" {
    testCanonical(
        \\if (foo) bar else baz
        \\if (const foo = bar()) baz
        \\
    );
}

test "catch" {
    testCanonical(
        \\foo() catch bar()
        \\baz() catch (const e) return e
        \\
    );
}

test "tuples, lists, maps" {
    testCanonical(
        \\(a, b)
        \\[a, b]
        \\{a: b, c: d}
        \\
    );
    testTransform(
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
    testCanonical(
        \\const foo = fn(arg1, arg2, _, arg3) (arg1, arg2, arg3)
        \\const bar = fn(val)
        \\    val * 45
        \\
        \\
        \\
    );
}

test "unicode identifiers" {
    testTransform(
        \\öäöäö;öö
    ,
        \\öäöäö
        \\öö
        \\
    );
}

test "trailing comma in call" {
    testCanonical(
        \\foo(2, 3)
        \\bar(
        \\    2,
        \\    3,
        \\)
        \\
    );
    testTransform(
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
    testCanonical(
        \\while (true) break
        \\return 123 // 4
        \\for (let foo in arr) foo + 2
        \\for (1:3) continue
        \\
    );
}

test "declarations" {
    testCanonical(
        \\let bar = import("args")
        \\const foo = bar + 2
        \\let err = error(foo)
        \\
    );
}

test "suffix ops" {
    testCanonical(
        \\foo[2].bar(2).baz[5 + 5]
        \\
    );
}

test "prefix ops" {
    testCanonical(
        \\not true
        \\-2
        \\
    );
}

test "infix ops" {
    testCanonical(
        \\123 + 2 * 3 / (4 as num) + ()
        \\
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const bog = @import("bog");

fn testTransform(source: []const u8, expected: []const u8) void {
    var errors = bog.Errors.init(std.testing.allocator);
    defer errors.deinit();
    var tree = bog.parse(std.testing.allocator, source, &errors) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError => {
            errors.render(source, std.io.getStdErr().outStream()) catch {};
            @panic("test failure");
        },
    };
    defer tree.deinit();

    var out_buf = std.ArrayList(u8).init(std.testing.allocator);
    defer out_buf.deinit();
    _ = tree.render(out_buf.outStream()) catch @panic("test failure");
    std.testing.expectEqualStrings(expected, out_buf.items);
}

fn testCanonical(source: []const u8) void {
    return testTransform(source, source);
}
