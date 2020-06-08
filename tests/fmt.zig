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

test "native function" {
    testCanonical(
        \\const foo = native("bar.foo")
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
    );
}

test "preserve comment after comma" {
    testCanonical(
        \\(1, #hello world
        \\    2)
        \\
    );
    // TODO make this prettier
    testCanonical(
        \\(1#hello world
        \\    , 2)
        \\
    );
}

test "range operator" {
    testCanonical(
        \\1...2
        \\
    );
}

test "preserve comments" {
    testCanonical(
        \\#some comment
        \\123 + #another comment
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
        \\    let (x, 2): x + 4
        \\    2, 3: 1
        \\    _: ()
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
        \\foo(2,3,)
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
        \\for (1...3) continue
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

var buffer: [10 * 1024]u8 = undefined;

fn testTransform(source: []const u8, expected: []const u8) void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    var errors = bog.Errors.init(alloc);
    var tree = bog.parse(alloc, source, &errors) catch |e| switch (e) {
        else => @panic("test failure"),
        error.TokenizeError, error.ParseError => {
            errors.render(source, std.io.getStdErr().outStream()) catch {};
            @panic("test failure");
        },
    };

    var out_buf = std.ArrayList(u8).init(alloc);
    tree.render(out_buf.outStream()) catch @panic("test failure");
    std.testing.expectEqualStrings(expected, out_buf.items);
}

fn testCanonical(source: []const u8) void {
    return testTransform(source, source);
}
