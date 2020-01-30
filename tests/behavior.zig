test "constant values" {
    try expectOutput(
        \\true
    ,
        \\true
    );
    try expectOutput(
        \\not true
    ,
        \\false
    );
    try expectOutput(
        \\-12
    ,
        \\-12
    );
}

const std = @import("std");
const mem = std.mem;
const warn = std.debug.warn;
const testing = std.testing;
const lang = @import("lang");
const Tree = lang.Tree;
const Tokenizer = lang.Tokenizer;
const parser = lang.parser;
const compiler = lang.compiler;
const Builder = lang.Builder;
const Vm = lang.Vm;

var buffer: [10 * 1024]u8 = undefined;

fn expectOutput(source: []const u8, expected: []const u8) !void {
    var buf_alloc = std.heap.FixedBufferAllocator.init(buffer[0..]);
    const alloc = &buf_alloc.allocator;

    // TODO compiling anything is really difficult
    var builder: Builder = undefined;
    try builder.init(alloc);
    var tree = Tree.init(alloc);
    var vm = Vm.init(alloc, true);
    var tokenizer = Tokenizer.init(&tree, true);

    // TODO move this
    try vm.call_stack.push(.{
        .return_ip = null,
        .result_reg = undefined,
        .stack = try vm.gc.stackAlloc(250),
    });
    testing.expect(try tokenizer.tokenize(source));
    try parser.parse(&tree, 0);
    try compiler.compile(&builder, &tree, 0);


    try vm.exec(builder.cur_func.code.toSliceConst());
    if (vm.result) |some| {
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
