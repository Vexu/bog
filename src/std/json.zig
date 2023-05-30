const std = @import("std");
const bog = @import("../bog.zig");
const Value = bog.Value;
const Vm = bog.Vm;

pub fn parse(ctx: Vm.Context, str: []const u8) !*Value {
    var scanner = std.json.Scanner.initCompleteInput(ctx.vm.gc.gpa, str);
    return parseInternal(ctx.vm, &scanner);
}

const Error = error{ UnexpectedToken, UnexpectedEndOfJson } || std.mem.Allocator.Error || std.json.Scanner.AllocIntoArrayListError ||
    std.fmt.ParseIntError;
fn parseInternal(vm: *Vm, scanner: *std.json.Scanner) Error!*Value {
    switch (try scanner.next()) {
        .object_begin => {
            const res = try vm.gc.alloc(.map);
            res.* = .{ .map = .{} };

            while (true) {
                switch (try scanner.peekNextTokenType()) {
                    .object_end => {
                        _ = try scanner.next();
                        break;
                    },
                    else => {},
                }
                const key = try parseInternal(vm, scanner);
                const val = try parseInternal(vm, scanner);
                try res.map.put(vm.gc.gpa, key, val);
            }
            return res;
        },
        .array_begin => {
            const res = try vm.gc.alloc(.list);
            res.* = .{ .list = .{} };

            while (true) {
                switch (try scanner.peekNextTokenType()) {
                    .array_end => {
                        _ = try scanner.next();
                        break;
                    },
                    else => {},
                }
                try res.list.inner.append(vm.gc.gpa, try parseInternal(vm, scanner));
            }
            return res;
        },
        .object_end, .array_end => return error.UnexpectedToken,
        .string => {
            var b = Value.String.builder(vm.gc.gpa);
            errdefer b.cancel();
            _ = try scanner.allocNextIntoArrayList(&b.inner, .alloc_always);
            const ret = try vm.gc.alloc(.str);
            ret.* = .{ .str = b.finish() };
            return ret;
        },
        .number => |bytes| {
            const val = try vm.gc.alloc(.int);
            val.* = .{ .int = std.fmt.parseInt(i64, bytes, 10) catch {
                val.* = .{ .num = try std.fmt.parseFloat(f64, bytes) };
                return val;
            } };
            return val;
        },
        .true => return Value.True,
        .false => return Value.False,
        .null => return Value.Null,
        .end_of_document => return Value.Null,

        .partial_number,
        .allocated_number,
        .partial_string,
        .partial_string_escaped_1,
        .partial_string_escaped_2,
        .partial_string_escaped_3,
        .partial_string_escaped_4,
        .allocated_string,
        => unreachable,
    }
}

pub fn stringify(ctx: Vm.Context, val: *Value) !Value.String {
    var b = Value.String.builder(ctx.vm.gc.gpa);
    errdefer b.cancel();
    try std.json.stringify(val, .{}, b.writer());
    return b.finish();
}
