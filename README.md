Small, strongly typed, embeddable language. 
## Examples

### Hello world (only the latter works currently)
```rust
const {print} = import("io")
print("hello world")
# or native("std.print")("hello world\n")
```

### Calculator
```rust
# TODO these should be hidden behind an import
const input = native("std.input")
const print = native ("std.print")

const val1 = input("first argument: ") as num
const op = input("operation: ")
const val2 = input("second argument: ") as num

if (op == "*")
    print(val1 * val2)
else if (op == "+")
    print(val1 + val2)
else if (op == "-")
    print(val1 - val2)
else if (op == "/")
    print(val1 / val2)
else if (op == "**")
    print(val1 ** val2)
else 
    print("unknown op")
```

### Loops
```rust
let sum = 0
for (let c in "hellö wörld")
    if (c == "h") sum += 1
    else if (c == "e") sum += 2
    else if (c == "l") sum += 3
    else if (c == "ö") sum += 4
    else if (c == "w") sum += 5
    else if (c == "d") sum += 6

return sum # 31
```
```rust
const getSome = fn(val)  if (val != 0) val - 1

let val = 10
while (let newVal = getSome(val))
    val = newVal
return val # 0
```

### Destructuring assignment
```rust
const add = fn ((a,b)) a + b
const tuplify = fn (a,b) (a,b)
return add(tuplify(1,2)) # 3
```

## Embed
```zig
const bog = @import("bog");

fn run(allocator: *Allocator, source: []const u8, vm: *Vm) !*bog.Value {
    var module = try bog.compile(allocator, source, &vm.errors);
    defer module.deinit();

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.entry;
    return try vm.exec(module);
}

...

var vm = bog.Vm.init(allocator, .{ .import_files = true });
defer vm.deinit();
try bog.std.registerAll(&vm.native_registry);

const res = run(allocator, source, &vm) catch |e| switch (e) {
    else => |err| return err,
    error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
        try vm.errors.render(source, out_stream);
        return error.RunningBogFailed;
    },
};

const bog_bool = try res.bogToZig(bool, &vm);
```

### Calling Bog functions from Zig

```zig
var vm = Vm.init(allocator, .{});
const res = run(allocator, source, &vm) catch |e| switch (e) {
    else => |err| return err,
    error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
        try vm.errors.render(source, out_stream);
        return error.RunningBogFailed;
    },
};

const call_res = vm.call(res, "bogFunction", .{1, true}) catch |e| switch (e) {
    else => |err| return err,
    error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
        try vm.errors.render(source, out_stream);
        return error.CallingBogFunctionFailed;
    },
};
const bog_integer = try call_res.bogToZig(i64, &vm);
```

## Setup
* Download master version of Zig from https://ziglang.org/download/
* Clone this repo
* Build with `zig build`
* Run with `./zig-cache/bin/bog`
