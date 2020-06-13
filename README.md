Small, strongly typed, embeddable language. 
## [Examples](examples)

### Hello world
```rust
const {print} = import("std.io")
print("hello world")
```

### Calculator
```rust
const {input, print} = import("std.io")

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

### Use command line arguments
```rust
# run with `path/to/bog path/here.bog arg1 arg2 "foo"`
const {print} = import("io")
print(import("args"))
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

var vm = bog.Vm.init(allocator, .{ .import_files = true });
defer vm.deinit();
try vm.addPackage("std.io", bog.std.io);

const res = vm.run(source) catch |e| switch (e) {
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
defer vm.deinit();

const res = vm.run(source) catch |e| switch (e) {
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

### Calling Zig functions from Bog

```zig
fn pow(val: i64) i64 {
    return val * val;
}

var vm = Vm.init(allocator, .{});
defer vm.deinit();
try vm.addPackage("pow", pow)

const res = vm.run(source) catch |e| switch (e) {
    else => |err| return err,
    error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
        try vm.errors.render(source, out_stream);
        return error.RunningBogFailed;
    },
};

const bog_integer = try res.bogToZig(i64, &vm);
std.debug.assert(bog_integer == 8);
```

```rust
const pow = import("pow")

return 2 * pow(2)
```

## Setup
* Download master version of Zig from https://ziglang.org/download/
* Clone this repo
* Build with `zig build`
* Run with `./zig-cache/bin/bog`
