Small, strongly typed, embeddable language. 
## [Examples](examples)

### Hello world
```julia
const {print} = import("std.io")
const world = "world"
print(f"hello {world}!")
```

### Calculator
```julia
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
```julia
# run with `path/to/bog path/here.bog arg1 arg2 "foo"`
const {print} = import("io")
print(import("args"))
```

### Loops
```julia
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
```julia
const getSome = fn(val)  if (val != 0) val - 1

let val = 10
while (let newVal = getSome(val))
    val = newVal
return val # 0
```

### Error handling
```julia
const {input, print} = import("std.io")

const fails_on_1 = fn(arg) if (arg == 1) error(69)
const fails_on_2 = fn(arg) if (arg == 2) error(42)
const fails_on_3 = fn(arg) if (arg == 3) error(17)

const foo = fn(arg)
    try
        fails_on_1(arg)
        fails_on_2(arg)
        fails_on_3(arg)
    catch (let err)
        return err

    return 99

print(for (let i in 0:4) foo(i)) # [99, 69, 42, 17]
print(try fails_on_1(input("give number: ") as int) catch "gave 1")
```

### Destructuring assignment
```julia
const add = fn ((a,b)) a + b
const tuplify = fn (a,b) (a,b)
return add(tuplify(1,2)) # 3
```

## Embed
```zig
const bog = @import("bog");

var vm = bog.Vm.init(allocator, .{ .import_files = true });
defer vm.deinit();
try vm.addStd();

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
const my_lib = struct {
    pub fn pow(val: i64) i64 {
        return val * val;
    }
};

var vm = Vm.init(allocator, .{});
defer vm.deinit();
try vm.addPackage("my_lib", my_lib);

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

```julia
const {pow} = import("my_lib")

return 2 * pow(2)
```

## Setup
* Download master version of Zig from https://ziglang.org/download/
* Clone this repo
* Build with `zig build`
* Run with `./zig-cache/bin/bog`
