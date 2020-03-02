Small, strongly typed, embeddable language. 
## Examples

### Hello world (Neither work yet)
```rust
const {print} = import("io")
print("hello world")
# or native("std.print")("hello world")
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

fn run(allocator: *Allocator, source: []const u8, vm: *Vm) !?*bog.Value {
    var module = try bog.compile(allocator, source, &vm.errors);
    defer module.deinit();

    // TODO this should happen in vm.exec but currently that would break repl
    vm.ip = module.entry;
    return try vm.exec(&module);
}

...

var vm = bog.Vm.init(allocator, .{});
defer vm.deinit();

const res = run(allocator, source, &vm) catch |e| switch (e) {
    else => |err| return err,
    error.TokenizeError, error.ParseError, error.CompileError, error.RuntimeError => {
        try vm.errors.render(source, out_stream);
        return e;
    },
};

if (res) |some| ...
```

## Setup
* Download latest Zig from https://ziglang.org/download/
* Clone this repo
* Build with `zig build`
* Run with `./zig-cache/bin/bog`
