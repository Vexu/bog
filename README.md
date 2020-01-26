Small, strongly typed, embeddable language. 
---
Not much is done but this is how it is going to look like
```rust
let {print} = import('io')

let assert = fn(ok) if (not ok) error("assertion failure")

let x = 0

# error: cannot convert boolean to integer
# x + true
x + (true as int)
assert(x == 1)

let foo = fn(arg) {
    if (arg == 4) {
        return error(4)
    }
    arg += if (arg == 2) {
        1
    } else 2
}

let y = foo(x)

assert(x == 2 and y == x)

# copy on assign
let baz(x) {
    let z = x
    z += 2
}

assert(baz(x) == 4 and x == 2)

# error: error discarded
# assert(false)

# return optional for last value of function (all files are functions)
return {
    assert: assert,
}
```