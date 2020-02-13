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

let foo = fn(arg)
    if (arg == 4)
        return error(4)
    arg += if (arg == 2) 1 else 2

let y = foo(x)

assert(x == 2 and y == x)

# copy on assign
let baz = fn(x)
    let z = x
    z += 2
    z

assert(baz(x) == 4 and x == 2)

# error: error discarded
# assert(false)

# constant declarations in module scope can be referenced before they 
# are declared but using them is an error
const two = fn() one(1)
# error: use of undefined value
# two()
const one = fn(val) val

assert(two() == 1)

# return optional as last statement of block
return {
    assert,
}
```