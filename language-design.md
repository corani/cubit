# Language Design: Syntax Overview

This document is a work in progress, summarizing the language syntax and semantics.

---

## 1. Constants and Variables

```odin
// Constants use '::'
PI :: 3.14159

// Variables use ':=' (type can be inferred or explicit)
count := 42
name : string = "hello"
```

---

## 2. Functions

```odin
// Function with default parameter and named argument
hello :: func(arg: int = 42) {
    message :: "Hello from compiler-%d!\n"
    printf(message, arg)
}

// Single-line pure function with return type inference
@(pure)
sum :: func(a, b: int) = a + b

// Named parameters in calls
hello(arg=99)
```

---

## 3. Attributes

```odin
// Attributes are placed before definitions, can be comma-separated
@(extern)
printf :: func(msg: string, arg: int)

@(export)
main :: func() -> int { ... }

@(pure, inline)
fast_add :: func(a, b: int) = a + b
```

---


## 4. Structs, Enums, and Type Aliases


```odin
// Structs
Point :: struct { x: int, y: int }

// Struct with default values
Config :: struct { host: string = "localhost", port: int = 8080 }

// Type aliases
Temperature :: int
Meters :: float
StringList :: []string

// Enums (simple)
Color :: enum { Red, Green, Blue }

// Rust-style enums (tagged unions with data)
Result :: enum {
    Ok(value: $T),
    Error(msg: string, code: int)
}

// Usage
res := Result.Ok(value=42)
err := Result.Error(msg="fail", code=1)

// Option type
Option :: enum {
    Some(value: $T),
    None
}

opt := Option.Some(value=123)
none := Option.None()
```

---

## 5. Generics (Parametric Polymorphism)

```odin
// Generic struct
Stack :: struct($T) { data: []T, len: int }

// Generic function (type parameters inferred from arguments)
pair :: func(a: $A, b: $B) = (a, b)

// Usage with type inference
a_stack := Stack(data: []int{1,2,3}, len: 3)
p := pair(1, "foo") // $A=int, $B=string
```

---

## 6. Arrays and Slices

```odin
nums :: [4]int         // Fixed-size array
names :: []string      // Slice (dynamic array)

matrix :: [3][3]float  // Multi-dimensional array
```

---

## 7. Function Overloading

- Overloading is allowed if signatures differ by parameter types or count.
- Ambiguous calls result in a compiler error.

```odin
foo :: func(x: int)
foo :: func(x: string)
foo(42)      // calls int version
foo("bar")  // calls string version
```

---

## 8. Struct Instantiation (Constructor Style)

```odin
cfg := Config(port: 9090)
p := Point(x: 1, y: 2)
```

---


## 9. Minimal Keywords and Visibility

- Most features are implemented via attributes and type parameters, not keywords.
- This keeps the language extensible and user-friendly.
- All symbols are public by default. Use the `@(private)` attribute to restrict visibility within a module/package.

```odin
@(private)
helper_func :: func(x: int) = x * 2

@(private)
InternalType :: struct { ... }
```

---


## 10. Imports

Imports bring in complete packages. By default, the last element of the import path is used as the prefix. Use `as` to specify an alias in case of conflicts.

```odin
import math
import mylib/utils

x := math.sqrt(2)
y := utils.do_something()
```

```odin
import utils
import mylib/utils as mu

x := utils.do_something()
y := mu.do_something_else()
```

---

## 11. Example Program

```odin
package main

@(extern)
printf :: func(msg: string, arg: int)

hello :: func(arg: int = 42) {
    message :: "Hello from compiler-%d!\n"
    printf(message, arg)
}

@(pure)
sum :: func(a, b: int) = a + b

@(export)
main :: func() -> int {
    count := sum(11, 22)
    hello(arg=count)
    return 0
}
```

---


## 12. Lambdas (Anonymous Functions)

Lambdas use the same syntax as function definitions, but with `:=` instead of `::`. You can assign them to variables or pass them directly as arguments.

```odin
// Assigning a lambda to a variable
add := func(a: int, b: int) = a + b

// Passing a lambda directly
result := map([1,2,3], func(x: int) = x * 2)

// With block body
printer := func(msg: string) {
    printf(msg)
}
```

---

## 13. Destructuring Assignment

Destructuring allows you to unpack values from tuples, function returns, structs, and enums directly into variables.

```odin
a := (1, 2)        // a is a tuple
(a, b) := (1, 2)   // destructures tuple into a and b

(x, y) := pair(1, 2) // destructures function return values
```

### Enum (Tagged Union) Destructuring (Pattern Matching)

```odin
switch res {
    case Result.Ok(value):
        // use value
    case Result.Error(msg, code):
        // use msg, code
}

// Or single-variant destructuring
if Result.Ok(value) := res {
    // use value
}
```

### Option Type Destructuring

```odin
switch opt {
    case Option.Some(value):    // use value
    case Option.None:           // handle none
}

if Option.Some(x) := opt {
    // use x
}
```

---
