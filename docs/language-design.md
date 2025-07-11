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

// Enums (with data)
Result :: enum(int) {
    Ok(value: int),
    Error(msg: string, code: int)
}

// Usage
res := Result.Ok(42)                     // positional
err := Result.Error(msg="fail", code=1)  // named

// Generic option type
Option :: enum($T) {
    Some(T),    // name is optional
    None
}

opt := Option.Some(123)             // type parameter is inferred
none : Option(int) = Option.None()  // can't infer the type parameter here
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
nums   : [4]int       // Fixed-size array (uninitialized, contents undefined)
names  : []string     // Slice (dynamic array)
matrix : [3][3]float  // Multi-dimensional array

// Array initialization semantics:
// Declaration without initialization (no zeroing, contents are undefined)
a : [128]int

// Declaration with zero-initialization
a := [128]int{}

// Assignment with zero-initialization
a = [128]int{}

// Declaration with explicit initialization
b := [4]int{1, 2, 3, 4}
```

- `a : [128]int` declares an array variable, but does not initialize its contents.
- `a := [128]int{}` or `a = [128]int{}` zero-initializes the array.
- Explicit value initialization (`{1,2,3}`) requires the array size to match the number of elements provided.

---

## 7. Function Overloading

- Overloading is allowed if signatures differ by parameter types or count.
- Ambiguous calls result in a compiler error.

```odin
foo :: func(x: int)
foo :: func(x: string)
foo(42)      // calls int version
foo("bar")   // calls string version
```

### Overloading Rules

- **User-defined functions:**
  - Overloads must be defined in the same package as the original function symbol.
  - Each package has its own namespace; functions with the same name in different packages are unrelated.
  - Overloads for a function cannot be defined in a different package than where the function symbol is declared.

- **Built-in functions (e.g., `len`):**
  - Built-ins exist in a special global namespace (not in any package).
  - You may provide overloads for built-in functions (such as `len`) only in the package where the custom type is defined.
  - This allows custom types to integrate with built-in operations in a controlled, unambiguous way.

- **General restriction:**
  - You may only define an overload for a function `f(x: T, ...)` in a package if that package defines either the function symbol `f` or the type `T`.
  - This prevents accidental or conflicting overloads across package boundaries and keeps resolution predictable.

#### Example: Overloading `len`

```odin
MyType :: struct { data: []int }

// In the same package as MyType:
len :: func(x: MyType) -> int {
    return len(x.data)
}

x := MyType(data: [1,2,3])
n := len(x) // calls user-defined len(MyType)
```

This approach ensures extensibility, avoids ambiguity, and keeps overloads local to the type or function's defining package.

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

## 11. If Statements

Conditional statements don't require parenthesis, but do require braces. They support an optional short variable declarations in the condition.

```odin
if x != 0 {
    // non-zero
}

if x := foo(); x == 0 {
    // zero
} else if x > 0 {
    // positive
} else {
    // negative
}
```

Notes:
- The `if` statement may include an optional initializer before the condition, separated by a semicolon.
- `else if` chains are supported for multiple branches.

---


## 12. Loops and Iterators

The language supports several forms of the `for` loop:

```odin
for {
    // infinite loop
}

for cond {
    // loop with condition
}

for x in my_iterable {
    // loop over iterable
}
```

Use `break` to exit a loop early, and `continue` to skip to the next iteration.

The `loop` context object is available inside all loop bodies, providing per-iteration state:
  - `loop.index` (current index, 0-based)
  - `loop.at_first` (true on first iteration)
  - Note: `loop.at_last` and `loop.at_new` are only available in `for x in ...` loops, not in infinite or conditional loops.

This approach keeps the loop header clean and avoids extra variables.

Any type can be made iterable by providing an `iter()` method that returns an iterator object with a `next()` method.

#### Example: Loop Context

```odin
for val in my_iterator {
    if loop.at_first {
        print("first iteration")
    }

    if loop.at_new(val.item) {
        print("item changed")
    }

    print("at: %d", loop.index)

    if loop.at_last {
        print("last iteration")
    }
}
```

- The `loop` context object is available inside the loop body, providing per-iteration state:
    - `loop.index` (current index, 0-based)
    - `loop.at_first` (true on first iteration)
    - `loop.at_last` (true on last iteration)
    - `loop.at_new(field)` (true if the given field changed compared to the previous iteration)
- This approach keeps the loop header clean and avoids extra variables.
- Any type can be made iterable by providing an `iter()` method that returns an iterator object.

### Iterator Protocol with Enum State

To support features like `at_last` without lookahead, the iterator protocol uses a generic enum to indicate the state and carry the value:

```odin
IteratorResult :: enum($T) {
    Item(T),   // regular item, more may follow
    Last(T),   // this is the last item
    Done,      // no more items
}

next :: func(self: ^Iterator) -> IteratorResult(T)
```

The loop machinery uses this enum to set `loop.at_last` and related fields, so users do not need to match on the enum directly in typical code.

### Example: Range Iterator

```odin
Range :: struct {
    start: int,
    end: int,
    curr: int,
}

Range.iter :: func(self: Range) -> RangeIterator {
    return RangeIterator(self.start, self.end, self.start)
}

RangeIterator :: struct {
    start: int,
    end: int,
    curr: int,
}

RangeIterator.next :: func(self: ^RangeIterator) -> IteratorResult(int) {
    if self.curr < self.end - 1 {
        val := self.curr
        self.curr += 1

        return IteratorResult.Item(val)
    } else if self.curr == self.end - 1 {
        val := self.curr
        self.curr += 1

        return IteratorResult.Last(val)
    }

    return IteratorResult.Done
}

range :: func(start: int, end: int) -> Range {
    return Range(start, end, start)
}
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

## 14. Pointers

Pointers provide low-level access to memory and are used for referencing, dereferencing, and pointer arithmetic.

### Pointer Types

- The type `^T` denotes a pointer to type `T`.
  - Example: `p: ^int` declares `p` as a pointer to an integer.

### Address-of Operator

- Use `&` to take the address of a variable or value.
  - Example: `p := &x` assigns `p` the address of `x`.

### Dereferencing

- Use postfix `^` to dereference a pointer.
  - Example: `val := p^` assigns the value pointed to by `p` to `val`.

### Pointer Arithmetic

- Pointers support arithmetic for types with a defined size (e.g., arrays, structs, primitives).
- You can add or subtract integers to/from pointers to move by element size:
  - `p + n` yields a pointer offset by `n` elements of the pointed-to type.
  - `p - n` yields a pointer offset by `-n` elements.
  - Example: `q := p + 1` points to the next element after `p`.
- Subtracting two pointers of the same type yields the number of elements between them:
  - Example: `diff := q - p` (if `q` and `p` are both `^T`).

### Nil Pointers

- The literal `nil` represents a pointer that does not point to any value.
  - Example: `p: ^int = nil`

### Usage Example

```odin
x := 42
p := &x        // p is ^int
y := p^        // y is 42

arr := [10]int{...}
p := &arr[0]
p2 := p + 3    // points to arr[3]
val := p2^     // value at arr[3]
```

### Safety

- Pointer arithmetic is only valid within the bounds of the same array or allocation.
- Dereferencing a nil or invalid pointer is undefined behavior.


---


## 12. Implicit Context

Every function, lambda, and method has access to an implicit context value via a special identifier (e.g., `context`). This context is not passed explicitly, but is always available for logging, resource management, cancellation, etc.

- The context is function-local and changes are only visible down the call stack (not up).
- When a function or block modifies the context, the change is scoped to that call and its callees; the previous context is restored on return.
- This enables safe dependency injection and avoids surprising side effects.

### Example

```odin
foo :: func(x: int) {
    context.log("foo called")
    context.allocator = myAllocator
    bar() // bar and anything it calls see the new allocator
    // after bar returns, context.allocator is restored
}

// Lambdas also have access to context
f := func(x: int) = context.trace("lambda called")
```

---

## 13. Lambdas (Anonymous Functions)

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

Destructuring allows you to unpack values from tuples, function returns, and enums directly into variables. Only positional destructuring with (a, b) is supported.

### Unnamed (Positional) Tuples

```odin
t := (1, 2)
(a, b) := t         // a = 1, b = 2
```

### Named Tuples

```odin
point := (x: 5, y: 7)
(a, b) := point     // a = 5, b = 7 (by position)

// Access by name
xval := point.x     // xval = 5
yval := point.y     // yval = 7
```

### Function Return Values

```odin
get_pair :: func() = (10, 20)
(x, y) := get_pair() // x = 10, y = 20

get_point :: func() = (x: 3, y: 4)
(x, y) := get_point() // x = 3, y = 4
```

### Pattern Matching on Tuples

```odin
switch t {
    case (a, b):
        // a = 1, b = 2
}

switch point {
    case (x, y):
        // x = 5, y = 7
}

// rebinding the fields to new names
switch point {
    case (x as a, y as b):
        // a = 5, b = 7
}
```

### Enum (Tagged Union) Destructuring (Pattern Matching)

```odin
switch res {
    case Result.Ok(value):
        // use value
    case Result.Error(msg, code):
        // use msg, code
    case Result.Pair(x as a, y as b):
        // use a, b
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

## 15. Strings: Immutable, Sized, and Zero-Terminated

Strings in this language are immutable and designed for maximal interoperability, especially with C code. Each string value stores both its length and a trailing zero byte (\0), enabling efficient and safe operations:

- **Immutability:** Strings cannot be modified after creation. This guarantees safety, thread-friendliness, and allows for internal sharing and optimizations.
- **Sized:** The length of a string is always known in O(1) time, allowing fast slicing, concatenation, and iteration without scanning for a terminator.
- **Zero-Terminated:** All strings are guaranteed to have a trailing \0 byte, making them directly usable with C APIs expecting null-terminated strings (e.g., `printf`, `strcpy`).
- **C Interoperability:**
    - When calling C functions, you can pass the string pointer directly for APIs expecting a null-terminated string, or both pointer and length for APIs expecting both.
    - When receiving strings from C, the language can construct a string value by scanning for the first \0 and storing the length.
- **Embedded Nulls:** Embedded null bytes (\0) are allowed in strings, but passing such strings to C APIs expecting null-terminated strings will result in truncation at the first null. The length field is always authoritative within the language.
- **Minimal Overhead:** The only overhead is a single trailing byte per string, which is negligible on modern systems.

This design provides the best of both worlds: fast, safe string operations and seamless C interop, while keeping the language simple and efficient.

---
