# compiler-33 🚀

A simple compiler project written in Go, demonstrating the process of tokenizing, parsing, and generating code for a minimal language. This project is educational and showcases the basics of building a compiler pipeline. 🛠️

Do not use this, it is only tested with the `example.in` file and doesn't even generate completely correct code there!

## Features ✨

- **Tokenization**: Converts source code into tokens. 🏷️
- **Parsing**: Builds an abstract representation (operations) from tokens. 🌳
- **Code Generation**: Outputs code in SSA and QBE formats. ⚙️
- **Command-line Options**: Control output and execution via flags. 🖥️
- **Example Input**: See `example.in` for a sample program. 📄

## Project Structure 📁

- `main.go` - Entry point, handles CLI flags and orchestrates compilation.
- `tokenizer.go` - Tokenizes the input source code.
- `scanner.go` - Reads and manages input data for tokenization.
- `parser.go` - Parses tokens into AST.
- `generator.go` - Generates code (SSA/QBE) from parsed operations.
- `ast.go` - AST structures.
- `example.in` - Example source file for the compiler.
- `go.mod` / `go.sum` - Go module files and dependencies.

## Example 📝

The provided `example.in`:

```odin
package main

printf :: extern(msg: string, arg: int)

hello :: func(arg: int) {
  printf("Hello from compiler-%d!\n", arg)
}

@(export)
main :: func() -> int {
  hello(33)

  return 0
}
```

## Usage 🏃

```sh
go run main.go [options]
```

### Options

- `-tok`  : Write tokens to file (`out/example.tok`)
- `-ssa`  : Write SSA code to file (`out/example.ssa`)
- `-run`  : Run the compiled code
- `-help` : Show help message

>[!note]
> `out/example.s` and `out/example` are always generated.

## Dependencies 📦

- Go 1.24
- C-compiler (`cc`) to compile the generated assembly

## Getting Started 🚦

1. Clone the repository.
2. Run `go mod tidy` to install dependencies.
3. Use `go run . -run` to compile and run the example file.

---

Happy compiling! 🎉
