<!-- Copied/merged guidance for AI coding agents working on the `cubit` compiler -->
# Copilot Instructions — cubit (short)

This file gives focused, actionable guidance for AI coding agents making changes in this repository.

1. Big-picture architecture
- **What:** `cubit` is an educational compiler written in Go. The pipeline is: loader -> lexer -> parser -> analyzer (typechecker) -> IR lowering -> SSA/QBE -> assembly -> native binary.
- **Key orchestrator:** `cmd/cubit/main.go` drives the high-level flow and CLI flags (`-ast`, `-ssa`, `-run`, `-ldflags`). Use it to replicate local builds and runs.
- **IR & Codegen:** The project lowers to an internal `ir` package (`internal/ir`) and uses an SSA visitor (`internal/ir` + `internal/codegen/ssa_visitor.go`) to produce QBE SSA text. The `internal/codegen` package calls `modernc.org/libqbe` to emit target asm and runs the system `cc` to link.

2. Developer workflows (explicit commands)
- **Run the compiler on the example:** `go run ./cmd/cubit -run` (defaults to `examples/example.in`).
- **Generate SSA file:** `go run ./cmd/cubit -ssa examples/example.in` → `examples/out/example.ssa`.
- **Write AST before/after typecheck:** `-ast` writes `.astu` (pre-check) and `.astt` (post-check) files under `out/` next to the source.
- **Unit tests & examples:** `./test.sh` runs whatever project tests/examples exist. Run `go test ./...` for package tests.
- **Cross-compilation note:** `internal/codegen.GenerateAssembly` uses `runtime.GOOS`/`GOARCH` and libqbe target logic; `Compile` shells out to `cc` and accepts extra linker flags via `-ldflags` as a comma-separated list (e.g. `-ldflags ./lib/libraylib.a,-lm`).

3. Project-specific conventions and patterns
- **Stdlib import merging:** The loader treats an import named `core` specially: importing `"core"` will load `stdlib/core/core.in` and merge its `Types`, `Data`, and `Funcs` into the importing unit's global namespace. See `internal/loader/loader.go`.
- **Imports resolution:** Non-core imports map to `stdlib/<name>/<name>.in` and the loader attaches `Unit` into the `cu.Imports` map.
- **Type system:** Type inference/specialization occurs in the analyzer: variables declared with unknown/any types may be specialized on assignment (see `internal/analyzer/typecheck.go` VisitAssign and VisitFuncParam). Respect the `TypeUnknown`/`TypeAny` semantics when editing type logic.
- **IR builder:** `internal/ir` defines an explicit visitor-based SSA/IR model (types: `Val`, `Const`, `FuncDef`, `Block`, `Instruction` etc.). When adding codegen features, implement the corresponding `Visitor` methods in the SSA visitor.
- **Error reporting:** The analyzer accumulates `errors` in the `TypeChecker` and returns only the first error from `Check`. Be conservative when changing error flow — tests depend on location-driven diagnostics.

4. Integration points & external deps
- **libqbe:** QBE backend used via `modernc.org/libqbe` inside `internal/codegen/generator.go` (export assembly generation). Changes to SSA output affect libqbe input format.
- **System `cc`:** Final linking is done by invoking `cc` (GCC/clang) directly; ensure ABI/assembler output matches host `cc` expectations.

5. Files to consult for common tasks
- **Entry & flags:** `cmd/cubit/main.go` (CLI, outputs in `out/` beside the source file)
- **Loader & stdlib handling:** `internal/loader/loader.go`
- **Type checker:** `internal/analyzer/typecheck.go` and `internal/analyzer/scope.go`
- **AST definitions:** `internal/ast/ast.go`
- **IR model & visitor:** `internal/ir/ir.go` and `internal/ir/visitor` (search for `SSAVisitor`)
- **Codegen:** `internal/codegen/generator.go` and `internal/codegen/ssa_visitor.go`
- **Lexer / Parser:** `internal/lexer/*` and `internal/parser/*`

6. Editing rules for AI agents (do this, not that)
- **Do:** Prefer small, focused changes; run `go run ./cmd/cubit` locally to validate end-to-end output for examples.
- **Do:** When changing IR shapes or SSA names, update `internal/codegen/ssa_visitor.go` and run a round-trip `GenerateAssembly` to ensure libqbe accepts the output.
- **Do:** Respect `core` import special-case — tests rely on merging rather than normal import-scoping.
- **Don't:** Rework the CLI behavior or output file layout unless the change is isolated and tests or examples are updated accordingly.

7. Examples to copy/paste
- Run compile+run on example: `go run ./cmd/cubit -run examples/example.in`
- Generate SSA and inspect: `go run ./cmd/cubit -ssa examples/example.in && less examples/out/example.ssa`

8. When you need more info
- Look at `examples/` for real input cases; inspect `stdlib/` to see platform-provided symbols and how they're linked.
- If unsure about how a change affects produced assembly, run `go run ./cmd/cubit -ssa` and `internal/codegen.GenerateAssembly` will call libqbe; inspect resulting `.s` and then run `cc` manually via `internal/codegen.Compile`.

If anything in this doc is unclear or you'd like more examples (e.g., common refactor patterns or testing commands), tell me which area to expand.
