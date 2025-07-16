package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/corani/cubit/internal/analyzer"
	"github.com/corani/cubit/internal/codegen"
	"github.com/corani/cubit/internal/ir"
	"github.com/corani/cubit/internal/loader"
)

func withExt(filename, ext string) string {
	// replace the existing extension with the new one
	current := filepath.Ext(filename)

	if current != "" {
		return filename[:len(filename)-len(current)] + ext
	}

	return filename + ext
}

func main() {
	var writeAST, writeSSA, run, help bool

	flag.BoolVar(&writeAST, "ast", false, "write AST to file")
	flag.BoolVar(&writeSSA, "ssa", false, "write SSA code to file")
	flag.BoolVar(&run, "run", false, "run the compiled code")
	flag.BoolVar(&help, "help", false, "show help message")

	flag.Parse()

	if help {
		fmt.Println("Usage: go run main.go [options] [source_file]")
		fmt.Println("Options:")
		flag.PrintDefaults()
		return
	}

	srcFile := "examples/example.in"
	if flag.NArg() > 0 {
		srcFile = flag.Arg(0)
	}

	// ensure the source file exists
	if _, err := os.Stat(srcFile); errors.Is(err, os.ErrNotExist) {
		fmt.Printf("Source file %s does not exist.\n", srcFile)
		os.Exit(1)
	}

	// output directory is relative to the source file
	srcDir := filepath.Dir(srcFile)
	outDir := filepath.Join(srcDir, "out")

	if err := os.MkdirAll(outDir, 0755); err != nil {
		panic(fmt.Sprintf("failed to create output directory: %v", err))
	}

	astuFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".astu"))
	asttFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".astt"))
	ssaFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".ssa"))
	asmFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".s"))
	binFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ""))

	ldr := loader.NewLoader()

	unit, err := ldr.Load(srcFile)
	if err != nil {
		panic(fmt.Sprintf("failed to load source and imports: %v", err))
	}

	if writeAST {
		// Before type checking
		if err := os.WriteFile(astuFile, []byte(unit.String()), 0644); err != nil {
			panic(fmt.Sprintf("failed to write AST file: %v", err))
		}
	}

	// Type checking
	if err := analyzer.Check(unit); err != nil {
		panic(fmt.Sprintf("type checking failed: %v", err))
	}

	if writeAST {
		// After type checking
		if err := os.WriteFile(asttFile, []byte(unit.String()), 0644); err != nil {
			panic(fmt.Sprintf("failed to write AST file: %v", err))
		}
	}

	lowUnit, err := ir.Lower(unit)
	if err != nil {
		panic(fmt.Sprintf("failed to lower IR: %v", err))
	}

	if writeSSA {
		if err := codegen.WriteSSA(lowUnit, ssaFile); err != nil {
			panic(fmt.Sprintf("failed to write SSA file: %v", err))
		}
	}

	if err := codegen.GenerateAssembly(srcFile, lowUnit, asmFile); err != nil {
		panic(fmt.Sprintf("failed to generate assembly: %v", err))
	}

	if err := codegen.Compile(asmFile, binFile); err != nil {
		panic(fmt.Sprintf("failed to compile assembly: %v", err))
	}

	if run {
		// run and check the exit code
		cmd := exec.Command(binFile)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr

		if err := cmd.Run(); err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				fmt.Printf("Program exited with code %d\n", exitErr.ExitCode())

				os.Exit(exitErr.ExitCode())
			} else {
				panic(fmt.Sprintf("failed to run compiled binary: %v", err))
			}
		}
	}
}
