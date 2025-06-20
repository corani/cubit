package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/corani/refactored-giggle/codegen"
	"github.com/corani/refactored-giggle/lexer"
	parserpkg "github.com/corani/refactored-giggle/parser"
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
	var writeTokens, writeSSA, run, help bool

	flag.BoolVar(&writeTokens, "tok", false, "write tokens to file")
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

	srcFile := "example.in"
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

	tokFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".tok"))
	ssaFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".ssa"))
	asmFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ".s"))
	binFile := filepath.Join(outDir, withExt(filepath.Base(srcFile), ""))

	reader, err := os.Open(srcFile)
	if err != nil {
		panic(err)
	}
	defer reader.Close()

	scanner, err := lexer.NewScanner(srcFile, reader)
	if err != nil {
		panic(fmt.Sprintf("failed to create scanner: %v", err))
	}

	tokenizer := lexer.NewTokenizer(scanner)

	tokens, err := tokenizer.Tokens()
	if err != nil {
		panic(fmt.Sprintf("failed to tokenize: %v", err))
	}

	if writeTokens {
		var sb strings.Builder

		for _, token := range tokens {
			fmt.Fprintln(&sb, token)
		}

		if err := os.WriteFile(tokFile, []byte(sb.String()), 0644); err != nil {
			panic(err)
		}
	}

	pr := parserpkg.New(tokens)

	unit, err := pr.Parse()
	if err != nil && !errors.Is(err, io.EOF) {
		panic(fmt.Sprintf("failed to parse: %v", err))
	}

	// Type checking
	if err := parserpkg.Check(unit); err != nil {
		panic(fmt.Sprintf("type checking failed: %v", err))
	}

	if writeSSA {
		if err := codegen.WriteSSA(unit, ssaFile); err != nil {
			panic(fmt.Sprintf("failed to write SSA file: %v", err))
		}
	}

	if err := codegen.GenerateAssembly(srcFile, unit, asmFile); err != nil {
		panic(fmt.Sprintf("failed to generate assembly: %v", err))
	}

	if err := codegen.Compile(asmFile, binFile, run); err != nil {
		panic(fmt.Sprintf("failed to compile assembly: %v", err))
	}
}
