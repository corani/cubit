package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
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
		fmt.Println("Usage: go run main.go [options]")
		fmt.Println("Options:")
		flag.PrintDefaults()
		return
	}

	srcFile := "example.in"
	tokFile, _ := filepath.Abs(filepath.Join("out", withExt(srcFile, ".tok")))
	ssaFile, _ := filepath.Abs(filepath.Join("out", withExt(srcFile, ".ssa")))
	asmFile, _ := filepath.Abs(filepath.Join("out", withExt(srcFile, ".s")))
	binFile, _ := filepath.Abs(filepath.Join("out", withExt(srcFile, "")))

	reader, err := os.Open(srcFile)
	if err != nil {
		panic(err)
	}
	defer reader.Close()

	scanner, err := NewScanner(srcFile, reader)
	if err != nil {
		panic(fmt.Sprintf("failed to create scanner: %v", err))
	}

	tokenizer := NewTokenizer(scanner)

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

	parser := NewParser(tokens)

	unit, err := parser.Parse()
	if err != nil && !errors.Is(err, io.EOF) {
		panic(fmt.Sprintf("failed to parse: %v", err))
	}

	code := unit.String()

	if writeSSA {
		if err := os.WriteFile(ssaFile, []byte(code), 0644); err != nil {
			panic(fmt.Sprintf("failed to write SSA file: %v", err))
		}
	}

	if err := GenerateAssembly(srcFile, code, asmFile); err != nil {
		panic(fmt.Sprintf("failed to generate assembly: %v", err))
	}

	if err := Compile(asmFile, binFile, run); err != nil {
		panic(fmt.Sprintf("failed to compile assembly: %v", err))
	}
}
