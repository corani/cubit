package loader

import (
	"errors"
	"io"
	"os"
	"path/filepath"

	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
	"github.com/corani/cubit/internal/parser"
)

type Loader struct {
	visited map[string]*ast.CompilationUnit
}

func NewLoader() *Loader {
	return &Loader{
		visited: make(map[string]*ast.CompilationUnit),
	}
}

// Load parses the given file and all its imports.
func (l *Loader) Load(filename string) (*ast.CompilationUnit, error) {
	absPath, err := filepath.Abs(filename)
	if err != nil {
		return nil, err
	}

	if cu, ok := l.visited[absPath]; ok {
		return cu, nil // already parsed
	}

	f, err := os.Open(absPath)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner, err := lexer.NewScanner(absPath, f)
	if err != nil {
		return nil, err
	}

	tokens, err := lexer.NewLexer(scanner).Tokens()
	if err != nil {
		return nil, err
	}

	pr := parser.New(tokens)

	cu, err := pr.Parse()
	if err != nil && !errors.Is(err, io.EOF) {
		return nil, err
	}

	l.visited[absPath] = cu

	for alias, importPath := range cu.Imports {
		// Special-case: import "core" brings in core.in into the global namespace
		if importPath.Name == "core" {
			subCU, err := l.Load("stdlib/core/core.in")
			if err != nil {
				return nil, err
			}

			// Merge subCU's definitions into global namespace
			cu.Types = append(cu.Types, subCU.Types...)
			cu.Data = append(cu.Data, subCU.Data...)
			cu.Funcs = append(cu.Funcs, subCU.Funcs...)
		} else {
			// For other imports, load the package and attach to import entry
			importFile := filepath.Join("stdlib", importPath.Name, importPath.Name+".in")
			subCU, err := l.Load(importFile)
			if err != nil {
				return nil, err
			}

			// Attach loaded unit to import entry
			entry := importPath
			entry.Unit = subCU
			cu.Imports[alias] = entry
		}
	}

	return cu, nil
}
