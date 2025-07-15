package loader

import (
	"errors"
	"io"
	"os"
	"path/filepath"

	"github.com/corani/cubit/ast"
	"github.com/corani/cubit/lexer"
	"github.com/corani/cubit/parser"
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
		_ = alias

		// Special-case: import "core" brings in core.in into the global namespace
		if importPath == "core" {
			subCU, err := l.Load("examples/core.in")
			if err != nil {
				return nil, err
			}

			// Merge subCU's definitions into cu
			cu.Types = append(cu.Types, subCU.Types...)
			cu.Data = append(cu.Data, subCU.Data...)
			cu.Funcs = append(cu.Funcs, subCU.Funcs...)
		} else {
			// Report an error for now
			return nil, errors.New("import handling not implemented: " + importPath)
		}

		// In the future, handle other imports here
	}

	return cu, nil
}
