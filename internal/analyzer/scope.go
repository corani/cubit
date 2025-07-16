package analyzer

import (
	"fmt"

	"github.com/corani/cubit/internal/ast"
)

// Symbol represents a variable or function in the symbol table.
type Symbol struct {
	Name        string
	Type        *ast.Type
	IsFunc      bool
	FuncDef     *ast.FuncDef // Only set if IsFunc
	Declaration *ast.Declare
}

func NewSymbolFunc(name string, ty *ast.Type, def *ast.FuncDef) *Symbol {
	return &Symbol{
		Name:        name,
		Type:        ty,
		IsFunc:      true,
		FuncDef:     def,
		Declaration: nil,
	}
}

func NewSymbolVariable(name string, ty *ast.Type, decl *ast.Declare) *Symbol {
	return &Symbol{
		Name:        name,
		Type:        ty,
		IsFunc:      false,
		FuncDef:     nil,
		Declaration: decl,
	}
}

func (s *Symbol) UpdateType(ty *ast.Type) error {
	if s.Type != nil && s.Type.Kind != ast.TypeUnknown {
		return fmt.Errorf("symbol %s already has a type: %s", s.Name, s.Type)
	}

	s.Type = ty

	if s.Declaration == nil {
		return nil
	}

	if s.Declaration.Type != nil && s.Declaration.Type.Kind != ast.TypeUnknown {
		return fmt.Errorf("symbol %s already has a declaration type: %s", s.Name, s.Declaration.Type)
	}

	s.Declaration.Type = ty

	return nil
}

// Scope management helpers
func (tc *TypeChecker) pushScope() {
	tc.scopes = append(tc.scopes, make(map[string]*Symbol))
}

func (tc *TypeChecker) popScope() {
	if len(tc.scopes) > 0 {
		tc.scopes = tc.scopes[:len(tc.scopes)-1]
	}
}

func (tc *TypeChecker) withScope(fn func()) {
	tc.pushScope()
	defer tc.popScope()

	fn()
}

func (tc *TypeChecker) addSymbol(sym *Symbol) {
	if len(tc.scopes) == 0 {
		tc.pushScope()
	}
	tc.scopes[len(tc.scopes)-1][sym.Name] = sym
}

func (tc *TypeChecker) lookupSymbol(name string) (*Symbol, bool) {
	for i := len(tc.scopes) - 1; i >= 0; i-- {
		if sym, ok := tc.scopes[i][name]; ok {
			return sym, true
		}
	}
	return nil, false
}
