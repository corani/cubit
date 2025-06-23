package parser

import (
	"fmt"

	"github.com/corani/refactored-giggle/ast"
)

func Check(unit *ast.CompilationUnit) error {
	for _, fn := range unit.Funcs {
		if fn.Body == nil {
			continue
		}
		// Symbol table for variable types in this function
		vars := make(map[string]ast.TypeKind)
		// Add function parameters to the symbol table
		for _, param := range fn.Params {
			vars[param.Ident] = param.Type
		}
		changed := true
		// Iterate until no new types are inferred
		for changed {
			changed = false
			for _, instr := range fn.Body.Instructions {
				switch a := instr.(type) {
				case *ast.Assign:
					rhsType := inferExprType(a.Value, vars)
					if a.Type == ast.TypeUnknown {
						if prev, ok := vars[a.Ident]; !ok || prev != rhsType {
							a.Type = rhsType
							vars[a.Ident] = rhsType
							changed = true
						}
					} else {
						if a.Type != rhsType {
							return fmt.Errorf("type error: variable '%s' declared as %s but assigned %s", a.Ident, a.Type, rhsType)
						}
						if prev, ok := vars[a.Ident]; !ok || prev != a.Type {
							vars[a.Ident] = a.Type
							changed = true
						}
					}
				case *ast.Call:
					// Check function existence and argument types
					var fnDef *ast.FuncDef
					for i := range unit.Funcs {
						if unit.Funcs[i].Ident == a.Ident {
							fnDef = &unit.Funcs[i]
							break
						}
					}
					if fnDef == nil {
						return fmt.Errorf("call to undefined function '%s'", a.Ident)
					}
					if len(a.Args) != len(fnDef.Params) {
						return fmt.Errorf("call to '%s' expects %d arguments, got %d", a.Ident, len(fnDef.Params), len(a.Args))
					}
					for i, arg := range a.Args {
						argType := inferExprType(arg.Value, vars)
						paramType := fnDef.Params[i].Type
						if paramType != ast.TypeUnknown && argType != paramType {
							return fmt.Errorf("call to '%s': argument %d type mismatch: expected %s, got %s", a.Ident, i+1, paramType, argType)
						}
					}
				case *ast.Return:
					// Optionally: check return type matches fn.ReturnType
				}
			}
		}
	}
	return nil
}

// inferExprType infers the type of an expression, using the symbol table for variable references.
func inferExprType(expr ast.Expression, vars map[string]ast.TypeKind) ast.TypeKind {
	switch e := expr.(type) {
	case *ast.Literal:
		return e.Type
	case *ast.Binop:
		// For now, assume both sides must be the same type
		lhs := inferExprType(e.Lhs, vars)
		rhs := inferExprType(e.Rhs, vars)
		if lhs == rhs {
			if e.Type != lhs {
				e.Type = lhs
			}
			return lhs
		}
		e.Type = ast.TypeUnknown
		return ast.TypeUnknown
	case *ast.VariableRef:
		if t, ok := vars[e.Ident]; ok {
			if e.Type != t {
				e.Type = t
			}
			return t
		}
		return e.Type
	default:
		return ast.TypeUnknown
	}
}
