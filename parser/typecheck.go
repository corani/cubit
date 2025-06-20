package parser

import (
	"fmt"

	"github.com/corani/refactored-giggle/ast"
)

// Check runs type checking on the given CompilationUnit.

func checkFuncDef(unit *ast.CompilationUnit, fn *ast.FuncDef) error {
	// Build symbol table for parameters
	symbols := make(map[string]ast.TypeKind)
	for _, param := range fn.Params {
		symbols[string(param.Ident)] = param.Ty
	}

	// Add all locals from all blocks to the symbol table
	for _, block := range fn.Blocks {
		for name, ty := range block.Locals {
			symbols[name] = ty
		}
	}

	// Check each block
	for _, block := range fn.Blocks {
		for _, instr := range block.Instructions {
			switch v := instr.(type) {
			case *ast.Ret:
				if v.Val != nil {
					if v.Val.Ty != fn.ReturnType {
						return typeError("return type mismatch in function '%s': got %s, want %s", fn.Ident, v.Val.Ty, fn.ReturnType)
					}
				} else if fn.ReturnType != ast.TypeVoid {
					return typeError("missing return value in function '%s' with non-void return type", fn.Ident)
				}
			case *ast.Call:
				// Check function call argument types
				callee := string(v.Val.Ident)
				sig, ok := fnUnitSig(unit, callee)
				if !ok {
					return typeError("call to unknown function '%s' in '%s'", callee, fn.Ident)
				}
				if len(sig.ParamTypes) != len(v.Args) {
					return typeError("call to '%s' in '%s': argument count mismatch (got %d, want %d)", callee, fn.Ident, len(v.Args), len(sig.ParamTypes))
				}
				for i, arg := range v.Args {
					argType := arg.Val.Ty
					// If the argument is an identifier and type is unknown, look up in symbol table
					if argType == ast.TypeUnknown && arg.Val.Ident != "" {
						if t, ok := symbols[string(arg.Val.Ident)]; ok {
							argType = t
							arg.Val.Ty = t // Set the type for later phases
						}
					}
					if argType != sig.ParamTypes[i] {
						return typeError("call to '%s' in '%s': argument %d type mismatch (got %s, want %s)", callee, fn.Ident, i+1, argType, sig.ParamTypes[i])
					}
				}
			case *ast.Add:
				// Typecheck Add: both operands must be int
				lhsType := v.Lhs.Ty
				rhsType := v.Rhs.Ty
				// If lhs is an identifier and type is unknown, look up in symbol table
				if lhsType == ast.TypeUnknown && v.Lhs.Ident != "" {
					if t, ok := symbols[string(v.Lhs.Ident)]; ok {
						lhsType = t
						v.Lhs.Ty = t
					}
				}
				if rhsType == ast.TypeUnknown && v.Rhs.Ident != "" {
					if t, ok := symbols[string(v.Rhs.Ident)]; ok {
						rhsType = t
						v.Rhs.Ty = t
					}
				}
				if lhsType != ast.TypeInt || rhsType != ast.TypeInt {
					return typeError("add instruction in function '%s': operands must be int (got %s, %s)", fn.Ident, lhsType, rhsType)
				}
				// Set result type to int for later phases
				v.Ret.Ty = ast.TypeInt
			}
		}
	}
	return nil
}

// fnUnitSig finds the function signature for a callee, searching the compilation unit.
func fnUnitSig(unit *ast.CompilationUnit, name string) (ast.FuncSig, bool) {
	sig, ok := unit.FuncSigs[name]
	return sig, ok
}

func Check(unit *ast.CompilationUnit) error {
	for i := range unit.FuncDefs {
		fn := &unit.FuncDefs[i]
		if err := checkFuncDef(unit, fn); err != nil {
			return err
		}
	}
	return nil
}

func typeError(format string, args ...interface{}) error {
	return fmt.Errorf("type error: "+format, args...)
}
