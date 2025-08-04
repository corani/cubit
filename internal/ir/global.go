package ir

import (
	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
)

// visitor implements ast.Visitor and produces IR nodes.
type literalGlobal struct {
	kind   ast.TypeKind
	value  any
	global *Val
}

// getOrCreateLiteralGlobal checks if a literal global already exists, and if not, creates it.
func getOrCreateLiteralGlobal[T string | int](v *visitor, loc lexer.Location, kind ast.TypeKind, value T) *Val {
	// NOTE(daniel): this is a function, as methods cannot be generic in Go.
	// Check if the literal global already exists
	for _, entry := range v.literalGlobals {
		if entry.kind == kind && entry.value == value {
			return entry.global
		}
	}

	// Create a new literal global if it doesn't exist
	switch kind {
	case ast.TypeString:
		sval, ok := any(value).(string)
		if !ok {
			loc.Errorf("expected string literal, got %T", value)
			return &Val{}
		}

		ident := v.nextIdent("str")
		global := NewValGlobal(loc, ident, NewAbiTyBase(BaseLong))

		v.unit.DataDefs = append(v.unit.DataDefs, NewDataDefStringZ(loc, ident, sval))
		v.literalGlobals = append(v.literalGlobals, literalGlobal{kind, sval, global})

		return global
	case ast.TypeInt:
		ival, ok := any(value).(int)
		if !ok {
			loc.Errorf("expected int literal, got %T", value)
			return &Val{}
		}

		ident := v.nextIdent("int")
		global := NewValGlobal(loc, ident, NewAbiTyBase(BaseLong))

		v.unit.DataDefs = append(v.unit.DataDefs, NewDataDefInteger(loc, ident, int64(ival)))
		v.literalGlobals = append(v.literalGlobals, literalGlobal{kind, ival, global})

		return global
	default:
		loc.Errorf("unsupported literal type for global: %s", kind)

		return &Val{}
	}
}
