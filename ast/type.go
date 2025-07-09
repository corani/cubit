package ast

import (
	"fmt"

	"github.com/corani/refactored-giggle/lexer"
)

// TypeKind represents the basic types in the language for type checking.
type TypeKind int

const (
	TypeUnknown TypeKind = iota
	TypeInt
	TypeBool
	TypeString
	TypeVoid
	TypePointer
)

// Type is a recursive type structure for basic and pointer types.
type Type struct {
	Kind TypeKind
	Elem *Type // non-nil if Kind == TypePointer
	Loc  lexer.Location
}

func NewType(kind TypeKind, location lexer.Location) *Type {
	return &Type{
		Kind: kind,
		Elem: nil,
		Loc:  location,
	}
}

func NewPointerType(elem *Type, depth int, location lexer.Location) *Type {
	for range depth {
		elem = &Type{
			Kind: TypePointer,
			Elem: elem,
			Loc:  location,
		}
	}

	return elem
}

func (t *Type) Location() lexer.Location {
	return t.Loc
}

func (t *Type) String() string {
	if t == nil {
		return "<nil>"
	}

	switch t.Kind {
	case TypeInt:
		return "int"
	case TypeBool:
		return "bool"
	case TypeString:
		return "string"
	case TypeVoid:
		return "void"
	case TypePointer:
		return fmt.Sprintf("^%s", t.Elem)
	default:
		return "unknown"
	}
}
