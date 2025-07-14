package ast

import (
	"fmt"

	"github.com/corani/cubit/lexer"
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
	TypeArray
)

// Type is a recursive type structure for basic and pointer types.
type Type struct {
	Kind TypeKind
	Elem *Type // non-nil if Kind == TypePointer or TypeArray
	Size *Size // if TypeArray
	Loc  lexer.Location
}

func NewType(kind TypeKind, location lexer.Location) *Type {
	return &Type{
		Kind: kind,
		Elem: nil,
		Size: nil,
		Loc:  location,
	}
}

func NewPointerType(elem *Type, depth int, location lexer.Location) *Type {
	for range depth {
		elem = &Type{
			Kind: TypePointer,
			Elem: elem,
			Size: nil,
			Loc:  location,
		}
	}

	return elem
}

func NewArrayType(elem *Type, size *Size, location lexer.Location) *Type {
	return &Type{
		Kind: TypeArray,
		Elem: elem,
		Size: size,
		Loc:  location,
	}
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
	case TypeArray:
		return fmt.Sprintf("[%s]%s", t.Size, t.Elem)
	default:
		return "unknown"
	}
}

type SizeKind int

const (
	SizeLiteral SizeKind = iota
	SizeSymbol
)

type Size struct {
	Kind   SizeKind
	Value  int    // for SizeLiteral
	Symbol string // for SizeSymbol
}

func NewSizeLiteral(value int) *Size {
	return &Size{
		Kind:  SizeLiteral,
		Value: value,
	}
}

func NewSizeSymbol(symbol string) *Size {
	return &Size{
		Kind:   SizeSymbol,
		Symbol: symbol,
	}
}

func (s *Size) String() string {
	if s == nil {
		return "<nil>"
	}

	switch s.Kind {
	case SizeLiteral:
		return fmt.Sprintf("%d", s.Value)
	case SizeSymbol:
		return s.Symbol
	default:
		return "unknown"
	}
}
