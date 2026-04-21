package analyzer

import (
	"fmt"
	"sort"
	"strings"

	"github.com/corani/cubit/internal/ast"
)

// inferBindings walks a generic param type and a concrete arg type in parallel,
// extracting type and value bindings.
func inferBindings(
	paramType *ast.Type,
	argType *ast.Type,
	typeBindings map[string]*ast.Type,
	valueBindings map[string]int,
) {
	if paramType == nil || argType == nil {
		return
	}

	switch paramType.Kind {
	case ast.TypeGeneric:
		// $T matched against a concrete type
		typeBindings[paramType.Symbol] = argType
	case ast.TypeArray:
		if argType.Kind != ast.TypeArray {
			return
		}
		// Bind value parameter from array size
		if paramType.Size != nil && paramType.Size.Kind == ast.SizeSymbol {
			if argType.Size != nil && argType.Size.Kind == ast.SizeLiteral {
				valueBindings[paramType.Size.Symbol] = argType.Size.Value
			}
		}
		// Recurse into element type
		inferBindings(paramType.Elem, argType.Elem, typeBindings, valueBindings)
	case ast.TypePointer:
		if argType.Kind != ast.TypePointer {
			return
		}
		inferBindings(paramType.Elem, argType.Elem, typeBindings, valueBindings)
	}
}

// substituteType replaces TypeGeneric and SizeSymbol occurrences with their bindings.
func substituteType(
	t *ast.Type,
	typeBindings map[string]*ast.Type,
	valueBindings map[string]int,
) *ast.Type {
	if t == nil {
		return nil
	}

	switch t.Kind {
	case ast.TypeGeneric:
		if concrete, ok := typeBindings[t.Symbol]; ok {
			return concrete
		}
		return t // unbound — leave as-is (caller should error)
	case ast.TypeArray:
		elem := substituteType(t.Elem, typeBindings, valueBindings)
		size := t.Size
		if size != nil && size.Kind == ast.SizeSymbol {
			if v, ok := valueBindings[size.Symbol]; ok {
				size = ast.NewSizeLiteral(v)
			}
		}
		return ast.NewArrayType(elem, size, t.Loc)
	case ast.TypePointer:
		return ast.NewPointerType(substituteType(t.Elem, typeBindings, valueBindings), 1, t.Loc)
	case ast.TypeVararg:
		return ast.NewVarargType(substituteType(t.Elem, typeBindings, valueBindings), t.Loc)
	default:
		return t
	}
}

// mangleName produces a deterministic concrete name for a generic instantiation.
// e.g. len with $N=3, $T=int → "len__3_int"
func mangleName(baseName string, genericParams []*ast.GenericParam, typeBindings map[string]*ast.Type, valueBindings map[string]int) string {
	// Build ordered list of param names as declared
	parts := make([]string, 0, len(genericParams))

	for _, gp := range genericParams {
		switch gp.Kind {
		case ast.GenericType:
			if t, ok := typeBindings[gp.Symbol]; ok {
				parts = append(parts, t.String())
			}
		case ast.GenericValue:
			if v, ok := valueBindings[gp.Symbol]; ok {
				parts = append(parts, fmt.Sprintf("%d", v))
			}
		}
	}

	// Sort for determinism (in case params were inferred in different orders)
	sort.Strings(parts)

	return baseName + "__" + strings.Join(parts, "_")
}

// cloneFuncDef creates a concrete copy of a generic FuncDef with all generic
// parameters substituted by their bindings.
func cloneFuncDef(orig *ast.FuncDef, typeBindings map[string]*ast.Type, valueBindings map[string]int) *ast.FuncDef {
	clone := ast.NewFuncDef(orig.Ident, orig.Attributes, orig.Loc)
	// Clear generic params — the clone is concrete
	clone.GenericParams = nil
	clone.ReturnType = substituteType(orig.ReturnType, typeBindings, valueBindings)
	clone.Body = orig.Body // body is shared (read-only for builtins; no generic refs in body)

	for _, p := range orig.Params {
		concreteType := substituteType(p.Type, typeBindings, valueBindings)
		clone.Params = append(clone.Params, ast.NewFuncParam(p.Ident, concreteType, p.Value, p.Attributes, p.Loc))
	}

	return clone
}
