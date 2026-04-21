package analyzer

import (
	"sort"
	"strconv"
	"strings"

	"github.com/corani/cubit/internal/ast"
)

// inferBindings walks a generic param type and a concrete arg type in parallel,
// extracting type and value bindings.
//
//nolint:cyclop
func inferBindings(
	paramType *ast.Type, argType *ast.Type, typeBindings map[string]*ast.Type, valueBindings map[string]int,
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
	case ast.TypeUnknown, ast.TypeInt, ast.TypeBool, ast.TypeVoid, ast.TypeString, ast.TypeAny,
		ast.TypeVararg:
		fallthrough
	default:
	}
}

// substituteType replaces TypeGeneric and SizeSymbol occurrences with their bindings.
//
//nolint:cyclop
func substituteType(
	ty *ast.Type, //nolint:varnamelen
	typeBindings map[string]*ast.Type, valueBindings map[string]int,
) *ast.Type {
	if ty == nil {
		return nil
	}

	switch ty.Kind {
	case ast.TypeGeneric:
		if concrete, ok := typeBindings[ty.Symbol]; ok {
			return concrete
		}

		return ty // unbound — leave as-is (caller should error)
	case ast.TypeArray:
		elem := substituteType(ty.Elem, typeBindings, valueBindings)
		size := ty.Size

		if size != nil && size.Kind == ast.SizeSymbol {
			if v, ok := valueBindings[size.Symbol]; ok {
				size = ast.NewSizeLiteral(v)
			}
		}

		return ast.NewArrayType(elem, size, ty.Loc)
	case ast.TypePointer:
		return ast.NewPointerType(substituteType(ty.Elem, typeBindings, valueBindings), 1, ty.Loc)
	case ast.TypeVararg:
		return ast.NewVarargType(substituteType(ty.Elem, typeBindings, valueBindings), ty.Loc)
	case ast.TypeUnknown, ast.TypeInt, ast.TypeBool, ast.TypeString, ast.TypeVoid, ast.TypeAny:
		fallthrough
	default:
		return ty
	}
}

// mangleName produces a deterministic concrete name for a generic instantiation.
// e.g. len with $N=3, $T=int → "len__3_int".
func mangleName(
	baseName string, genericParams []*ast.GenericParam, typeBindings map[string]*ast.Type, valueBindings map[string]int,
) string {
	// Build ordered list of param names as declared
	parts := make([]string, 0, len(genericParams))

	//nolint:varnamelen // gp
	for _, gp := range genericParams {
		switch gp.Kind {
		case ast.GenericType:
			if t, ok := typeBindings[gp.Symbol]; ok {
				parts = append(parts, t.String())
			}
		case ast.GenericValue:
			if v, ok := valueBindings[gp.Symbol]; ok {
				parts = append(parts, strconv.Itoa(v))
			}
		}
	}

	// Sort for determinism (in case params were inferred in different orders)
	sort.Strings(parts)

	return baseName + "__" + strings.Join(parts, "_")
}

// validateGenericParams checks that any generic param used as an array size
// is a GenericValue, not a GenericType.
//
//nolint:cyclop
func validateGenericParams(fn *ast.FuncDef) { //nolint:varnamelen
	if len(fn.GenericParams) == 0 {
		return
	}

	paramKind := make(map[string]ast.GenericParamKind, len(fn.GenericParams))
	for _, gp := range fn.GenericParams {
		paramKind[gp.Symbol] = gp.Kind
	}

	// For recursive call.
	var checkType func(ty *ast.Type)

	checkType = func(ty *ast.Type) { //nolint:varnamelen
		if ty == nil {
			return
		}

		if ty.Kind == ast.TypeArray {
			if ty.Size != nil && ty.Size.Kind == ast.SizeSymbol {
				if kind, ok := paramKind[ty.Size.Symbol]; ok && kind == ast.GenericType {
					fn.Location().Errorf(
						"array size '$%s' must be a value parameter — did you mean '[$%s: int]'?",
						ty.Size.Symbol, ty.Size.Symbol,
					)
				}
			}

			checkType(ty.Elem)
		} else if ty.Elem != nil {
			checkType(ty.Elem)
		}
	}

	for _, param := range fn.Params {
		checkType(param.Type)
	}
}

// monomorphizeCall handles a call to a generic function: it infers type/value
// bindings from the call arguments, produces (or reuses) a concrete FuncDef,
// and updates call.FuncDef and call.Type in place.
// Returns true if the call was handled (callee is generic), false otherwise.
//
//nolint:cyclop,funlen
func (tc *TypeChecker) monomorphizeCall(call *ast.Call) bool {
	if len(call.FuncDef.GenericParams) == 0 {
		return false
	}

	// Type-check all arguments to get their concrete types.
	for i, arg := range call.Args {
		argType, _ := tc.visitNode(arg.Value)
		call.Args[i].Type = argType
	}

	// Infer bindings by walking param types vs arg types.
	typeBindings := map[string]*ast.Type{}
	valueBindings := map[string]int{}

	for i, param := range call.FuncDef.Params {
		if i >= len(call.Args) {
			break
		}

		inferBindings(param.Type, call.Args[i].Type, typeBindings, valueBindings)
	}

	// Check that all generic params were bound.
	//
	//nolint:varnamelen // gp
	for _, gp := range call.FuncDef.GenericParams {
		switch gp.Kind {
		case ast.GenericType:
			if _, ok := typeBindings[gp.Symbol]; !ok {
				call.Location().Errorf("call to '%s': could not infer type for generic parameter '$%s'",
					call.Ident, gp.Symbol)

				//nolint:exhaustruct
				tc.lastType = &ast.Type{Kind: ast.TypeUnknown}

				return true
			}
		case ast.GenericValue:
			if _, ok := valueBindings[gp.Symbol]; !ok {
				call.Location().Errorf("call to '%s': could not infer value for generic parameter '$%s'",
					call.Ident, gp.Symbol)

				//nolint:exhaustruct
				tc.lastType = &ast.Type{Kind: ast.TypeUnknown}

				return true
			}
		}
	}

	// Produce a mangled name and check if we already have this instantiation.
	mangledName := mangleName(call.Ident, call.FuncDef.GenericParams, typeBindings, valueBindings)

	if existing, ok := tc.lookupSymbol(mangledName); ok && existing.IsFunc {
		call.FuncDef = existing.FuncDef
	} else {
		// Clone and substitute.
		concrete := cloneFuncDef(call.FuncDef, typeBindings, valueBindings)
		concrete.Ident = mangledName

		// Register in global scope and in the compilation unit so IR lowering sees it.
		tc.addGlobalSymbol(NewSymbolFunc(mangledName, concrete.ReturnType, concrete))

		if tc.unit != nil {
			tc.unit.Funcs = append(tc.unit.Funcs, concrete)
		}

		call.FuncDef = concrete
	}

	call.Type = call.FuncDef.ReturnType
	tc.lastType = call.Type

	return true
}

// substituteExpr replaces any VariableRef whose Ident matches a value binding
// with the corresponding integer Literal. Other expression types are cloned
// shallowly (their structure is preserved) with recursive substitution.
//
//nolint:ireturn
func substituteExpr(expr ast.Expression, valueBindings map[string]int) ast.Expression { //nolint:cyclop
	if expr == nil {
		return nil
	}

	switch e := expr.(type) { //nolint:varnamelen // e
	case *ast.VariableRef:
		if v, ok := valueBindings[e.Ident]; ok {
			return ast.NewIntLiteral(v, e.Location())
		}

		return e
	case *ast.Literal:
		return e // already concrete
	case *ast.Binop:
		return ast.NewBinop(e.Operation,
			substituteExpr(e.Lhs, valueBindings),
			substituteExpr(e.Rhs, valueBindings),
			e.Location())
	case *ast.UnaryOp:
		return ast.NewUnaryOp(e.Operation,
			substituteExpr(e.Expr, valueBindings),
			e.Location())
	case *ast.ArrayIndex:
		return ast.NewArrayIndex(
			substituteExpr(e.Array, valueBindings),
			substituteExpr(e.Index, valueBindings),
			e.Location())
	case *ast.Deref:
		return ast.NewDeref(substituteExpr(e.Expr, valueBindings), e.Location())
	case *ast.Call:
		args := make([]ast.Arg, len(e.Args))

		for i, a := range e.Args {
			args[i] = ast.NewArg(a.Ident,
				substituteExpr(a.Value, valueBindings), a.Type, a.Location())
		}

		call := ast.NewCall(e.Location(), e.Namespace, e.Ident, args...)
		call.FuncDef = e.FuncDef
		call.Type = e.Type

		return call
	default:
		// TODO: handle any new expression types added in the future
		return expr
	}
}

// substituteBody clones a Body, replacing generic value parameter references
// (VariableRefs whose Ident is a key in valueBindings) with integer Literals.
// Returns nil if body is nil (builtin / extern functions have no body).
//
//nolint:cyclop
func substituteBody(body *ast.Body, valueBindings map[string]int) *ast.Body {
	if body == nil || len(valueBindings) == 0 {
		return body
	}

	instrs := make([]ast.Instruction, 0, len(body.Instructions))

	for _, instr := range body.Instructions {
		switch s := instr.(type) { //nolint:varnamelen // s
		case *ast.Return:
			instrs = append(instrs, ast.NewReturn(s.Location(), s.Type, substituteExpr(s.Value, valueBindings)))
		case *ast.Assign:
			instrs = append(instrs, ast.NewAssign(s.LHS, substituteExpr(s.Value, valueBindings), s.Type, s.Location()))
		case *ast.Declare:
			instrs = append(instrs, s) // no expressions in a bare declare
		case *ast.Call:
			//nolint:forcetypeassert
			instrs = append(instrs, substituteExpr(s, valueBindings).(*ast.Call))
		case *ast.If:
			init := substituteInstrs(s.Init, valueBindings)
			then := substituteBody(s.Then, valueBindings)
			els := substituteBody(s.Else, valueBindings)
			instrs = append(instrs, ast.NewIf(s.Location(), init, substituteExpr(s.Cond, valueBindings), then, els))
		case *ast.For:
			init := substituteInstrs(s.Init, valueBindings)
			post := substituteInstrs(s.Post, valueBindings)
			body := substituteBody(s.Body, valueBindings)
			instrs = append(instrs, ast.NewFor(s.Location(), init, substituteExpr(s.Cond, valueBindings), post, body))
		default:
			// TODO: handle any new instruction types added in the future
			instrs = append(instrs, instr)
		}
	}

	return ast.NewBody(instrs, body.Location())
}

func substituteInstrs(instrs []ast.Instruction, valueBindings map[string]int) []ast.Instruction {
	result := make([]ast.Instruction, len(instrs))
	for i, instr := range instrs {
		// Reuse substituteBody for single-instruction slice would waste an allocation;
		// inline the cast via a temporary single-body call.
		tmp := substituteBody(ast.NewBody([]ast.Instruction{instr}, instr.Location()), valueBindings)
		result[i] = tmp.Instructions[0]
	}

	return result
}

// parameters substituted by their bindings.
func cloneFuncDef(orig *ast.FuncDef, typeBindings map[string]*ast.Type, valueBindings map[string]int) *ast.FuncDef {
	clone := ast.NewFuncDef(orig.Ident, orig.Attributes, orig.Loc)

	// Clear generic params — the clone is concrete
	clone.GenericParams = nil
	clone.ReturnType = substituteType(orig.ReturnType, typeBindings, valueBindings)
	clone.Body = substituteBody(orig.Body, valueBindings)

	for _, p := range orig.Params {
		concreteType := substituteType(p.Type, typeBindings, valueBindings)
		clone.Params = append(clone.Params, ast.NewFuncParam(p.Ident, concreteType, p.Value, p.Attributes, p.Loc))
	}

	return clone
}
