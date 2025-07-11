package analyzer

import (
	"github.com/corani/cubit/ast"
)

// TypeChecker implements a visitor for type checking the AST.
type TypeChecker struct {
	scopes     []map[string]*Symbol
	errors     []error
	lastType   *ast.Type
	lastSymbol *Symbol // set by VisitVariableRef for lvalue assignment
}

func NewTypeChecker() *TypeChecker {
	return &TypeChecker{
		scopes: nil,
		errors: nil,
	}
}

// Check runs the type checker on the given compilation unit.
func Check(unit *ast.CompilationUnit) error {
	tc := NewTypeChecker()

	unit.Accept(tc)

	// TODO(daniel): improve error reporting
	if len(tc.errors) > 0 {
		return tc.errors[0] // Return the first error for now
	}

	return nil
}

func (tc *TypeChecker) VisitCompilationUnit(unit *ast.CompilationUnit) {
	// Push global scope
	tc.pushScope()

	// Add all function definitions to the global scope first
	for _, fn := range unit.Funcs {
		tc.addSymbol(NewSymbolFunc(fn.Ident, fn.ReturnType, fn))
	}

	// Visit all function, type, and data definitions
	for _, td := range unit.Types {
		td.Accept(tc)
	}
	for _, dd := range unit.Data {
		dd.Accept(tc)
	}
	for _, fn := range unit.Funcs {
		fn.Accept(tc)
	}
}

func (tc *TypeChecker) VisitTypeDef(fn *ast.TypeDef) {
	// TODO: implement
}

func (tc *TypeChecker) VisitDataDef(fn *ast.DataDef) {
	// TODO: implement
}

func (tc *TypeChecker) VisitFuncDef(fn *ast.FuncDef) {
	tc.withScope(func() {
		// Add parameters to the new scope
		for i := range fn.Params {
			param := fn.Params[i]

			// Visit first to allow type inference/checking
			param.Accept(tc)

			tc.addSymbol(NewSymbolVariable(param.Ident, param.Type, nil))
		}

		// Type check the function body (if present)
		if fn.Body != nil {
			fn.Body.Accept(tc)
		}
	})
}

func (tc *TypeChecker) VisitFuncParam(fn *ast.FuncParam) {
	if fn.Value != nil {
		valueType, _ := tc.visitNode(fn.Value)

		if fn.Type != nil && fn.Type.Kind == ast.TypeUnknown {
			// Case 3: arg := 1 (infer type from value)
			fn.Type = valueType
		} else {
			// Case 2: arg : int = 1 (check match)
			if !tc.typeEqual(valueType, fn.Type) {
				fn.Location().Errorf("parameter '%s' declared as %s but default value is %s",
					fn.Ident, fn.Type, valueType)
			}
		}
	}

	// Case 1: arg: int (declared type, no value) or after inference
	tc.lastType = fn.Type
}

func (tc *TypeChecker) VisitBody(body *ast.Body) {
	// Type check each instruction in the body
	for _, instr := range body.Instructions {
		instr.Accept(tc)
	}
}

// VisitDeclare handles variable declarations.
func (tc *TypeChecker) VisitDeclare(d *ast.Declare) {
	// Add the declared variable to the current scope. Type may be unknown
	// at this point, and could be updated later when the variable is assigned.
	tc.addSymbol(NewSymbolVariable(d.Ident, d.Type, d))
}

// VisitAssign handles assignment to lvalues.
func (tc *TypeChecker) VisitAssign(a *ast.Assign) {
	// Typecheck the lvalue
	lvalType, lvalSymbol := tc.visitNode(a.LHS)

	// Typecheck the value
	valType, _ := tc.visitNode(a.Value)

	// If the lvalue is a variable, lastSymbol will be set
	if lvalSymbol != nil {
		if lvalSymbol.Type.Kind == ast.TypeUnknown {
			if err := lvalSymbol.UpdateType(valType); err != nil {
				a.Location().Errorf("type error: %s", err)
			}

			// If LHS is a variable, we can set its type now
			switch lvalue := a.LHS.(type) {
			case *ast.VariableRef:
				lvalue.Type = lvalSymbol.Type
			}
		} else if !tc.typeEqual(lvalType, valType) {
			a.Location().Errorf("variable '%s' declared as %s but assigned %s",
				lvalSymbol.Name, lvalSymbol.Type, valType)
		}
	} else {
		// TODO: handle pointer deref, array index, etc.
		if lvalType != nil && lvalType.Kind != ast.TypeUnknown && !tc.typeEqual(lvalType, valType) {
			a.Location().Errorf("lvalue type %s but assigned %s", lvalType, valType)
		}
	}

	a.Type = valType
	tc.lastType = valType
}

func (tc *TypeChecker) VisitCall(call *ast.Call) {
	// Look up the function definition
	sym, ok := tc.lookupSymbol(call.Ident)
	if !ok || !sym.IsFunc || sym.FuncDef == nil {
		call.Location().Errorf("call to undefined function '%s'", call.Ident)
		tc.lastType = &ast.Type{Kind: ast.TypeUnknown}

		return
	}

	fnDef := sym.FuncDef

	// Check argument count
	if len(call.Args) != len(fnDef.Params) {
		call.Location().Errorf("call to '%s' expects %d arguments, got %d",
			call.Ident, len(fnDef.Params), len(call.Args))
		tc.lastType = sym.Type

		return
	}

	// Check argument types
	for i, arg := range call.Args {
		argType, _ := tc.visitNode(arg.Value)
		paramType := fnDef.Params[i].Type

		call.Args[i].Type = argType // Set the type of the argument

		if paramType != nil && paramType.Kind != ast.TypeUnknown && !tc.typeEqual(argType, paramType) {
			arg.Location().Errorf("call to '%s': argument %d type mismatch: expected %s, got %s",
				call.Ident, i+1, paramType, argType)
		}
	}

	// Set the type of the call to the function's return type
	call.Type = fnDef.ReturnType
	tc.lastType = call.Type
}

func (tc *TypeChecker) VisitReturn(ret *ast.Return) {
	// Type check the return value (if any)
	retType := &ast.Type{Kind: ast.TypeVoid}

	if ret.Value != nil {
		retType, _ = tc.visitNode(ret.Value)
	}

	tc.lastType = retType
}

func (tc *TypeChecker) VisitLiteral(lit *ast.Literal) {
	switch lit.Type.Kind {
	case ast.TypeInt, ast.TypeBool, ast.TypeString:
		// Literals already have their type set
	case ast.TypeArray:
		// Array literals must have all elements of the same type
		// TODO(daniel): check array value types
	}

	tc.lastType = lit.Type
}

func (tc *TypeChecker) VisitVariableRef(ref *ast.VariableRef) {
	// Look up the variable in the current scope stack
	if sym, ok := tc.lookupSymbol(ref.Ident); ok && !sym.IsFunc {
		ref.Type = sym.Type
		tc.lastType = sym.Type
		tc.lastSymbol = sym
	} else {
		ref.Location().Errorf("undefined variable '%s'", ref.Ident)
		ref.Type = &ast.Type{Kind: ast.TypeUnknown}
		tc.lastType = ref.Type
		tc.lastSymbol = nil
	}
}

func (tc *TypeChecker) VisitBinop(binop *ast.Binop) {
	lhsType, _ := tc.visitNode(binop.Lhs)
	rhsType, _ := tc.visitNode(binop.Rhs)

	unknown := func(msg string, args ...any) *ast.Type {
		binop.Type = &ast.Type{Kind: ast.TypeUnknown}
		binop.Location().Errorf(msg, args...)
		return binop.Type
	}

	isInt := func(t *ast.Type) bool { return t != nil && t.Kind == ast.TypeInt }
	isBool := func(t *ast.Type) bool { return t != nil && t.Kind == ast.TypeBool }
	isPointer := func(t *ast.Type) bool { return t != nil && t.Kind == ast.TypePointer }
	isString := func(t *ast.Type) bool { return t != nil && t.Kind == ast.TypeString }

	switch binop.Operation {
	case ast.BinOpAdd, ast.BinOpSub:
		// Pointer arithmetic support
		if lhsType == nil || rhsType == nil {
			unknown("invalid operands for pointer arithmetic: %s %s %s",
				lhsType, binop.Operation, rhsType)
			break
		}
		if isPointer(lhsType) && isInt(rhsType) {
			binop.Type = lhsType
		} else if isInt(lhsType) && isPointer(rhsType) && binop.Operation == ast.BinOpAdd {
			binop.Type = rhsType
		} else if isPointer(lhsType) && isPointer(rhsType) && binop.Operation == ast.BinOpSub {
			if tc.typeEqual(lhsType, rhsType) {
				binop.Type = &ast.Type{Kind: ast.TypeInt}
			} else {
				unknown("pointer subtraction requires matching pointer types, got %s - %s",
					lhsType, rhsType)
			}
		} else if tc.typeEqual(lhsType, rhsType) {
			binop.Type = lhsType
		} else {
			unknown("invalid operands for pointer arithmetic: %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpDiv, ast.BinOpMul, ast.BinOpMod:
		if isInt(lhsType) && isInt(rhsType) {
			binop.Type = &ast.Type{Kind: ast.TypeInt}
		} else {
			unknown("invalid operands for arithmetic: %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpEq, ast.BinOpNe:
		if lhsType != nil && rhsType != nil && tc.typeEqual(lhsType, rhsType) {
			binop.Type = &ast.Type{Kind: ast.TypeBool}
		} else {
			unknown("type mismatch in equality/inequality operation: %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpLt, ast.BinOpLe, ast.BinOpGt, ast.BinOpGe:
		if lhsType != nil && rhsType != nil && tc.typeEqual(lhsType, rhsType) &&
			(isInt(lhsType) || isString(lhsType) || isPointer(lhsType)) {
			binop.Type = &ast.Type{Kind: ast.TypeBool}
		} else {
			unknown("type mismatch or invalid types in comparison operation: %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpShl, ast.BinOpShr:
		if isInt(lhsType) && isInt(rhsType) {
			binop.Type = &ast.Type{Kind: ast.TypeInt}
		} else {
			unknown("shift operation requires int operands, got %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpAnd, ast.BinOpOr:
		if isInt(lhsType) && isInt(rhsType) {
			binop.Type = &ast.Type{Kind: ast.TypeInt}
		} else {
			unknown("bitwise operation requires int operands, got %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpLogAnd, ast.BinOpLogOr:
		if isBool(lhsType) && isBool(rhsType) {
			binop.Type = &ast.Type{Kind: ast.TypeBool}
		} else {
			unknown("logical operation requires bool operands, got %s %s %s",
				lhsType, binop.Operation, rhsType)
		}
	default:
		unknown("unknown binary operation: %s", binop.Operation)
	}

	tc.lastType = binop.Type
}

func (tc *TypeChecker) VisitUnaryOp(u *ast.UnaryOp) {
	// Type check the expression
	u.Type, _ = tc.visitNode(u.Expr)

	switch u.Operation {
	case ast.UnaryOpMinus:
		if u.Type == nil || u.Type.Kind != ast.TypeInt {
			u.Location().Errorf("unary minus requires int type, got %s", u.Type)
			u.Type = &ast.Type{Kind: ast.TypeUnknown}
		}
	default:
		u.Location().Errorf("unknown unary operation: %s", u.Operation)
		u.Type = &ast.Type{Kind: ast.TypeUnknown}
	}

	tc.lastType = u.Type
}

func (tc *TypeChecker) VisitIf(iff *ast.If) {
	// If statements introduce a new scope for variables (e.g. initializer)
	tc.withScope(func() {
		// Type check the initializers, if present
		for _, init := range iff.Init {
			init.Accept(tc)
		}

		// Type check the condition
		condType, _ := tc.visitNode(iff.Cond)
		if condType == nil || condType.Kind != ast.TypeBool {
			iff.Location().Errorf("if condition must be bool, got %s", condType)
		}

		// Type check the 'then' branch
		iff.Then.Accept(tc)

		// Type check the 'else' branch, if present
		if iff.Else != nil {
			iff.Else.Accept(tc)
		}

		tc.lastType = &ast.Type{Kind: ast.TypeVoid} // if is a statement, not an expression
	})
}

func (tc *TypeChecker) VisitFor(f *ast.For) {
	// For statements introduce a new scope for variables
	tc.withScope(func() {
		// Type check the initializers, if present
		for _, init := range f.Init {
			init.Accept(tc)
		}

		// Type check the condition
		condType, _ := tc.visitNode(f.Cond)
		if condType == nil || condType.Kind != ast.TypeBool {
			f.Location().Errorf("for condition must be bool, got %s", condType)
		}

		// Type check the body
		if f.Body != nil {
			f.Body.Accept(tc)
		}

		// Type check the post-conditions, if present
		for _, post := range f.Post {
			post.Accept(tc)
		}

		tc.lastType = &ast.Type{Kind: ast.TypeVoid} // for is a statement, not an expression
	})
}

// VisitDeref handles pointer dereference expressions (currently a no-op).
func (tc *TypeChecker) VisitDeref(d *ast.Deref) {
	// Dereference does not change the type, just returns the type of the dereferenced expression
	ref, _ := tc.visitNode(d.Expr)
	if ref == nil || ref.Kind != ast.TypePointer {
		d.Location().Errorf("dereference requires pointer type, got %s", ref)
		d.Type = &ast.Type{Kind: ast.TypeUnknown}
	} else {
		d.Type = ref.Elem // Dereference returns the element type
	}

	tc.lastType = d.Type
}

// VisitArrayIndex handles array index expressions.
func (tc *TypeChecker) VisitArrayIndex(a *ast.ArrayIndex) {
	// Typecheck the array expression
	arrayType, _ := tc.visitNode(a.Array)
	indexType, _ := tc.visitNode(a.Index)

	if arrayType == nil || arrayType.Kind != ast.TypeArray {
		a.Location().Errorf("cannot index non-array type %s", arrayType)
		a.Type = &ast.Type{Kind: ast.TypeUnknown}
		tc.lastType = a.Type
		return
	}

	if indexType == nil || indexType.Kind != ast.TypeInt {
		a.Location().Errorf("array index must be int, got %s", indexType)
	}

	a.Type = arrayType.Elem
	tc.lastType = a.Type
}

// visitNode is a helper method to visit a node and return the lastType.
func (tc *TypeChecker) visitNode(node interface{ Accept(visitor ast.Visitor) }) (*ast.Type, *Symbol) {
	if node != nil {
		node.Accept(tc)
	} else {
		tc.lastType = &ast.Type{Kind: ast.TypeUnknown}
	}

	return tc.lastType, tc.lastSymbol
}

// typeEqual returns true if two types are structurally equal (including pointer depth)
func (tc *TypeChecker) typeEqual(a, b *ast.Type) bool {
	if a == nil || b == nil {
		return a == b
	}
	if a.Kind != b.Kind {
		return false
	}
	if a.Kind == ast.TypePointer {
		return tc.typeEqual(a.Elem, b.Elem)
	}
	return true
}
