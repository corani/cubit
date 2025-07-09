package analyzer

import (
	"fmt"

	"github.com/corani/cubit/ast"
	"github.com/corani/cubit/lexer"
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
		for _, err := range tc.errors {
			fmt.Println("Type error:", err)
		}

		return tc.errors[0] // Return the first error for now
	}

	return nil
}

func (tc *TypeChecker) VisitCompilationUnit(unit *ast.CompilationUnit) {
	// Push global scope
	tc.pushScope()

	// Add all function definitions to the global scope first
	for _, fn := range unit.Funcs {
		tc.addSymbol(&Symbol{
			Name:    fn.Ident,
			Type:    fn.ReturnType,
			IsFunc:  true,
			FuncDef: fn,
		})
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
	// Enter a new scope for the function
	tc.pushScope()

	// Add parameters to the new scope
	for i := range fn.Params {
		param := fn.Params[i]

		// Visit first to allow type inference/checking
		param.Accept(tc)

		tc.addSymbol(&Symbol{
			Name:   param.Ident,
			Type:   param.Type,
			IsFunc: false,
		})
	}

	// Type check the function body (if present)
	if fn.Body != nil {
		fn.Body.Accept(tc)
	}

	// Leave the function scope
	tc.popScope()
}

func (tc *TypeChecker) VisitFuncParam(fn *ast.FuncParam) {
	if fn.Value != nil {
		valueType := tc.visitNode(fn.Value)

		if fn.Type != nil && fn.Type.Kind == ast.TypeUnknown {
			// Case 3: arg := 1 (infer type from value)
			fn.Type = valueType
		} else {
			// Case 2: arg : int = 1 (check match)
			if !tc.typeEqual(valueType, fn.Type) {
				tc.errorf(fn.Location(), "type error: parameter '%s' declared as %s but default value is %s",
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

// VisitDeclare handles variable declarations (no IR emitted, but needed for typechecking and lowering).
func (tc *TypeChecker) VisitDeclare(d *ast.Declare) {
	// Add the declared variable to the current scope with its type (may be TypeUnknown)
	tc.addSymbol(&Symbol{
		Name:        d.Ident,
		Type:        d.Type,
		IsFunc:      false,
		Declaration: d,
	})

	// No type to propagate
}

// VisitAssign handles assignment to lvalues.
func (tc *TypeChecker) VisitAssign(a *ast.Assign) {
	// Typecheck the lvalue (sets tc.lastType and tc.lastSymbol)
	tc.lastSymbol = nil
	a.LHS.Accept(tc)
	lvalType := tc.lastType
	lvalSymbol := tc.lastSymbol

	// Typecheck the value
	valType := tc.visitNode(a.Value)

	// If the lvalue is a variable, lastSymbol will be set
	if lvalSymbol != nil {
		if lvalSymbol.Type.Kind == ast.TypeUnknown {
			if err := lvalSymbol.UpdateType(valType); err != nil {
				tc.errorf(a.Location(), "type error: %s", err)
			}

			// If LHS is a variable, we can set its type now
			switch lvalue := a.LHS.(type) {
			case *ast.VariableRef:
				lvalue.Type = lvalSymbol.Type
			}
		} else if !tc.typeEqual(lvalType, valType) {
			tc.errorf(a.Location(), "type error: variable '%s' declared as %s but assigned %s", lvalSymbol.Name, lvalSymbol.Type, valType)
		}
	} else {
		// TODO: handle pointer deref, array index, etc.
		if lvalType != nil && lvalType.Kind != ast.TypeUnknown && !tc.typeEqual(lvalType, valType) {
			tc.errorf(a.Location(), "type error: lvalue type %s but assigned %s", lvalType, valType)
		}
	}

	a.Type = valType
	tc.lastType = valType
}

func (tc *TypeChecker) VisitCall(call *ast.Call) {
	// Look up the function definition
	sym, ok := tc.lookupSymbol(call.Ident)
	if !ok || !sym.IsFunc || sym.FuncDef == nil {
		tc.errorf(call.Location(), "call to undefined function '%s'", call.Ident)
		tc.lastType = &ast.Type{Kind: ast.TypeUnknown}

		return
	}

	fnDef := sym.FuncDef

	// Check argument count
	if len(call.Args) != len(fnDef.Params) {
		tc.errorf(call.Location(), "call to '%s' expects %d arguments, got %d", call.Ident, len(fnDef.Params), len(call.Args))
		tc.lastType = sym.Type

		return
	}

	// Check argument types
	for i, arg := range call.Args {
		argType := tc.visitNode(arg.Value)
		paramType := fnDef.Params[i].Type

		call.Args[i].Type = argType // Set the type of the argument

		if paramType != nil && paramType.Kind != ast.TypeUnknown && !tc.typeEqual(argType, paramType) {
			tc.errorf(arg.Location(), "call to '%s': argument %d type mismatch: expected %s, got %s", call.Ident, i+1, paramType, argType)
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
		retType = tc.visitNode(ret.Value)
	}

	tc.lastType = retType
}

func (tc *TypeChecker) VisitLiteral(lit *ast.Literal) {
	// Literals already have their type set
	tc.lastType = lit.Type
}

func (tc *TypeChecker) VisitVariableRef(ref *ast.VariableRef) {
	// Look up the variable in the current scope stack
	if sym, ok := tc.lookupSymbol(ref.Ident); ok && !sym.IsFunc {
		ref.Type = sym.Type
		tc.lastType = sym.Type
		tc.lastSymbol = sym
	} else {
		tc.errorf(ref.Location(), "undefined variable '%s'", ref.Ident)
		ref.Type = &ast.Type{Kind: ast.TypeUnknown}
		tc.lastType = ref.Type
		tc.lastSymbol = nil
	}
}

func (tc *TypeChecker) VisitBinop(binop *ast.Binop) {
	lhsType := tc.visitNode(binop.Lhs)
	rhsType := tc.visitNode(binop.Rhs)

	switch binop.Operation {
	case ast.BinOpAdd, ast.BinOpSub:
		// Pointer arithmetic support
		if lhsType != nil && rhsType != nil {
			if lhsType.Kind == ast.TypePointer && rhsType.Kind == ast.TypeInt {
				// pointer + int or pointer - int => pointer
				binop.Type = lhsType
			} else if lhsType.Kind == ast.TypeInt && rhsType.Kind == ast.TypePointer && binop.Operation == ast.BinOpAdd {
				// int + pointer => pointer (only for add)
				binop.Type = rhsType
			} else if lhsType.Kind == ast.TypePointer && rhsType.Kind == ast.TypePointer && binop.Operation == ast.BinOpSub {
				// pointer - pointer => int (if element types match)
				if tc.typeEqual(lhsType, rhsType) {
					binop.Type = &ast.Type{Kind: ast.TypeInt}
				} else {
					binop.Type = &ast.Type{Kind: ast.TypeUnknown}
					tc.errorf(binop.Location(), "pointer subtraction requires matching pointer types, got %s - %s", lhsType, rhsType)
				}
			} else if tc.typeEqual(lhsType, rhsType) {
				// fallback: both operands same type
				binop.Type = lhsType
			} else {
				binop.Type = &ast.Type{Kind: ast.TypeUnknown}
				tc.errorf(binop.Location(), "invalid operands for pointer arithmetic: %s %s %s", lhsType, binop.Operation, rhsType)
			}
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "invalid operands for pointer arithmetic: %s %s %s", lhsType, binop.Operation, rhsType)
		}
	case ast.BinOpEq, ast.BinOpNe:
		// Equality/inequality returns bool if types match
		if lhsType != nil && rhsType != nil && tc.typeEqual(lhsType, rhsType) {
			binop.Type = &ast.Type{Kind: ast.TypeBool}
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "type mismatch in equality/inequality operation: %s vs %s", lhsType, rhsType)
		}
	case ast.BinOpLt, ast.BinOpLe, ast.BinOpGt, ast.BinOpGe:
		// Comparison operators: valid for int, string, or pointer (if types match)
		if lhsType != nil && rhsType != nil && tc.typeEqual(lhsType, rhsType) &&
			(lhsType.Kind == ast.TypeInt || lhsType.Kind == ast.TypeString || lhsType.Kind == ast.TypePointer) {
			binop.Type = &ast.Type{Kind: ast.TypeBool}
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "type mismatch or invalid types in comparison operation: %s vs %s", lhsType, rhsType)
		}
	case ast.BinOpShl, ast.BinOpShr:
		// Shift ops: both sides must be int, result is int
		if lhsType != nil && rhsType != nil && lhsType.Kind == ast.TypeInt && rhsType.Kind == ast.TypeInt {
			binop.Type = &ast.Type{Kind: ast.TypeInt}
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "shift operation requires int operands, got %s << %s", lhsType, rhsType)
		}
	case ast.BinOpAnd, ast.BinOpOr:
		// Bitwise ops: both sides must be int, result is int
		if lhsType != nil && rhsType != nil && lhsType.Kind == ast.TypeInt && rhsType.Kind == ast.TypeInt {
			binop.Type = &ast.Type{Kind: ast.TypeInt}
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "bitwise operation requires int operands, got %s & %s", lhsType, rhsType)
		}
	case ast.BinOpLogAnd, ast.BinOpLogOr:
		// Logical ops: both sides must be bool, result is bool
		if lhsType != nil && rhsType != nil && lhsType.Kind == ast.TypeBool && rhsType.Kind == ast.TypeBool {
			binop.Type = &ast.Type{Kind: ast.TypeBool}
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "logical operation requires bool operands, got %s && %s", lhsType, rhsType)
		}
	default:
		if lhsType != nil && rhsType != nil && tc.typeEqual(lhsType, rhsType) {
			binop.Type = lhsType
		} else {
			binop.Type = &ast.Type{Kind: ast.TypeUnknown}
			tc.errorf(binop.Location(), "type mismatch in binary operation: %s vs %s", lhsType, rhsType)
		}
	}

	tc.lastType = binop.Type
}

func (tc *TypeChecker) VisitIf(iff *ast.If) {
	// If statements introduce a new scope for variables (e.g. initializer)
	tc.pushScope()

	// Type check the initializers, if present
	for _, init := range iff.Init {
		init.Accept(tc)
	}

	// Type check the condition
	condType := tc.visitNode(iff.Cond)
	if condType == nil || condType.Kind != ast.TypeBool {
		tc.errorf(iff.Location(), "if condition must be bool, got %s", condType)
	}

	// Type check the 'then' branch
	iff.Then.Accept(tc)

	// Type check the 'else' branch, if present
	if iff.Else != nil {
		iff.Else.Accept(tc)
	}

	tc.popScope()
	tc.lastType = &ast.Type{Kind: ast.TypeVoid} // if is a statement, not an expression
}

func (tc *TypeChecker) VisitFor(f *ast.For) {
	// For statements introduce a new scope for variables
	tc.pushScope()

	// Type check the initializers, if present
	for _, init := range f.Init {
		init.Accept(tc)
	}

	// Type check the condition
	condType := tc.visitNode(f.Cond)
	if condType == nil || condType.Kind != ast.TypeBool {
		tc.errorf(f.Location(), "for condition must be bool, got %s", condType)
	}

	// Type check the body
	if f.Body != nil {
		f.Body.Accept(tc)
	}

	// Type check the post-conditions, if present
	for _, post := range f.Post {
		post.Accept(tc)
	}

	tc.popScope()
	tc.lastType = &ast.Type{Kind: ast.TypeVoid} // for is a statement, not an expression
}

// VisitDeref handles pointer dereference expressions (currently a no-op).
func (tc *TypeChecker) VisitDeref(d *ast.Deref) {
	// Dereference does not change the type, just returns the type of the dereferenced expression
	ref := tc.visitNode(d.Expr)
	if ref == nil || ref.Kind != ast.TypePointer {
		tc.errorf(d.Location(), "dereference requires pointer type, got %s", ref)
		d.Type = &ast.Type{Kind: ast.TypeUnknown}
	} else {
		d.Type = ref.Elem // Dereference returns the element type
	}

	tc.lastType = d.Type
}

// visitNode is a helper method to visit a node and return the lastType.
func (tc *TypeChecker) visitNode(node interface{ Accept(visitor ast.Visitor) }) *ast.Type {
	if node != nil {
		node.Accept(tc)
	} else {
		tc.lastType = &ast.Type{Kind: ast.TypeUnknown}
	}

	return tc.lastType
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

// Helper to record errors
func (tc *TypeChecker) errorf(location lexer.Location, format string, args ...any) {
	fmt.Printf("%s: [ERRO] "+format+"\n", append([]any{location}, args...)...)

	err := fmt.Errorf("%s: "+format, append([]any{location}, args...)...)
	tc.errors = append(tc.errors, err)
}

func (tc *TypeChecker) infof(location lexer.Location, format string, args ...any) {
	fmt.Printf("%s: [INFO] "+format+"\n", append([]any{location}, args...)...)
}
