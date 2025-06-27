package analyzer

import (
	"fmt"

	"github.com/corani/refactored-giggle/ast"
)

// Symbol represents a variable or function in the symbol table.
type Symbol struct {
	Name    string
	Type    ast.TypeKind
	IsFunc  bool
	FuncDef *ast.FuncDef // Only set if IsFunc
}

// TypeChecker implements a visitor for type checking the AST.
type TypeChecker struct {
	scopes   []map[string]Symbol
	errors   []error
	lastType ast.TypeKind
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
		tc.addSymbol(Symbol{
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

		tc.addSymbol(Symbol{
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

		if fn.Type == ast.TypeUnknown {
			// Case 3: arg := 1 (infer type from value)
			fn.Type = valueType
		} else {
			// Case 2: arg : int = 1 (check match)
			if valueType != fn.Type {
				tc.errorf("type error: parameter '%s' declared as %s but default value is %s",
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

func (tc *TypeChecker) VisitAssign(assign *ast.Assign) {
	// Type check the right-hand side expression
	rhsType := tc.visitNode(assign.Value)

	// If the assignment declares a type, check it matches the value
	if assign.Type != ast.TypeUnknown && assign.Type != rhsType {
		tc.errorf("type error: variable '%s' declared as %s but assigned %s", assign.Ident, assign.Type, rhsType)
	}

	// Add or update the variable in the current scope
	tc.addSymbol(Symbol{
		Name:   assign.Ident,
		Type:   rhsType,
		IsFunc: false,
	})

	assign.Type = rhsType // Set the type of the assignment
	tc.lastType = assign.Type
}

func (tc *TypeChecker) VisitSet(set *ast.Set) {
	// Look up the variable in the current scope
	sym, ok := tc.lookupSymbol(set.Ident)
	if !ok {
		tc.errorf("undefined variable '%s'", set.Ident)

		set.Type = ast.TypeUnknown
		tc.lastType = ast.TypeUnknown

		return
	} else if sym.IsFunc {
		tc.errorf("cannot assign to function '%s'", set.Ident)

		set.Type = ast.TypeUnknown
		tc.lastType = ast.TypeUnknown

		return
	}

	// Type check the value being assigned
	valueType := tc.visitNode(set.Value)

	// If the variable has a declared type, check it matches the value
	if sym.Type != valueType {
		tc.errorf("type error: variable '%s' declared as %s but assigned %s",
			set.Ident, sym.Type, valueType)
	}

	set.Type = valueType // Set the type of the set operation
	tc.lastType = set.Type
}

func (tc *TypeChecker) VisitCall(call *ast.Call) {
	// Look up the function definition
	sym, ok := tc.lookupSymbol(call.Ident)
	if !ok || !sym.IsFunc || sym.FuncDef == nil {
		tc.errorf("call to undefined function '%s'", call.Ident)
		tc.lastType = ast.TypeUnknown

		return
	}

	fnDef := sym.FuncDef

	// Check argument count
	if len(call.Args) != len(fnDef.Params) {
		tc.errorf("call to '%s' expects %d arguments, got %d", call.Ident, len(fnDef.Params), len(call.Args))
		tc.lastType = sym.Type

		return
	}

	// Check argument types
	for i, arg := range call.Args {
		argType := tc.visitNode(arg.Value)
		paramType := fnDef.Params[i].Type

		if paramType != ast.TypeUnknown && argType != paramType {
			tc.errorf("call to '%s': argument %d type mismatch: expected %s, got %s", call.Ident, i+1, paramType, argType)
		}
	}

	// Set the type of the call to the function's return type
	call.Type = fnDef.ReturnType
	tc.lastType = call.Type
}

func (tc *TypeChecker) VisitReturn(ret *ast.Return) {
	// Type check the return value (if any)
	retType := ast.TypeVoid

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
	} else {
		tc.errorf("undefined variable '%s'", ref.Ident)
		ref.Type = ast.TypeUnknown
		tc.lastType = ast.TypeUnknown
	}
}

func (tc *TypeChecker) VisitBinop(binop *ast.Binop) {
	lhsType := tc.visitNode(binop.Lhs)
	rhsType := tc.visitNode(binop.Rhs)

	switch binop.Operation {
	case ast.BinOpEq, ast.BinOpNe:
		// Equality/inequality returns bool if types match
		if lhsType == rhsType {
			binop.Type = ast.TypeBool
		} else {
			binop.Type = ast.TypeUnknown
			tc.errorf("type mismatch in equality/inequality operation: %s vs %s", lhsType, rhsType)
		}
	case ast.BinOpLt, ast.BinOpLe, ast.BinOpGt, ast.BinOpGe:
		// Comparison operators: only valid for int or string, and types must match
		if lhsType == rhsType && (lhsType == ast.TypeInt || lhsType == ast.TypeString) {
			binop.Type = ast.TypeBool
		} else {
			binop.Type = ast.TypeUnknown
			tc.errorf("type mismatch or invalid types in comparison operation: %s vs %s", lhsType, rhsType)
		}
	default:
		if lhsType == rhsType {
			binop.Type = lhsType
		} else {
			binop.Type = ast.TypeUnknown
			tc.errorf("type mismatch in binary operation: %s vs %s", lhsType, rhsType)
		}
	}

	tc.lastType = binop.Type
}

func (tc *TypeChecker) VisitIf(iff *ast.If) {
	// If statements introduce a new scope for variables (e.g. initializer)
	tc.pushScope()

	// Type check the initializer, if present
	if iff.Init != nil {
		iff.Init.Accept(tc)
	}

	// Type check the condition
	condType := tc.visitNode(iff.Cond)
	if condType != ast.TypeBool {
		tc.errorf("if condition must be bool, got %s", condType)
	}

	// Type check the 'then' branch
	iff.Then.Accept(tc)

	// Type check the 'else' branch, if present
	if iff.Else != nil {
		iff.Else.Accept(tc)
	}

	tc.popScope()
	tc.lastType = ast.TypeVoid // if is a statement, not an expression
}

func (tc *TypeChecker) VisitFor(f *ast.For) {
	// For statements introduce a new scope for variables
	tc.pushScope()

	// Type check the initializer, if present
	if f.Init != nil {
		f.Init.Accept(tc)
	}

	// Type check the condition
	condType := tc.visitNode(f.Cond)
	if condType != ast.TypeBool {
		tc.errorf("for condition must be bool, got %s", condType)
	}

	// Type check the body
	if f.Body != nil {
		f.Body.Accept(tc)
	}

	// Type check the post-condition, if present
	if f.Post != nil {
		f.Post.Accept(tc)
	}

	tc.popScope()
	tc.lastType = ast.TypeVoid // for is a statement, not an expression
}

// visitNode is a helper method to visit a node and return the lastType.
func (tc *TypeChecker) visitNode(node interface{ Accept(visitor ast.Visitor) }) ast.TypeKind {
	if node != nil {
		node.Accept(tc)
	} else {
		tc.lastType = ast.TypeUnknown
	}

	return tc.lastType
}

// Scope management helpers
func (tc *TypeChecker) pushScope() {
	tc.scopes = append(tc.scopes, make(map[string]Symbol))
}

func (tc *TypeChecker) popScope() {
	if len(tc.scopes) > 0 {
		tc.scopes = tc.scopes[:len(tc.scopes)-1]
	}
}

func (tc *TypeChecker) addSymbol(sym Symbol) {
	if len(tc.scopes) == 0 {
		tc.pushScope()
	}
	tc.scopes[len(tc.scopes)-1][sym.Name] = sym
}

func (tc *TypeChecker) lookupSymbol(name string) (Symbol, bool) {
	for i := len(tc.scopes) - 1; i >= 0; i-- {
		if sym, ok := tc.scopes[i][name]; ok {
			return sym, true
		}
	}
	return Symbol{}, false
}

// Helper to record errors
func (tc *TypeChecker) errorf(format string, args ...any) {
	err := fmt.Errorf(format, args...)
	tc.errors = append(tc.errors, err)
}
