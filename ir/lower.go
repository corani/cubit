// nextIdent generates a unique identifier with the given prefix (e.g., "tmp" or "str").
package ir

import (
	"fmt"

	"github.com/corani/refactored-giggle/ast"
)

func Lower(unit *ast.CompilationUnit) (*CompilationUnit, error) {
	visitor := newVisitor()
	unit.Accept(visitor)
	return visitor.IRUnit, nil
}

// visitor implements ast.Visitor and produces IR nodes.
type visitor struct {
	IRUnit           *CompilationUnit
	lastVal          *Val          // holds the result of lowering the last value (for expressions)
	lastParam        *Param        // holds the result of lowering the last parameter
	lastInstructions []Instruction // holds the result of lowering a body
	tmpCounter       int           // for unique temp and string literal names
}

func newVisitor() *visitor {
	return &visitor{
		IRUnit: &CompilationUnit{},
	}
}

// nextIdent generates a unique identifier with the given prefix (e.g., "tmp" or "str").
func (v *visitor) nextIdent(prefix string) Ident {
	v.tmpCounter++
	return Ident(fmt.Sprintf("_%s_%04d", prefix, v.tmpCounter))
}

// Ensure a blank line separates the newVisitor function and the visitor struct definition

func (v *visitor) VisitCompilationUnit(cu *ast.CompilationUnit) {
	// Lower types
	for i := range cu.Types {
		cu.Types[i].Accept(v)
	}
	// Lower data
	for i := range cu.Data {
		cu.Data[i].Accept(v)
	}
	// Lower functions
	for i := range cu.Funcs {
		cu.Funcs[i].Accept(v)
	}
}

func (v *visitor) VisitTypeDef(td *ast.TypeDef) {}
func (v *visitor) VisitDataDef(dd *ast.DataDef) {}
func (v *visitor) VisitFuncDef(fd *ast.FuncDef) {
	// Skip functions with the extern attribute
	if _, ok := fd.Attributes[ast.AttrKeyExtern]; ok {
		return
	}
	// Lower parameters using VisitFuncParam
	var params []Param
	for i := range fd.Params {
		v.lastParam = nil
		fd.Params[i].Accept(v)
		if v.lastParam != nil {
			params = append(params, *v.lastParam)
		}
	}

	// Lower function body (blocks)
	var blocks []Block
	if fd.Body != nil {
		v.lastInstructions = nil
		fd.Body.Accept(v)
		block := Block{
			Label:        "start",
			Instructions: v.lastInstructions,
			Locals:       map[string]ast.TypeKind{},
		}
		blocks = append(blocks, block)
	}

	irFunc := NewFuncDef(Ident(fd.Ident), params...)
	// Set return type
	var retTy AbiTy
	switch fd.ReturnType {
	case ast.TypeInt:
		retTy = NewAbiTyBase(BaseWord)
	case ast.TypeString:
		retTy = NewAbiTyBase(BaseLong)
	case ast.TypeVoid:
		// No return type needed for void
	default:
		// Unknown type, leave as zero value
	}
	if fd.ReturnType != ast.TypeVoid {
		irFunc = irFunc.WithRetTy(retTy)
	}
	// Set linkage to export if the function has the export attribute
	if _, ok := fd.Attributes[ast.AttrKeyExport]; ok {
		irFunc = irFunc.WithLinkage(NewLinkageExport())
	}
	irFunc = irFunc.WithBlocks(blocks...)
	v.IRUnit.FuncDefs = append(v.IRUnit.FuncDefs, irFunc)
}

func (v *visitor) VisitFuncParam(fp *ast.FuncParam) {
	var abiTy AbiTy
	switch fp.Type {
	case ast.TypeInt:
		abiTy = NewAbiTyBase(BaseWord)
	case ast.TypeString:
		abiTy = NewAbiTyBase(BaseLong)
	default:
		abiTy = NewAbiTyBase(BaseWord) // fallback
	}
	param := NewParamRegular(abiTy, Ident(fp.Ident))
	v.lastParam = &param
}
func (v *visitor) VisitBody(b *ast.Body) {
	v.lastInstructions = nil
	for _, instr := range b.Instructions {
		// Lower the instruction
		instr.Accept(v)
		// If lowering a binop or other expression, it may have appended to lastInstructions
		// If lowering an assignment, it will append its own instruction
		// So we just need to ensure all instructions are accumulated
		// (No need to do anything here, as each lowering appends to v.lastInstructions)
	}
}
func (v *visitor) VisitAssign(a *ast.Assign) {
	// Lower the right-hand side expression
	v.lastVal = nil
	a.Value.Accept(v)
	val := v.lastVal

	// Left-hand side variable
	lhsIdent := Ident(a.Ident)
	lhsVal := NewValIdent(lhsIdent)

	// For assignment, use Add(lhs, rhs, 0) as a stand-in for move
	zero := NewValInteger(0)
	addInstr := NewAdd(lhsVal, *val, zero)
	v.lastInstructions = append(v.lastInstructions, addInstr)
}

func (v *visitor) VisitCall(c *ast.Call) {
	// Lower the callee (function name)
	calleeVal := NewValGlobal(Ident(c.Ident))

	// Lower arguments
	var args []Arg
	for _, arg := range c.Args {
		v.lastVal = nil
		arg.Value.Accept(v)
		argVal := *v.lastVal
		abiTy := NewAbiTyBase(BaseWord) // TODO: map type properly
		args = append(args, NewArgRegular(abiTy, argVal))
	}

	// Create a temporary for the return value
	tmp := v.nextIdent("tmp")
	retVal := NewValIdent(tmp)

	// Emit the Call instruction
	callInstr := NewCall(calleeVal, args...)
	callWithRet := callInstr.WithRet(tmp, NewAbiTyBase(BaseWord)) // TODO: map return type
	v.lastInstructions = append(v.lastInstructions, &callWithRet)

	v.lastVal = &retVal
}

func (v *visitor) VisitReturn(r *ast.Return) {
	if r.Value == nil {
		v.lastInstructions = append(v.lastInstructions, NewRet())
	} else {
		v.lastVal = nil
		r.Value.Accept(v)
		val := v.lastVal

		v.lastInstructions = append(v.lastInstructions, NewRet(*val))
	}
}
func (v *visitor) VisitLiteral(l *ast.Literal) {
	switch l.Type {
	case ast.TypeInt:
		v_ := NewValInteger(int64(l.IntValue))
		v.lastVal = &v_
	case ast.TypeString:
		// Generate a unique identifier for the string literal
		ident := v.nextIdent("str")
		v.IRUnit.DataDefs = append(v.IRUnit.DataDefs, NewDataDefStringZ(ident, l.StringValue))
		v_ := NewValGlobal(ident)
		v.lastVal = &v_
	default:
		v.lastVal = nil
	}
}
func (v *visitor) VisitBinop(b *ast.Binop) {
	// Only plus is supported
	if b.Operation != "+" {
		v.lastVal = nil
		return
	}

	// Lower left and right operands
	v.lastVal = nil
	b.Lhs.Accept(v)
	left := v.lastVal
	v.lastVal = nil
	b.Rhs.Accept(v)
	right := v.lastVal

	// Create a new temporary for the result
	tmp := v.nextIdent("tmp")
	result := NewValIdent(tmp)

	// Emit the Add instruction
	addInstr := NewAdd(result, *left, *right)
	v.lastInstructions = append(v.lastInstructions, addInstr)

	v.lastVal = &result
}

func (v *visitor) VisitVariableRef(vr *ast.VariableRef) {
	// Lower a variable reference to an identifier value
	v_ := NewValIdent(Ident(vr.Ident))
	v.lastVal = &v_
}
