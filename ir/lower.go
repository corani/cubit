// nextIdent generates a unique identifier with the given prefix (e.g., "tmp" or "str").
package ir

import (
	"fmt"

	"github.com/corani/refactored-giggle/ast"
)

func Lower(unit *ast.CompilationUnit) (*CompilationUnit, error) {
	visitor := newVisitor()

	unit.Accept(visitor)

	return visitor.unit, nil
}

// visitor implements ast.Visitor and produces IR nodes.
type visitor struct {
	unit             *CompilationUnit
	lastVal          *Val          // holds the result of lowering the last value (for expressions)
	lastParam        *Param        // holds the result of lowering the last parameter
	lastInstructions []Instruction // holds the result of lowering a body
	tmpCounter       int           // for unique temp and string literal names
}

func newVisitor() *visitor {
	return &visitor{
		unit: NewCompilationUnit(),
	}
}

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

// TODO(daniel): TypeDef lowering is not implemented yet.
func (v *visitor) VisitTypeDef(td *ast.TypeDef) {}

// TODO(daniel): DataDef lowering is not implemented yet.
func (v *visitor) VisitDataDef(dd *ast.DataDef) {}

func (v *visitor) VisitFuncDef(fd *ast.FuncDef) {
	// Lower parameters using VisitFuncParam
	var params []*Param

	for _, param := range fd.Params {
		v.lastParam = nil
		param.Accept(v)
		if v.lastParam != nil {
			params = append(params, v.lastParam)
		}
	}

	irFunc := NewFuncDef(Ident(fd.Ident), params...)

	if v, ok := fd.Attributes[ast.AttrKeyLinkname]; ok {
		if v.Type() != ast.AttrStringType {
			panic("link_name attribute must be a string")
		}

		irFunc.LinkName = Ident(string(v.(ast.AttrString)))
	}

	if fd.ReturnType != ast.TypeVoid {
		irFunc = irFunc.WithRetTy(v.mapTypeToAbiTy(fd.ReturnType))
	}

	// Set linkage to export if the function has the export attribute
	if _, ok := fd.Attributes[ast.AttrKeyExport]; ok {
		irFunc = irFunc.WithLinkage(NewLinkageExport())
	}

	// Lower function body (blocks)
	if fd.Body != nil {
		fd.Body.Accept(v)

		irFunc = irFunc.WithBlocks(Block{
			Label:        "start",
			Instructions: v.lastInstructions,
		})
	}

	v.unit.FuncDefs = append(v.unit.FuncDefs, irFunc)
}

func (v *visitor) VisitFuncParam(fp *ast.FuncParam) {
	v.lastParam = NewParamRegular(v.mapTypeToAbiTy(fp.Type), Ident(fp.Ident))
}

func (v *visitor) VisitBody(b *ast.Body) {
	v.lastInstructions = nil

	for _, instr := range b.Instructions {
		instr.Accept(v)
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

	// For assignment, use Binop with add as a stand-in for move
	zero := NewValInteger(0)
	binopInstr := NewBinop(BinOpAdd, lhsVal, val, zero)
	v.lastInstructions = append(v.lastInstructions, binopInstr)
}

func (v *visitor) VisitCall(c *ast.Call) {
	// Lower the callee (function name)
	ident := Ident(c.Ident)

	for _, fd := range v.unit.FuncDefs {
		if fd.Ident == ident && fd.LinkName != "" {
			// If the function has a link name, use that instead
			ident = fd.LinkName
			break
		}
	}

	calleeVal := NewValGlobal(ident)

	// Lower arguments
	var args []Arg

	for _, arg := range c.Args {
		v.lastVal = nil
		arg.Value.Accept(v)
		args = append(args, NewArgRegular(v.mapTypeToAbiTy(arg.Type), v.lastVal))
	}

	// Create a temporary for the return value
	retVal := NewValIdent(v.nextIdent("tmp"))

	// Emit the Call instruction
	call := NewCall(calleeVal, args...)

	if c.Type != ast.TypeVoid {
		call.WithRet(retVal.Ident, v.mapTypeToAbiTy(c.Type))
	}

	v.lastInstructions = append(v.lastInstructions, call)
	v.lastVal = retVal
}

func (v *visitor) VisitReturn(r *ast.Return) {
	if r.Value == nil {
		v.lastInstructions = append(v.lastInstructions, NewRet())
	} else {
		v.lastVal = nil
		r.Value.Accept(v)
		val := v.lastVal

		v.lastInstructions = append(v.lastInstructions, NewRet(val))
	}
}

func (v *visitor) VisitLiteral(l *ast.Literal) {
	switch l.Type {
	case ast.TypeInt:
		v.lastVal = NewValInteger(int64(l.IntValue))
	case ast.TypeBool:
		if l.BoolValue {
			v.lastVal = NewValInteger(1)
		} else {
			v.lastVal = NewValInteger(0)
		}
	case ast.TypeString:
		// TODO(daniel): This does not deduplicate identical string literals. Consider interning/deduplicating.
		ident := v.nextIdent("str")
		v.unit.DataDefs = append(v.unit.DataDefs, NewDataDefStringZ(ident, l.StringValue))
		v.lastVal = NewValGlobal(ident)
	default:
		panic("unsupported literal type: " + l.Type.String())
	}
}

func (v *visitor) VisitBinop(b *ast.Binop) {
	// Lower left and right operands
	v.lastVal = nil
	b.Lhs.Accept(v)
	left := v.lastVal
	v.lastVal = nil
	b.Rhs.Accept(v)
	right := v.lastVal

	// Create a new temporary for the result
	result := NewValIdent(v.nextIdent("tmp"))

	// Map ast.BinOpKind to ir.BinOpKind
	var irOp BinOpKind

	switch b.Operation {
	case ast.BinOpAdd:
		irOp = BinOpAdd
	case ast.BinOpSub:
		irOp = BinOpSub
	case ast.BinOpMul:
		irOp = BinOpMul
	case ast.BinOpDiv:
		irOp = BinOpDiv
	case ast.BinOpEq:
		irOp = BinOpEq
	default:
		panic("unsupported binary operation: " + b.Operation)
	}

	v.lastInstructions = append(v.lastInstructions, NewBinop(irOp, result, left, right))
	v.lastVal = result
}

func (v *visitor) VisitVariableRef(vr *ast.VariableRef) {
	// Lower a variable reference to an identifier value
	v.lastVal = NewValIdent(Ident(vr.Ident))
}

// nextIdent generates a unique identifier with the given prefix (e.g., "tmp" or "str").
func (v *visitor) nextIdent(prefix string) Ident {
	v.tmpCounter++

	return Ident(fmt.Sprintf("_%s_%04d", prefix, v.tmpCounter))
}

// mapTypeToAbiTy maps an ast.TypeKind to the appropriate AbiTy for IR lowering.
func (v *visitor) mapTypeToAbiTy(ty ast.TypeKind) AbiTy {
	switch ty {
	case ast.TypeInt:
		return NewAbiTyBase(BaseWord)
	case ast.TypeString:
		return NewAbiTyBase(BaseLong)
	default:
		return NewAbiTyBase(BaseWord) // fallback
	}
}
