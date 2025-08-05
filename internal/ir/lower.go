package ir

import (
	"fmt"

	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
)

func Lower(unit *ast.CompilationUnit) (*CompilationUnit, error) {
	visitor := newVisitor()

	unit.Accept(visitor)

	return visitor.unit, nil
}

type visitor struct {
	unit             *CompilationUnit
	lastVal          *Val          // holds the result of lowering the last value (for expressions)
	lastType         *ast.Type     // holds the type of the last value (for expressions)
	lastParam        *Param        // holds the result of lowering the last parameter
	lastInstructions []Instruction // holds the result of lowering a body
	tmpCounter       int           // for unique temp and string literal names
	labelCounter     int
	localSlots       map[string]*Val // variable/param name -> stack slot (function-local)
	lvalue           bool
	lastAddress      *Val            // holds the address (slot) of the last lowered lvalue
	literalGlobals   []literalGlobal // deduplication for literals
}

func newVisitor() *visitor {
	return &visitor{
		unit:           NewCompilationUnit(),
		literalGlobals: make([]literalGlobal, 0),
	}
}

func (v *visitor) VisitCompilationUnit(cu *ast.CompilationUnit) {
	v.unit.WithPackage(cu.Ident, cu.Location())

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
	// TODO(daniel): This will fail for nested functions like lambdas!
	// Labels are function-local, so we can reset the counter for each function
	v.labelCounter = 0
	v.lastInstructions = nil

	// Lower parameters using VisitFuncParam
	var params []*Param
	v.localSlots = make(map[string]*Val) // function-local slot map

	for _, param := range fd.Params {
		v.lastParam = nil
		param.Accept(v)
		if v.lastParam != nil {
			params = append(params, v.lastParam)
		}
	}

	irFunc := NewFuncDef(lexer.Location{
		Filename: fd.Loc.Filename,
		Line:     fd.Loc.Line,
		Column:   fd.Loc.Column,
	}, Ident(fd.Ident), params...)

	if v, ok := fd.Attributes[ast.AttrKeyLinkname]; ok {
		if v.Type() != ast.AttrStringType {
			fd.Location().Errorf("link_name attribute must be a string")
		} else {
			irFunc.LinkName = Ident(string(v.(ast.AttrString)))
		}
	}

	if fd.ReturnType != nil && fd.ReturnType.Kind != ast.TypeVoid {
		irFunc = irFunc.WithRetTy(v.mapTypeToAbiTy(fd.ReturnType))
	}

	// Set linkage to export if the function has the export attribute
	if _, ok := fd.Attributes[ast.AttrKeyExport]; ok {
		irFunc = irFunc.WithLinkage(NewLinkageExport(fd.Location()))
	}

	// --- Stack-allocate all parameters at function entry ---
	var paramInitInstrs []Instruction

	for _, param := range params {
		// Create a stack slot for the parameter
		slotName := Ident(string(param.Ident) + "_slot")
		slotVal := NewValIdent(param.Loc, slotName, NewAbiTyBase(BaseLong))

		// Assume 4 bytes for int/bool, 8 for long/pointer
		var size int64 = 4
		switch param.AbiTy.BaseTy {
		case BaseLong:
			size = 8
		case BaseWord:
			size = 4
			// Add more cases as needed
		}

		sizeVal := NewValInteger(param.Loc, size, NewAbiTyBase(BaseLong))
		paramInitInstrs = append(paramInitInstrs, NewAlloc(param.Loc, slotVal, sizeVal))

		// Store the incoming parameter value into the slot
		paramVal := NewValIdent(param.Loc, param.Ident, param.AbiTy)
		paramInitInstrs = append(paramInitInstrs, NewStore(param.Loc, slotVal, paramVal))
		v.localSlots[string(param.Ident)] = slotVal
	}

	// Lower function body (blocks)
	if fd.Body != nil {
		fd.Body.Accept(v)
		// Prepend paramInitInstrs to the function's instructions
		allInstrs := append(paramInitInstrs, v.lastInstructions...)
		irFunc = irFunc.WithBlocks(
			NewBlock(fd.Body.Location(), "start", allInstrs))
	}

	v.unit.FuncDefs = append(v.unit.FuncDefs, irFunc)
}

func (v *visitor) VisitGenericParam(gp *ast.GenericParam) {
	// TODO: implementation
}

func (v *visitor) VisitFuncParam(fp *ast.FuncParam) {
	v.lastParam = NewParamRegular(fp.Location(), v.mapTypeToAbiTy(fp.Type), Ident(fp.Ident))
}

func (v *visitor) VisitBody(b *ast.Body) {
	for _, instr := range b.Instructions {
		instr.Accept(v)
	}
}

// VisitDeclare handles variable declarations (no IR emitted, but needed for IR lowering).
func (v *visitor) VisitDeclare(d *ast.Declare) {
	// Stack-allocate all locals (scalars and arrays)
	var size int64 = 4
	abiTy := v.mapTypeToAbiTy(d.Type)
	if d.Type != nil && d.Type.Kind == ast.TypeArray {
		size = 1
		tmpType := d.Type
		for tmpType != nil && tmpType.Kind == ast.TypeArray {
			// TODO: support symbolic sizes?
			if tmpType.Size.Kind != ast.SizeLiteral {
				d.Location().Errorf("array size must be a literal")
			} else {
				size *= int64(tmpType.Size.Value)
				tmpType = tmpType.Elem
			}
		}

		// Assume only int arrays for now (4 bytes per int)
		eleSize := int64(4)
		size *= eleSize
	} else if abiTy.BaseTy == BaseLong {
		size = 8
	}

	sizeVal := NewValInteger(d.Location(), size, NewAbiTyBase(BaseLong))
	slotName := Ident(string(d.Ident) + "_slot")
	slotVal := NewValIdent(d.Location(), slotName, NewAbiTyBase(BaseLong))
	v.appendInstruction(NewAlloc(d.Location(), slotVal, sizeVal))

	v.localSlots[string(d.Ident)] = slotVal
	v.lastVal = slotVal
	v.lastType = d.Type
}

// zeroInitialize emits IR to zero out a memory region [addr, addr+size)
func (v *visitor) zeroInitialize(loc lexer.Location, addr *Val, size *Val) {
	// We'll emit a simple loop:
	//   i = 0
	//   loop:
	//     if i >= size goto end
	//     storew 0, addr + i
	//     i += 4
	//     goto loop
	//   end:

	idx := NewValIdent(loc, v.nextIdent("zi_idx"), NewAbiTyBase(BaseLong))
	zero := NewValInteger(loc, 0, NewAbiTyBase(BaseWord))
	step := NewValInteger(loc, 4, NewAbiTyBase(BaseLong))

	loopLabel := v.nextLabel("zi_loop")
	endLabel := v.nextLabel("zi_end")
	falseLabel := v.nextLabel("zi_tmp")

	// i = 0
	v.appendInstruction(NewBinop(loc, BinOpAdd, idx, zero, NewValInteger(loc, 0, NewAbiTyBase(BaseLong))))
	// loop:
	v.appendInstruction(NewLabel(loc, loopLabel))
	// if i >= size goto end
	cmp := NewValIdent(loc, v.nextIdent("zi_cmp"), NewAbiTyBase(BaseWord))
	v.appendInstruction(NewBinop(loc, BinOpGe, cmp, idx, size))
	v.appendInstruction(NewJnz(loc, cmp, endLabel, falseLabel))
	v.appendInstruction(NewLabel(loc, falseLabel))
	// addr + i
	addrPlusIdx := NewValIdent(loc, v.nextIdent("zi_addr"), NewAbiTyBase(BaseLong))
	v.appendInstruction(NewBinop(loc, BinOpAdd, addrPlusIdx, addr, idx))
	// storew 0, addr + i
	v.appendInstruction(NewStore(loc, addrPlusIdx, zero))
	// i += 4
	v.appendInstruction(NewBinop(loc, BinOpAdd, idx, idx, step))
	// goto loop
	v.appendInstruction(NewJmp(loc, loopLabel))
	// end:
	v.appendInstruction(NewLabel(loc, endLabel))
}

func (v *visitor) VisitAssign(a *ast.Assign) {
	// Lower the right-hand side expression
	v.lastVal = nil
	a.Value.Accept(v)

	v.acceptLValue(a.LHS)
}

func (v *visitor) VisitCall(c *ast.Call) {
	if c.FuncDef.Attributes.Has(ast.AttrKeyBuiltin) {
		v.visitBuiltinCall(c)

		return
	}

	// Lower the callee (function name)
	ident := Ident(c.Ident)

	if v, ok := c.FuncDef.Attributes.GetString(ast.AttrKeyLinkname); ok {
		ident = Ident(v)
	}

	calleeVal := NewValGlobal(c.Location(), ident, v.mapTypeToAbiTy(c.Type))

	// Lower arguments
	var args []Arg

	for _, arg := range c.Args {
		v.lastVal = nil
		arg.Value.Accept(v)
		args = append(args, NewArgRegular(arg.Location(), v.lastVal))
	}

	// Create a temporary for the return value
	retVal := NewValIdent(c.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(c.Type))

	// Emit the Call instruction
	call := NewCall(c.Location(), calleeVal, args...)

	if c.Type != nil && c.Type.Kind != ast.TypeVoid {
		call.WithRet(retVal.Ident, v.mapTypeToAbiTy(c.Type))
	}

	v.appendInstruction(call)
	v.lastVal = retVal
	v.lastType = c.Type
}

func (v *visitor) VisitReturn(r *ast.Return) {
	if r.Value == nil {
		v.appendInstruction(NewRet(r.Location()))
	} else {
		v.lastVal = nil
		r.Value.Accept(v)
		val := v.lastVal

		v.appendInstruction(NewRet(r.Location(), val))
	}
}

func (v *visitor) VisitLiteral(l *ast.Literal) {
	if l.Type == nil {
		l.Location().Errorf("literal has nil type")

		return
	}

	switch l.Type.Kind {
	case ast.TypeInt:
		v.lastVal = NewValInteger(l.Location(), int64(l.IntValue), v.mapTypeToAbiTy(l.Type))
	case ast.TypeBool:
		if l.BoolValue {
			v.lastVal = NewValInteger(l.Location(), 1, v.mapTypeToAbiTy(l.Type))
		} else {
			v.lastVal = NewValInteger(l.Location(), 0, v.mapTypeToAbiTy(l.Type))
		}
	case ast.TypeString:
		v.lastVal = getOrCreateLiteralGlobal(v, l.Location(), ast.TypeString, l.StringValue)
	case ast.TypeArray:
		// Compute array size
		size := int64(1)
		tmpType := l.Type
		for tmpType != nil && tmpType.Kind == ast.TypeArray {
			// TODO: support symbolic sizes?
			if tmpType.Size.Kind != ast.SizeLiteral {
				l.Location().Errorf("array size must be a literal")
			} else {
				size *= int64(tmpType.Size.Value)
				tmpType = tmpType.Elem
			}
		}

		// Assume only int arrays for now (4 bytes per int)
		eleSize := int64(4)
		totalBytes := size * eleSize
		sizeVal := NewValInteger(l.Location(), totalBytes, NewAbiTyBase(BaseLong))
		retVal := NewValIdent(l.Location(), v.nextIdent("arr"), NewAbiTyBase(BaseLong))
		v.appendInstruction(NewAlloc(l.Location(), retVal, sizeVal))

		// Initialize the array elements
		switch len(l.ArrayValue) {
		case int(size):
			// Initialize each element if the size matches
			for i, elem := range l.ArrayValue {
				elem.Accept(v)
				value := v.lastVal
				offset := NewValInteger(l.Location(), int64(i)*eleSize, NewAbiTyBase(BaseLong))
				addr := NewValIdent(l.Location(), v.nextIdent("arr_elem"), NewAbiTyBase(BaseLong))
				v.appendInstruction(NewBinop(l.Location(), BinOpAdd, addr, retVal, offset))
				v.appendInstruction(NewStore(l.Location(), addr, value))
			}
		case 0:
			// Zero-initialize the array if empty
			v.zeroInitialize(l.Location(), retVal, sizeVal)
		default:
			// Size mismatch: report an error
			l.Location().Errorf("array literal size mismatch: expected %d, got %d", size, len(l.ArrayValue))

			// Recovery: zero-initialize the array
			v.zeroInitialize(l.Location(), retVal, sizeVal)
		}

		v.lastVal = retVal
	default:
		l.Location().Errorf("unsupported literal type: %s", l.Type.String())
	}

	v.lastType = l.Type
}

func (v *visitor) VisitBinop(b *ast.Binop) {
	// Lower left and right operands
	v.lastVal, v.lastType = nil, nil
	b.Lhs.Accept(v)
	left, leftType := v.lastVal, v.lastType

	// Create a new temporary for the result
	result := NewValIdent(b.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(b.Type))

	// Handle logical operations separately using compare and jump.
	switch b.Operation {
	case ast.BinOpLogAnd:
		v.visitBinOpLogAnd(left, b, result)

		v.lastVal = result
		return
	case ast.BinOpLogOr:
		v.visitBinOpLogOr(left, b, result)

		v.lastVal = result
		return
	}

	v.lastVal, v.lastType = nil, nil
	b.Rhs.Accept(v)
	right, rightType := v.lastVal, v.lastType

	// Map ast.BinOpKind to ir.BinOpKind using a map for maintainability
	binOpMap := map[ast.BinOpKind]BinOpKind{
		ast.BinOpAdd: BinOpAdd,
		ast.BinOpSub: BinOpSub,
		ast.BinOpMul: BinOpMul,
		ast.BinOpDiv: BinOpDiv,
		ast.BinOpMod: BinOpMod,
		ast.BinOpEq:  BinOpEq,
		ast.BinOpNe:  BinOpNe,
		ast.BinOpLt:  BinOpLt,
		ast.BinOpLe:  BinOpLe,
		ast.BinOpGt:  BinOpGt,
		ast.BinOpGe:  BinOpGe,
		ast.BinOpShl: BinOpShl,
		ast.BinOpShr: BinOpShr,
		ast.BinOpAnd: BinOpAnd,
		ast.BinOpOr:  BinOpOr,
	}

	irOp, ok := binOpMap[b.Operation]
	if !ok {
		b.Location().Errorf("unsupported binary operation: %s", b.Operation)

		return
	}

	// Pointer arithmetic scaling
	if b.Operation == ast.BinOpAdd || b.Operation == ast.BinOpSub {
		var ptrSide *Val
		var intSide *Val
		var ptrType *ast.Type
		isLhsPtr := leftType != nil && leftType.Kind == ast.TypePointer
		isRhsPtr := rightType != nil && rightType.Kind == ast.TypePointer
		if isLhsPtr != isRhsPtr {
			var elemSize int64 = 4
			if isLhsPtr {
				ptrSide = left
				intSide = right
				ptrType = leftType
			} else {
				ptrSide = right
				intSide = left
				ptrType = rightType
			}
			if ptrType != nil && ptrType.Elem != nil && ptrType.Elem.Kind == ast.TypeInt {
				elemSize = 4
			}

			// TODO: handle other element types
			if elemSize != 1 {
				tmpScaled := NewValIdent(b.Location(), v.nextIdent("idx"), intSide.AbiTy)
				v.appendInstruction(NewBinop(b.Location(), BinOpMul, tmpScaled, intSide, NewValInteger(b.Location(), elemSize, intSide.AbiTy)))
				// Convert word to long
				if tmpScaled.AbiTy.BaseTy != BaseLong {
					tmpLong := NewValIdent(b.Location(), v.nextIdent("tmp"), NewAbiTyBase(BaseLong))
					v.appendInstruction(NewConvert(b.Location(), tmpLong, tmpScaled))
					tmpScaled = tmpLong
				}

				// Perform the pointer arithmetic
				v.appendInstruction(NewBinop(b.Location(), irOp, result, ptrSide, tmpScaled))
				v.lastVal = result
				v.lastType = b.Type
				return
			}
		}
	}

	if rightType.Kind != leftType.Kind {
		// If types differ, we need to extend the small one
		if leftType.Kind == ast.TypeInt && rightType.Kind == ast.TypePointer {
			// Extend int to pointer
			tmp := NewValIdent(b.Lhs.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(rightType))
			v.appendInstruction(NewConvert(b.Lhs.Location(), tmp, left))
			left = tmp
			leftType = rightType // now both are pointer
		} else if leftType.Kind == ast.TypePointer && rightType.Kind == ast.TypeInt {
			tmp := NewValIdent(b.Lhs.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(leftType))
			v.appendInstruction(NewConvert(b.Rhs.Location(), tmp, right))
			right = tmp
			rightType = leftType // now both are pointer
		} else {
			b.Location().Errorf("type mismatch in binary operation: %s vs %s", leftType.String(), rightType.String())
		}
	}

	v.appendInstruction(NewBinop(b.Location(), irOp, result, left, right))
	v.lastVal = result
	v.lastType = b.Type
}

func (v *visitor) visitBinOpLogAnd(left *Val, b *ast.Binop, result *Val) {
	// Shape of a logical AND when lowered:
	// 		%tmp = <left>
	// 		jnz %tmp, @true, @false
	//  @false:
	// 		%result = %left
	//		jp @end
	// 	@true:
	// 		%tmp = <right>
	//		%result = %tmp
	//  @end:
	trueLabel := v.nextLabel("true")
	falseLabel := v.nextLabel("false")
	endLabel := v.nextLabel("end")

	v.appendInstruction(NewJnz(b.Location(), left, trueLabel, falseLabel))
	// @false:
	v.appendInstruction(NewLabel(b.Location(), falseLabel))
	v.appendInstruction(NewBinop(b.Location(), BinOpAdd, result, left, NewValInteger(b.Location(), 0, left.AbiTy)))
	v.appendInstruction(NewJmp(b.Location(), endLabel))
	// @true:
	v.appendInstruction(NewLabel(b.Location(), trueLabel))
	b.Rhs.Accept(v)
	right := v.lastVal
	v.appendInstruction(NewBinop(b.Location(), BinOpAdd, result, right, NewValInteger(b.Location(), 0, right.AbiTy)))
	// @end:
	v.appendInstruction(NewLabel(b.Location(), endLabel))
}

func (v *visitor) visitBinOpLogOr(left *Val, b *ast.Binop, result *Val) {
	// Shape of a logical OR when lowered:
	// 		%tmp = <left>
	// 		jnz %tmp, @true, @false
	//  @true:
	//		%result = %left
	//		jp @end
	// 	@false:
	// 		%tmp = <right>
	// 		%result = %tmp
	//  @end:
	trueLabel := v.nextLabel("true")
	falseLabel := v.nextLabel("false")
	endLabel := v.nextLabel("end")

	v.appendInstruction(NewJnz(b.Location(), left, trueLabel, falseLabel))
	// @true:
	v.appendInstruction(NewLabel(b.Location(), trueLabel))
	v.appendInstruction(NewBinop(b.Location(), BinOpAdd, result, left, NewValInteger(b.Location(), 0, left.AbiTy)))
	v.appendInstruction(NewJmp(b.Location(), endLabel))
	// @false:
	v.appendInstruction(NewLabel(b.Location(), falseLabel))
	b.Rhs.Accept(v)
	right := v.lastVal
	v.appendInstruction(NewBinop(b.Location(), BinOpAdd, result, right, NewValInteger(b.Location(), 0, right.AbiTy)))
	// @end:
	v.appendInstruction(NewLabel(b.Location(), endLabel))
}

func (v *visitor) VisitUnaryOp(u *ast.UnaryOp) {
	u.Expr.Accept(v)
	operand := v.lastVal
	operandType := v.lastType

	switch u.Operation {
	case ast.UnaryOpMinus:
		// Only support int for now
		if operandType != nil && operandType.Kind == ast.TypeInt {
			result := NewValIdent(u.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(operandType))
			zero := NewValInteger(u.Location(), 0, v.mapTypeToAbiTy(operandType))
			v.appendInstruction(NewBinop(u.Location(), BinOpSub, result, zero, operand))
			v.lastVal = result
			v.lastType = operandType
		} else {
			u.Location().Errorf("unary minus is only supported for int types, got: %s", operandType.String())
		}
	case ast.UnaryOpAddrOf:
		switch expr := u.Expr.(type) {
		case ast.LValue:
			// If the operand is an lvalue, use its address
			v.lastVal = v.lastAddress
			v.lastType = &ast.Type{Kind: ast.TypePointer, Elem: operandType}
		case *ast.Literal:
			// Use deduplication helper for string/int literals
			switch expr.Type.Kind {
			case ast.TypeInt:
				global := getOrCreateLiteralGlobal(v, u.Location(), expr.Type.Kind, expr.IntValue)
				v.lastVal = global
				v.lastType = &ast.Type{Kind: ast.TypePointer, Elem: expr.Type}
			case ast.TypeString:
				// NOTE(daniel): string literals are already pointers.
				fallthrough
			default:
				// Fallback: synthesize a temp local
				slot := v.nextTempVar(operandType, operand, u.Location())
				v.lastVal = slot
				v.lastType = &ast.Type{Kind: ast.TypePointer, Elem: operandType}
			}
		default:
			// Not an lvalue or literal: synthesize a temp, assign, and take its address
			slot := v.nextTempVar(operandType, operand, u.Location())
			v.lastVal = slot
			v.lastType = &ast.Type{Kind: ast.TypePointer, Elem: operandType}
		}
	default:
		u.Location().Errorf("unsupported unary operator: %s", u.Operation)
	}
}

func (v *visitor) VisitIf(iff *ast.If) {
	// Shape of an If statement when lowered:
	// 		%tmp = <cond>
	// 		jnz %tmp, @true, @false
	// @true:
	// 		<then block instructions>
	// 		jmp @end
	// @false:
	// 		<else block instructions>
	// @end:

	trueLabel := v.nextLabel("then")
	falseLabel := v.nextLabel("else")
	endLabel := v.nextLabel("end")

	for _, init := range iff.Init {
		init.Accept(v)
	}

	// Lower the condition
	iff.Cond.Accept(v)
	condVal := v.lastVal
	v.appendInstruction(NewJnz(iff.Cond.Location(), condVal, trueLabel, falseLabel))

	// Lower the 'then' block
	v.appendInstruction(NewLabel(iff.Then.Location(), trueLabel))
	iff.Then.Accept(v)
	v.appendInstruction(NewJmp(iff.Then.Location(), endLabel))

	// Lower the 'else' block if present
	if iff.Else == nil {
		v.appendInstruction(NewLabel(iff.Location(), falseLabel))
	} else {
		v.appendInstruction(NewLabel(iff.Else.Location(), falseLabel))
		iff.Else.Accept(v)
	}

	// End label for the If statement
	v.appendInstruction(NewLabel(iff.Location(), endLabel))
}

func (v *visitor) VisitFor(f *ast.For) {
	// Shape of a For loop when lowered:
	// 		<optional initializer>
	// @start:
	// 		<condition>
	// 		jnz %tmp, @body, @end
	// @body:
	// 		<loop body instructions>
	// 		<optional post-condition>
	// 		jmp @start
	// @end:

	startLabel := v.nextLabel("for")
	bodyLabel := v.nextLabel("body")
	endLabel := v.nextLabel("end")

	// Lower the initializers if present
	for _, init := range f.Init {
		init.Accept(v)
	}

	// Lower the condition
	{
		v.appendInstruction(NewLabel(f.Cond.Location(), startLabel))
		f.Cond.Accept(v)
		condVal := v.lastVal
		v.appendInstruction(NewJnz(f.Cond.Location(), condVal, bodyLabel, endLabel))
	}

	// Lower the loop body
	{
		v.appendInstruction(NewLabel(f.Body.Location(), bodyLabel))
		f.Body.Accept(v)

		// Lower the post-conditions if present
		for _, post := range f.Post {
			post.Accept(v)
		}

		v.appendInstruction(NewJmp(f.Body.Location(), startLabel))
	}

	// End label for the For loop
	v.appendInstruction(NewLabel(f.Location(), endLabel))
}

func (v *visitor) VisitVariableRef(vr *ast.VariableRef) {
	if slot, ok := v.localSlots[vr.Ident]; ok {
		v.lastAddress = slot
		if v.lvalue {
			val := v.lastVal
			v.lvalue = false
			v.appendInstruction(NewStore(vr.Location(), slot, val))
			return
		} else {
			// Always load from the stack slot for both parameters and locals
			tmp := NewValIdent(vr.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(vr.Type))
			v.appendInstruction(NewLoad(vr.Location(), tmp, slot))
			v.lastVal = tmp
			v.lastType = vr.Type
			return
		}
	}

	// If we reach here, the variable is not declared in the current scope
	vr.Location().Errorf("reference to undeclared variable: %s", vr.Ident)
}

// VisitDeref handles pointer dereference expressions
func (v *visitor) VisitDeref(d *ast.Deref) {
	if v.lvalue {
		val := v.lastVal
		v.lvalue = false // can't have lvalue in the expression

		// Lower the pointer expression
		d.Expr.Accept(v)
		addr := v.lastVal

		// Store: storew val, addr
		v.appendInstruction(NewStore(d.Location(), addr, val))
	} else {
		// Lower the pointer expression
		d.Expr.Accept(v)
		addr := v.lastVal

		// Load: %tmp =w loadw addr
		tmp := NewValIdent(d.Location(), v.nextIdent("tmp"), v.mapTypeToAbiTy(d.Type))
		v.appendInstruction(NewLoad(d.Location(), tmp, addr))

		v.lastVal = tmp
		v.lastType = d.Type
	}
}

func (v *visitor) VisitArrayIndex(a *ast.ArrayIndex) {
	if v.lvalue {
		val := v.lastVal
		v.lvalue = false // can't have lvalue in the array index

		// Lower the array expression
		a.Array.Accept(v)
		arrayAddr := v.lastVal

		// Compute the offset for the array index
		a.Index.Accept(v)
		index := v.lastVal

		// Convert the index to long if necessary
		if index.AbiTy.BaseTy != BaseLong {
			tmp := NewValIdent(a.Location(), v.nextIdent("idx"), NewAbiTyBase(BaseLong))
			v.appendInstruction(NewConvert(a.Location(), tmp, index))
			index = tmp
		}

		// Scale the index by the element size (assume 4 bytes for int)
		elemSize := int64(4)
		tmpScaled := NewValIdent(a.Location(), v.nextIdent("idx"), index.AbiTy)
		v.appendInstruction(NewBinop(a.Location(), BinOpMul, tmpScaled, index, NewValInteger(a.Location(), elemSize, index.AbiTy)))
		// Compute the address: addr = arrayAddr + index * elemSize
		v.appendInstruction(NewBinop(a.Location(), BinOpAdd, tmpScaled, tmpScaled, arrayAddr))
		// Store: storew val, addr
		v.appendInstruction(NewStore(a.Location(), tmpScaled, val))
	} else {
		// Lower array indexing: compute address and load value
		// 1. Lower base (array) expression
		a.Array.Accept(v)
		base := v.lastVal
		baseType := v.lastType

		// 2. Lower index expression
		a.Index.Accept(v)
		idx := v.lastVal

		// 3. Compute element size
		eleSize := int64(4) // default to 4 for int
		if baseType != nil && baseType.Kind == ast.TypeArray && baseType.Elem != nil {
			if baseType.Elem.Kind == ast.TypeInt {
				eleSize = 4
			}
			// TODO: handle other element types
		}

		// 4. Compute offset: idx * eleSize
		tmpMul := NewValIdent(a.Location(), v.nextIdent("idx"), idx.AbiTy)
		v.appendInstruction(NewBinop(a.Location(), BinOpMul, tmpMul, idx, NewValInteger(a.Location(), eleSize, idx.AbiTy)))

		// 5. Convert offset to long if needed
		offset := tmpMul
		if tmpMul.AbiTy.BaseTy != BaseLong {
			tmpLong := NewValIdent(a.Location(), v.nextIdent("tmp"), NewAbiTyBase(BaseLong))
			v.appendInstruction(NewConvert(a.Location(), tmpLong, tmpMul))
			offset = tmpLong
		}

		// 6. Compute address: base + offset
		addr := NewValIdent(a.Location(), v.nextIdent("addr"), NewAbiTyBase(BaseLong))
		v.appendInstruction(NewBinop(a.Location(), BinOpAdd, addr, base, offset))

		// 7. For r-value: load from address
		result := NewValIdent(a.Location(), v.nextIdent("tmp"), NewAbiTyBase(BaseWord))
		v.appendInstruction(NewLoad(a.Location(), result, addr))
		v.lastVal = result
		v.lastType = baseType.Elem
		v.lastAddress = addr
	}
}

func (v *visitor) acceptLValue(node interface{ Accept(ast.Visitor) }) {
	v.lvalue = true
	node.Accept(v)
	v.lvalue = false
}

func (v *visitor) appendInstruction(instr Instruction) {
	if _, ok := instr.(*Label); ok {
		v.lastInstructions = append(v.lastInstructions, instr)

		return
	}

	// If the previous instruction was a Ret, we need to add a label for the new block
	if len(v.lastInstructions) > 0 {
		if ret, ok := v.lastInstructions[len(v.lastInstructions)-1].(*Ret); ok {
			// Append a label to separate instructions
			label := v.nextLabel("block")
			v.lastInstructions = append(v.lastInstructions, NewLabel(ret.Location(), label))
		}
	}

	// Append an instruction to the last instructions
	v.lastInstructions = append(v.lastInstructions, instr)
}

func (v *visitor) nextLabel(tag string) string {
	// Generate a unique label identifier
	v.labelCounter++
	return fmt.Sprintf("L%04d_%s", v.labelCounter, tag)
}

// nextIdent generates a unique identifier with the given prefix (e.g., "tmp" or "str").
func (v *visitor) nextIdent(prefix string) Ident {
	v.tmpCounter++

	return Ident(fmt.Sprintf("_%s_%04d", prefix, v.tmpCounter))
}

// synthesizeTempVar creates a new temporary local variable, assigns the given value to it, and returns its identifier.
// Used for lowering address-of on literals and non-addressable expressions.
func (v *visitor) nextTempVar(ty *ast.Type, value *Val, loc lexer.Location) *Val {
	tmpName := v.nextIdent("tmp")
	abiTy := v.mapTypeToAbiTy(ty)
	slot := NewValIdent(loc, tmpName, abiTy)
	// Assume 4 bytes for int/bool, 8 for long/pointer
	var size int64 = 4
	switch abiTy.BaseTy {
	case BaseLong:
		size = 8
	case BaseWord:
		size = 4
	}
	sizeVal := NewValInteger(loc, size, abiTy)
	v.lastInstructions = append(v.lastInstructions, NewAlloc(loc, slot, sizeVal))
	v.lastInstructions = append(v.lastInstructions, NewStore(loc, slot, value))
	v.localSlots[string(tmpName)] = slot
	return slot
}

// mapTypeToAbiTy maps an *ast.Type to the appropriate AbiTy for IR lowering.
func (v *visitor) mapTypeToAbiTy(ty *ast.Type) AbiTy {
	if ty == nil {
		return NewAbiTyBase(BaseWord)
	}
	switch ty.Kind {
	case ast.TypeInt, ast.TypeBool:
		return NewAbiTyBase(BaseWord)
	case ast.TypeString:
		return NewAbiTyBase(BaseLong)
	case ast.TypePointer:
		return NewAbiTyBase(BaseLong)
	case ast.TypeArray:
		return NewAbiTyBase(BaseLong)
	default:
		return NewAbiTyBase(BaseWord) // fallback
	}
}
