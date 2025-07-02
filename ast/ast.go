package ast

import "maps"

// Visitor interface for double-dispatch on AST nodes.
type Visitor interface {
	VisitCompilationUnit(*CompilationUnit)
	VisitTypeDef(*TypeDef)
	VisitDataDef(*DataDef)
	VisitFuncDef(*FuncDef)
	VisitFuncParam(*FuncParam)
	VisitBody(*Body)
	VisitCall(*Call)
	VisitDeclare(*Declare)
	VisitAssign(*Assign)
	VisitReturn(*Return)
	VisitLiteral(*Literal)
	VisitBinop(*Binop)
	VisitVariableRef(*VariableRef)
	VisitDeref(*Deref)
	VisitIf(*If)
	VisitFor(*For)
}

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
}

type CompilationUnit struct {
	Ident      string // package name
	Types      []*TypeDef
	Data       []*DataDef
	Funcs      []*FuncDef
	Attributes Attributes
}

// NewCompilationUnit creates a new, empty CompilationUnit.
func NewCompilationUnit() *CompilationUnit {
	return &CompilationUnit{
		Types:      nil,
		Data:       nil,
		Funcs:      nil,
		Attributes: Attributes{},
	}
}

// Accept implements the Visitor pattern for CompilationUnit.
func (cu *CompilationUnit) Accept(v Visitor) {
	v.VisitCompilationUnit(cu)
}

type TypeDef struct {
	Ident      string // type name
	Type       *Type
	Value      Expression // optional initial value
	Attributes Attributes
}

func (td *TypeDef) Accept(v Visitor) {
	v.VisitTypeDef(td)
}

type DataDef struct {
	Ident      string // data name
	Type       *Type
	Value      Expression // optional initial value
	Attributes Attributes
}

func (dd *DataDef) Accept(v Visitor) {
	v.VisitDataDef(dd)
}

type FuncDef struct {
	Ident      string // function name
	Params     []*FuncParam
	ReturnType *Type
	Body       *Body
	Attributes Attributes
}

func NewFuncDef(ident string, attributes Attributes) *FuncDef {
	return &FuncDef{
		Ident:      ident,
		Params:     nil,
		ReturnType: &Type{Kind: TypeVoid},
		Body:       nil,
		Attributes: maps.Clone(attributes),
	}
}

func (fd *FuncDef) Accept(v Visitor) {
	v.VisitFuncDef(fd)
}

type FuncParam struct {
	Ident      string // parameter name
	Type       *Type
	Value      Expression // optional default value
	Attributes Attributes
}

func (fp *FuncParam) Accept(v Visitor) {
	v.VisitFuncParam(fp)
}

type Body struct {
	Instructions []Instruction
}

func (b *Body) Accept(v Visitor) {
	v.VisitBody(b)
}

func (*Body) isInstruction() {}

type Instruction interface {
	isInstruction()
	String() string
	Accept(v Visitor)
}

var _ []Instruction = []Instruction{
	(*Call)(nil),
	(*Declare)(nil),
	(*Assign)(nil),
	(*Return)(nil),
	(*If)(nil),
	(*For)(nil),
	(*Body)(nil),
}

// LValue represents an assignable location (left-hand side of assignment)
type LValue interface {
	isLValue()
	Expression
}

// Declare represents a variable declaration (with or without type)
type Declare struct {
	Ident string
	Type  *Type // declared type, or TypeUnknown
}

func (d *Declare) Accept(v Visitor) {
	v.VisitDeclare(d)
}

func (*Declare) isInstruction() {}

// Assign represents assignment to an lvalue (e.g., x = 1, p^ = 2, a[0] = 3)
type Assign struct {
	LHS   LValue
	Type  *Type
	Value Expression
}

func (a *Assign) Accept(v Visitor) {
	v.VisitAssign(a)
}

func (*Assign) isInstruction() {}

// If represents an if/else if/else statement.
type If struct {
	Init []Instruction // optional initializer(s); can be nil or empty
	Cond Expression    // condition expression
	Then *Body         // body for the 'if' branch
	Else Instruction   // *If, *Body, or nil
}

func (iff *If) Accept(v Visitor) {
	v.VisitIf(iff)
}

func (*If) isInstruction() {}

type For struct {
	Init []Instruction // optional initializer(s); can be nil or empty
	Cond Expression
	Post []Instruction // optional post-condition(s); can be nil or empty
	Body *Body
}

func (f *For) Accept(v Visitor) {
	v.VisitFor(f)
}

func (*For) isInstruction() {}

type Call struct {
	Ident string // function name
	Type  *Type  // return type, if any
	Args  []Arg
}

func (c *Call) Accept(v Visitor) {
	v.VisitCall(c)
}

func NewCall(ident string, args ...Arg) *Call {
	return &Call{
		Ident: ident,
		Args:  args,
	}
}

func (*Call) isInstruction() {}
func (*Call) isExpression()  {}

type Arg struct {
	Ident string     // (optional) argument name
	Value Expression // argument value
	Type  *Type
}

type Return struct {
	Value Expression // optional return value
}

func (r *Return) Accept(v Visitor) {
	v.VisitReturn(r)
}

func NewReturn(val ...Expression) *Return {
	switch len(val) {
	case 0:
		return &Return{}
	case 1:
		return &Return{Value: val[0]}
	default:
		panic("Return can only have one value")
	}
}

func (*Return) isInstruction() {}

type Expression interface {
	isExpression()
	String() string
	Accept(v Visitor)
}

var _ []Expression = []Expression{
	(*Literal)(nil),
	(*Binop)(nil),
	(*VariableRef)(nil),
	(*Deref)(nil),
}

// Deref represents a pointer dereference expression (e.g., a^)
type Deref struct {
	Expr Expression // the pointer expression to dereference
	Type *Type      // the type after dereferencing
}

func (d *Deref) Accept(v Visitor) {
	v.VisitDeref(d)
}

func (*Deref) isExpression() {}
func (*Deref) isLValue()     {}

func NewDeref(expr Expression) *Deref {
	return &Deref{
		Expr: expr,
		Type: &Type{Kind: TypeUnknown},
	}
}

type VariableRef struct {
	Ident string
	Type  *Type
}

func (vref *VariableRef) Accept(v Visitor) {
	v.VisitVariableRef(vref)
}

func NewVariableRef(ident string, ty TypeKind) *VariableRef {
	return &VariableRef{
		Ident: ident,
		Type:  &Type{Kind: ty},
	}
}

func (*VariableRef) isExpression() {}
func (*VariableRef) isLValue()     {}

type Literal struct {
	Type        *Type
	IntValue    int
	StringValue string
	BoolValue   bool
}

func (l *Literal) Accept(v Visitor) {
	v.VisitLiteral(l)
}

func NewIntLiteral(val int) *Literal {
	return &Literal{
		Type:     &Type{Kind: TypeInt},
		IntValue: val,
	}
}

func NewBoolLiteral(val bool) *Literal {
	return &Literal{
		Type:      &Type{Kind: TypeBool},
		BoolValue: val,
	}
}

func NewStringLiteral(val string) *Literal {
	return &Literal{
		Type:        &Type{Kind: TypeString},
		StringValue: val,
	}
}

func (*Literal) isExpression() {}

// BinOpKind represents the kind of binary operation.
type BinOpKind string

const (
	BinOpAdd    BinOpKind = "+"
	BinOpSub    BinOpKind = "-"
	BinOpMul    BinOpKind = "*"
	BinOpDiv    BinOpKind = "/"
	BinOpEq     BinOpKind = "=="
	BinOpNe     BinOpKind = "!="
	BinOpLt     BinOpKind = "<"
	BinOpLe     BinOpKind = "<="
	BinOpGt     BinOpKind = ">"
	BinOpGe     BinOpKind = ">="
	BinOpShl    BinOpKind = "<<"
	BinOpShr    BinOpKind = ">>"
	BinOpAnd    BinOpKind = "&"
	BinOpOr     BinOpKind = "|"
	BinOpLogAnd BinOpKind = "&&"
	BinOpLogOr  BinOpKind = "||"
)

type Binop struct {
	Operation BinOpKind
	Lhs, Rhs  Expression
	Type      *Type
}

func (b *Binop) Accept(v Visitor) {
	v.VisitBinop(b)
}

// NewBinop creates a new Binop node. Only Add is supported for now.
func NewBinop(op BinOpKind, lhs, rhs Expression) *Binop {
	return &Binop{
		Operation: op,
		Lhs:       lhs,
		Rhs:       rhs,
		Type:      &Type{Kind: TypeUnknown},
	}
}

func (*Binop) isExpression() {}
