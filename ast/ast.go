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
	VisitAssign(*Assign)
	VisitSet(*Set)
	VisitReturn(*Return)
	VisitLiteral(*Literal)
	VisitBinop(*Binop)
	VisitVariableRef(*VariableRef)
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
)

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
	Type       TypeKind
	Value      Expression // optional initial value
	Attributes Attributes
}

func (td *TypeDef) Accept(v Visitor) {
	v.VisitTypeDef(td)
}

type DataDef struct {
	Ident      string // data name
	Type       TypeKind
	Value      Expression // optional initial value
	Attributes Attributes
}

func (dd *DataDef) Accept(v Visitor) {
	v.VisitDataDef(dd)
}

type FuncDef struct {
	Ident      string // function name
	Params     []*FuncParam
	ReturnType TypeKind
	Body       *Body
	Attributes Attributes
}

func NewFuncDef(ident string, attributes Attributes) *FuncDef {
	return &FuncDef{
		Ident:      ident,
		Params:     nil,
		ReturnType: TypeVoid,
		Body:       nil,
		Attributes: maps.Clone(attributes),
	}
}

func (fd *FuncDef) Accept(v Visitor) {
	v.VisitFuncDef(fd)
}

type FuncParam struct {
	Ident      string // parameter name
	Type       TypeKind
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
	(*Assign)(nil),
	(*Set)(nil),
	(*Return)(nil),
	(*If)(nil),
	(*For)(nil),
	(*Body)(nil),
}

// Set represents assignment to an existing variable (e.g., x = 1)
type Set struct {
	Ident string
	Type  TypeKind // type of the variable being set
	Value Expression
}

func (s *Set) Accept(v Visitor) {
	v.VisitSet(s)
}

func (*Set) isInstruction() {}

// If represents an if/else if/else statement.
type If struct {
	Init *Assign     // optional initializer (assignment); can be nil
	Cond Expression  // condition expression
	Then *Body       // body for the 'if' branch
	Else Instruction // *If, *Body, or nil
}

func (iff *If) Accept(v Visitor) {
	v.VisitIf(iff)
}

func (*If) isInstruction() {}

type For struct {
	Cond Expression
	Body *Body
}

func (f *For) Accept(v Visitor) {
	v.VisitFor(f)
}

func (*For) isInstruction() {}

type Call struct {
	Ident string   // function name
	Type  TypeKind // return type, if any
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
	Type  TypeKind
}

type Assign struct {
	Ident string     // variable name
	Type  TypeKind   // variable type
	Value Expression // right-hand side expression
}

func (a *Assign) Accept(v Visitor) {
	v.VisitAssign(a)
}

func (*Assign) isInstruction() {}

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
}

type VariableRef struct {
	Ident string
	Type  TypeKind
}

func (vref *VariableRef) Accept(v Visitor) {
	v.VisitVariableRef(vref)
}

func NewVariableRef(ident string, ty TypeKind) *VariableRef {
	return &VariableRef{
		Ident: ident,
		Type:  ty,
	}
}

func (*VariableRef) isExpression() {}

type Literal struct {
	Type        TypeKind
	IntValue    int
	StringValue string
	BoolValue   bool
}

func (l *Literal) Accept(v Visitor) {
	v.VisitLiteral(l)
}

func NewIntLiteral(val int) *Literal {
	return &Literal{
		Type:     TypeInt,
		IntValue: val,
	}
}

func NewBoolLiteral(val bool) *Literal {
	return &Literal{
		Type:      TypeBool,
		BoolValue: val,
	}
}

func NewStringLiteral(val string) *Literal {
	return &Literal{
		Type:        TypeString,
		StringValue: val,
	}
}

func (*Literal) isExpression() {}

// BinOpKind represents the kind of binary operation.
type BinOpKind string

const (
	BinOpAdd BinOpKind = "+"
	BinOpSub BinOpKind = "-"
	BinOpMul BinOpKind = "*"
	BinOpDiv BinOpKind = "/"
	BinOpEq  BinOpKind = "=="
	BinOpNe  BinOpKind = "!="
	BinOpLt  BinOpKind = "<"
	BinOpLe  BinOpKind = "<="
	BinOpGt  BinOpKind = ">"
	BinOpGe  BinOpKind = ">="
)

type Binop struct {
	Operation BinOpKind
	Lhs, Rhs  Expression
	Type      TypeKind
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
		Type:      TypeUnknown,
	}
}

func (*Binop) isExpression() {}
