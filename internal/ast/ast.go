package ast

import (
	"maps"

	"github.com/corani/cubit/internal/lexer"
)

// Visitor interface for double-dispatch on AST nodes.
type Visitor interface {
	VisitCompilationUnit(*CompilationUnit)
	VisitTypeDef(*TypeDef)
	VisitDataDef(*DataDef)
	VisitFuncDef(*FuncDef)
	VisitGenericParam(*GenericParam)
	VisitFuncParam(*FuncParam)
	VisitBody(*Body)
	VisitCall(*Call)
	VisitDeclare(*Declare)
	VisitAssign(*Assign)
	VisitReturn(*Return)
	VisitLiteral(*Literal)
	VisitBinop(*Binop)
	VisitUnaryOp(*UnaryOp)
	VisitVariableRef(*VariableRef)
	VisitDeref(*Deref)
	VisitArrayIndex(*ArrayIndex)
	VisitIf(*If)
	VisitFor(*For)
}

type CompilationUnit struct {
	Ident      string            // package name
	Imports    map[string]Import // imported packages (alias -> Import)
	Types      []*TypeDef
	Data       []*DataDef
	Funcs      []*FuncDef
	Attributes Attributes
	Loc        lexer.Location
}

type Import struct {
	Name string           // The package name (as written in import "...")
	Unit *CompilationUnit // The loaded AST, nil until loaded
}

// NewCompilationUnit creates a new, empty CompilationUnit.
func NewCompilationUnit(location lexer.Location) *CompilationUnit {
	return &CompilationUnit{
		Ident:      "",
		Imports:    make(map[string]Import),
		Types:      nil,
		Data:       nil,
		Funcs:      nil,
		Attributes: Attributes{},
		Loc:        location,
	}
}

func (cu *CompilationUnit) Location() lexer.Location {
	return cu.Loc
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
	Loc        lexer.Location
}

func NewTypeDef(ident string, ty *Type, value Expression, attributes Attributes, location lexer.Location) *TypeDef {
	return &TypeDef{
		Ident:      ident,
		Type:       ty,
		Value:      value,
		Attributes: maps.Clone(attributes),
		Loc:        location,
	}
}

func (td *TypeDef) Location() lexer.Location {
	return td.Loc
}

func (td *TypeDef) Accept(v Visitor) {
	v.VisitTypeDef(td)
}

type DataDef struct {
	Ident      string // data name
	Type       *Type
	Value      Expression // optional initial value
	Attributes Attributes
	Loc        lexer.Location
}

func NewDataDef(ident string, ty *Type, value Expression, attributes Attributes, location lexer.Location) *DataDef {
	return &DataDef{
		Ident:      ident,
		Type:       ty,
		Value:      value,
		Attributes: maps.Clone(attributes),
		Loc:        location,
	}
}

func (dd *DataDef) Location() lexer.Location {
	return dd.Loc
}

func (dd *DataDef) Accept(v Visitor) {
	v.VisitDataDef(dd)
}

type FuncDef struct {
	Ident         string          // function name
	GenericParams []*GenericParam // generic parameters, if any
	Params        []*FuncParam    // function parameters
	ReturnType    *Type           // return type
	Body          *Body           // function body
	Attributes    Attributes      // function attributes
	Loc           lexer.Location  // location information
}

func NewFuncDef(ident string, attributes Attributes, location lexer.Location) *FuncDef {
	return &FuncDef{
		Ident:      ident,
		Params:     nil,
		ReturnType: &Type{Kind: TypeVoid},
		Body:       nil,
		Attributes: maps.Clone(attributes),
		Loc:        location,
	}
}

func (fd *FuncDef) Location() lexer.Location {
	return fd.Loc
}

func (fd *FuncDef) Accept(v Visitor) {
	v.VisitFuncDef(fd)
}

type FuncParam struct {
	Ident      string // parameter name
	Type       *Type
	Value      Expression // optional default value
	Attributes Attributes
	Loc        lexer.Location
}

func NewFuncParam(ident string, ty *Type, value Expression, attributes Attributes, location lexer.Location) *FuncParam {
	return &FuncParam{
		Ident:      ident,
		Type:       ty,
		Value:      value,
		Attributes: maps.Clone(attributes),
		Loc:        location,
	}
}

func (fp *FuncParam) Location() lexer.Location {
	return fp.Loc
}

func (fp *FuncParam) Accept(v Visitor) {
	v.VisitFuncParam(fp)
}

type Body struct {
	Instructions []Instruction
	Loc          lexer.Location
}

func NewBody(instructions []Instruction, location lexer.Location) *Body {
	return &Body{
		Instructions: instructions,
		Loc:          location,
	}
}

func (b *Body) Location() lexer.Location {
	return b.Loc
}

func (b *Body) Accept(v Visitor) {
	v.VisitBody(b)
}

func (*Body) isInstruction() {}

type Instruction interface {
	isInstruction()
	Location() lexer.Location
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
	Loc   lexer.Location
}

func NewDeclare(ident string, ty *Type, location lexer.Location) *Declare {
	return &Declare{
		Ident: ident,
		Type:  ty,
		Loc:   location,
	}
}

func (d *Declare) Location() lexer.Location {
	return d.Loc
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
	Loc   lexer.Location
}

func NewAssign(lhs LValue, value Expression, ty *Type, location lexer.Location) *Assign {
	return &Assign{
		LHS:   lhs,
		Type:  ty,
		Value: value,
		Loc:   location,
	}
}

func (a *Assign) Location() lexer.Location {
	return a.Loc
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
	Else *Body         // optional body for the 'else' branch
	Loc  lexer.Location
}

func NewIf(location lexer.Location, init []Instruction, cond Expression, then *Body, elseBranch *Body) *If {
	return &If{
		Init: init,
		Cond: cond,
		Then: then,
		Else: elseBranch,
		Loc:  location,
	}
}

func (i *If) Location() lexer.Location {
	return i.Loc
}

func (i *If) Accept(v Visitor) {
	v.VisitIf(i)
}

func (*If) isInstruction() {}

type For struct {
	Init []Instruction // optional initializer(s); can be nil or empty
	Cond Expression
	Post []Instruction // optional post-condition(s); can be nil or empty
	Body *Body
	Loc  lexer.Location
}

func NewFor(location lexer.Location, init []Instruction, cond Expression, post []Instruction, body *Body) *For {
	return &For{
		Init: init,
		Cond: cond,
		Post: post,
		Body: body,
		Loc:  location,
	}
}

func (f *For) Location() lexer.Location {
	return f.Loc
}

func (f *For) Accept(v Visitor) {
	v.VisitFor(f)
}

func (*For) isInstruction() {}

type Call struct {
	Namespace string   // optional namespace (e.g. "raylib")
	Ident     string   // function name
	Type      *Type    // return type, if any
	FuncDef   *FuncDef // set during type checking
	Args      []Arg
	Loc       lexer.Location
}

// NewCall creates a Call with a namespace.
func NewCall(location lexer.Location, namespace, ident string, args ...Arg) *Call {
	return &Call{
		Namespace: namespace,
		Ident:     ident,
		Args:      args,
		Loc:       location,
	}
}

func (c *Call) Location() lexer.Location {
	return c.Loc
}

func (c *Call) Accept(v Visitor) {
	v.VisitCall(c)
}

func (*Call) isInstruction() {}
func (*Call) isExpression()  {}

type Arg struct {
	Ident string     // (optional) argument name
	Value Expression // argument value
	Type  *Type
	Loc   lexer.Location
}

func NewArg(ident string, value Expression, ty *Type, location lexer.Location) Arg {
	return Arg{
		Ident: ident,
		Value: value,
		Type:  ty,
		Loc:   location,
	}
}

func (a *Arg) Location() lexer.Location {
	return a.Loc
}

type Return struct {
	Value Expression // optional return value
	Type  *Type
	Loc   lexer.Location
}

func NewReturn(location lexer.Location, ty *Type, val ...Expression) *Return {
	switch len(val) {
	case 0:
		return &Return{Loc: location, Type: ty}
	case 1:
		return &Return{Value: val[0], Loc: location, Type: ty}
	default:
		panic("Return can only have one value")
	}
}

func (r *Return) Location() lexer.Location {
	return r.Loc
}

func (r *Return) Accept(v Visitor) {
	v.VisitReturn(r)
}

func (*Return) isInstruction() {}

type Expression interface {
	isExpression()
	Location() lexer.Location
	Accept(v Visitor)
}

var _ []Expression = []Expression{
	(*Literal)(nil),
	(*Binop)(nil),
	(*UnaryOp)(nil),
	(*VariableRef)(nil),
	(*Deref)(nil),
	(*Call)(nil),
	(*ArrayIndex)(nil),
}

// Deref represents a pointer dereference expression (e.g., a^)
type Deref struct {
	Expr Expression // the pointer expression to dereference
	Type *Type      // the type after dereferencing
	Loc  lexer.Location
}

func NewDeref(expr Expression, location lexer.Location) *Deref {
	return &Deref{
		Expr: expr,
		Type: &Type{Kind: TypeUnknown},
		Loc:  location,
	}
}

func (d *Deref) Location() lexer.Location {
	return d.Loc
}

func (d *Deref) Accept(v Visitor) {
	v.VisitDeref(d)
}

func (*Deref) isExpression() {}
func (*Deref) isLValue()     {}

type VariableRef struct {
	Namespace string // optional namespace (e.g. "raylib")
	Ident     string
	Type      *Type
	Loc       lexer.Location
}

// NewVariableRef creates a VariableRef with a namespace.
func NewVariableRef(namespace, ident string, ty TypeKind, location lexer.Location) *VariableRef {
	return &VariableRef{
		Namespace: namespace,
		Ident:     ident,
		Type:      &Type{Kind: ty},
		Loc:       location,
	}
}

func (vref *VariableRef) Location() lexer.Location {
	return vref.Loc
}

func (vref *VariableRef) Accept(v Visitor) {
	v.VisitVariableRef(vref)
}

func (*VariableRef) isExpression() {}
func (*VariableRef) isLValue()     {}

type Literal struct {
	Type        *Type
	IntValue    int
	StringValue string
	BoolValue   bool
	ArrayValue  []Literal
	Loc         lexer.Location
}

func NewArrayLiteral(ty *Type, elements []Literal, location lexer.Location) *Literal {
	return &Literal{
		Type:       ty,
		ArrayValue: elements,
		Loc:        location,
	}
}
func NewIntLiteral(val int, location lexer.Location) *Literal {
	return &Literal{
		Type:     &Type{Kind: TypeInt},
		IntValue: val,
		Loc:      location,
	}
}

func NewBoolLiteral(val bool, location lexer.Location) *Literal {
	return &Literal{
		Type:      &Type{Kind: TypeBool},
		BoolValue: val,
		Loc:       location,
	}
}

func NewStringLiteral(val string, location lexer.Location) *Literal {
	return &Literal{
		Type:        &Type{Kind: TypeString},
		StringValue: val,
		Loc:         location,
	}
}

func (l *Literal) Location() lexer.Location {
	return l.Loc
}

func (l *Literal) Accept(v Visitor) {
	v.VisitLiteral(l)
}

func (*Literal) isExpression() {}

// BinOpKind represents the kind of binary operation.
type BinOpKind string

const (
	BinOpAdd    BinOpKind = "+"
	BinOpSub    BinOpKind = "-"
	BinOpMul    BinOpKind = "*"
	BinOpDiv    BinOpKind = "/"
	BinOpMod    BinOpKind = "%"
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
	Loc       lexer.Location
}

// NewBinop creates a new Binop node. Only Add is supported for now.
func NewBinop(op BinOpKind, lhs, rhs Expression, location lexer.Location) *Binop {
	return &Binop{
		Operation: op,
		Lhs:       lhs,
		Rhs:       rhs,
		Type:      &Type{Kind: TypeUnknown},
		Loc:       location,
	}
}

func (b *Binop) Location() lexer.Location {
	return b.Loc
}

func (b *Binop) Accept(v Visitor) {
	v.VisitBinop(b)
}

func (*Binop) isExpression() {}

// ArrayIndex represents an array access (e.g., data[1])
type ArrayIndex struct {
	Array Expression // the array variable/expression
	Index Expression // the index expression
	Type  *Type      // the type of the array element
	Loc   lexer.Location
}

func NewArrayIndex(array, index Expression, location lexer.Location) *ArrayIndex {
	return &ArrayIndex{
		Array: array,
		Index: index,
		Type:  &Type{Kind: TypeUnknown},
		Loc:   location,
	}
}

func (a *ArrayIndex) Location() lexer.Location {
	return a.Loc
}

func (a *ArrayIndex) Accept(v Visitor) {
	v.VisitArrayIndex(a)
}

func (*ArrayIndex) isExpression() {}
func (*ArrayIndex) isLValue()     {}

// UnaryOpKind represents the kind of unary operation.
type UnaryOpKind string

const (
	UnaryOpMinus UnaryOpKind = "-"
)

type UnaryOp struct {
	Operation UnaryOpKind
	Expr      Expression
	Type      *Type
	Loc       lexer.Location
}

func NewUnaryOp(op UnaryOpKind, expr Expression, location lexer.Location) *UnaryOp {
	return &UnaryOp{
		Operation: op,
		Expr:      expr,
		Type:      &Type{Kind: TypeUnknown},
		Loc:       location,
	}
}

func (u *UnaryOp) Location() lexer.Location {
	return u.Loc
}

func (u *UnaryOp) Accept(v Visitor) {
	v.VisitUnaryOp(u)
}

func (*UnaryOp) isExpression() {}
