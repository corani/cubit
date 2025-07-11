package ir

import "github.com/corani/cubit/lexer"

// Visitor defines the visitor interface for SSA code generation.
type Visitor interface {
	VisitCompilationUnit(*CompilationUnit) string
	VisitTypeDef(*TypeDef) string
	VisitDataDef(*DataDef) string
	VisitFuncDef(*FuncDef) string
	VisitLabel(*Label) string
	VisitRet(*Ret) string
	VisitCall(*Call) string
	VisitBinop(*Binop) string
	VisitJmp(*Jmp) string
	VisitJnz(*Jnz) string
	VisitLoad(*Load) string
	VisitStore(*Store) string
	VisitConvert(*Convert) string
	VisitAlloc(*Alloc) string
}

type CompilationUnit struct {
	Loc      lexer.Location
	Package  string
	Types    []TypeDef
	DataDefs []DataDef
	FuncDefs []FuncDef
}

// Accept implements the classic visitor pattern for CompilationUnit.
func (cu *CompilationUnit) Accept(visitor Visitor) string {
	return visitor.VisitCompilationUnit(cu)
}

func NewCompilationUnit() *CompilationUnit {
	return &CompilationUnit{
		Types:    []TypeDef{},
		DataDefs: []DataDef{},
		FuncDefs: []FuncDef{},
	}
}

func (cu *CompilationUnit) WithPackage(name string, loc lexer.Location) *CompilationUnit {
	cu.Package = name
	cu.Loc = loc

	return cu
}

func (cu *CompilationUnit) WithTypes(types ...TypeDef) *CompilationUnit {
	cu.Types = append(cu.Types, types...)

	return cu
}

func (cu *CompilationUnit) WithDataDefs(dataDefs ...DataDef) *CompilationUnit {
	cu.DataDefs = append(cu.DataDefs, dataDefs...)

	return cu
}

func (cu *CompilationUnit) WithFuncDefs(funcDefs ...FuncDef) *CompilationUnit {
	cu.FuncDefs = append(cu.FuncDefs, funcDefs...)

	return cu
}

type (
	Ident  string
	BaseTy string
)

const (
	BaseWord   BaseTy = "w"
	BaseLong   BaseTy = "l"
	BaseSingle BaseTy = "s"
	BaseDouble BaseTy = "d"
)

type ExtTy string

const (
	ExtByte   = ExtTy("b")
	ExtHalf   = ExtTy("h")
	ExtWord   = ExtTy(BaseWord)
	ExtLong   = ExtTy(BaseLong)
	ExtSingle = ExtTy(BaseSingle)
	ExtDouble = ExtTy(BaseDouble)
)

type SubTy struct {
	Type  SubTyType
	ExtTy ExtTy
	Ident Ident
}

type SubTyType string

const (
	SubTyExt   SubTyType = "ext"
	SubTyIdent SubTyType = "ident"
)

type SubTySize struct {
	SubTy SubTy
	Size  int
}

func NewSubTyExtSize(extTy ExtTy, size int) SubTySize {
	return SubTySize{
		SubTy: SubTy{Type: SubTyExt, ExtTy: extTy},
		Size:  size,
	}
}

func NewSubTyIdentSize(ident Ident, size int) SubTySize {
	return SubTySize{
		SubTy: SubTy{Type: SubTyIdent, Ident: ident},
		Size:  size,
	}
}

type Const struct {
	Loc   lexer.Location
	Type  ConstType
	F32   float32
	F64   float64
	I64   int64
	Ident Ident
}

func NewConstInteger(loc lexer.Location, i int64) Const {
	return Const{Loc: loc, Type: ConstInteger, I64: i}
}

func NewConstSingle(loc lexer.Location, f float32) Const {
	return Const{Loc: loc, Type: ConstSingle, F32: f}
}

func NewConstDouble(loc lexer.Location, f float64) Const {
	return Const{Loc: loc, Type: ConstDouble, F64: f}
}

func NewConstIdent(loc lexer.Location, ident Ident) Const {
	return Const{Loc: loc, Type: ConstIdent, Ident: ident}
}

type ConstType string

const (
	ConstInteger ConstType = "integer"
	ConstSingle  ConstType = "single"
	ConstDouble  ConstType = "double"
	ConstIdent   ConstType = "ident"
)

type DynConst struct {
	Loc   lexer.Location
	Type  DynConstType
	Const Const
	Ident Ident
}

func NewDynConst(loc lexer.Location, constv Const) DynConst {
	return DynConst{Loc: loc, Type: DynConstConst, Const: constv}
}

func NewDynConstThread(loc lexer.Location, ident Ident) DynConst {
	return DynConst{Loc: loc, Type: DynConstThread, Ident: ident}
}

type DynConstType string

const (
	DynConstConst  DynConstType = "const"
	DynConstThread DynConstType = "thread"
)

type ValType string

const (
	ValDynConst ValType = "dynconst"
	ValIdent    ValType = "ident"
)

type Val struct {
	Loc      lexer.Location
	Type     ValType
	DynConst DynConst
	Ident    Ident
	AbiTy    AbiTy
}

func NewValDynConst(loc lexer.Location, dc DynConst, abiTy AbiTy) *Val {
	return &Val{
		Loc:      loc,
		Type:     ValDynConst,
		DynConst: dc,
		AbiTy:    abiTy,
	}
}

func NewValGlobal(loc lexer.Location, ident Ident, abiTy AbiTy) *Val {
	v := NewValDynConst(loc, NewDynConst(loc, NewConstIdent(loc, ident)), abiTy)
	v.Ident = ident

	return v
}

func NewValInteger(loc lexer.Location, i int64, abiTy AbiTy) *Val {
	return NewValDynConst(loc, NewDynConst(loc, NewConstInteger(loc, i)), abiTy)
}

func NewValIdent(loc lexer.Location, ident Ident, abiTy AbiTy) *Val {
	return &Val{
		Loc:   loc,
		Type:  ValIdent,
		Ident: ident,
		AbiTy: abiTy,
	}
}

type Linkage struct {
	Loc      lexer.Location
	Type     LinkageType
	SecName  string
	SecFlags string
}

func NewLinkageExport(loc lexer.Location) Linkage {
	return Linkage{Loc: loc, Type: LinkageExport}
}

func NewLinkageThread(loc lexer.Location) Linkage {
	return Linkage{Loc: loc, Type: LinkageThread}
}

func NewLinkageSection(loc lexer.Location, secName, secFlags string) Linkage {
	return Linkage{Loc: loc, Type: LinkageSection, SecName: secName, SecFlags: secFlags}
}

type LinkageType string

const (
	LinkageExport  LinkageType = "export"
	LinkageThread  LinkageType = "thread"
	LinkageSection LinkageType = "section"
)

type TypeDef struct {
	Loc         lexer.Location
	Type        TypeDefType
	Ident       Ident
	Align       int
	Fields      []SubTySize
	UnionFields [][]SubTySize
	OpaqueSize  int
}

func (td *TypeDef) Accept(visitor Visitor) string {
	return visitor.VisitTypeDef(td)
}

func NewTypeDefRegular(loc lexer.Location, ident Ident, fields ...SubTySize) TypeDef {
	return TypeDef{Loc: loc, Type: TypeDefRegular, Ident: ident, Fields: fields}
}

func NewTypeDefUnion(loc lexer.Location, ident Ident, unionFields ...[]SubTySize) TypeDef {
	return TypeDef{Loc: loc, Type: TypeDefUnion, Ident: ident, UnionFields: unionFields}
}

func NewTypeDefOpaque(loc lexer.Location, ident Ident, opaqueSize int) TypeDef {
	return TypeDef{Loc: loc, Type: TypeDefOpaque, Ident: ident, OpaqueSize: opaqueSize}
}

func (td TypeDef) WithAlign(align int) TypeDef {
	td.Align = align

	return td
}

type TypeDefType string

const (
	TypeDefRegular TypeDefType = "regular"
	TypeDefUnion   TypeDefType = "union"
	TypeDefOpaque  TypeDefType = "opaque"
)

type DataDef struct {
	Loc         lexer.Location
	Linkage     *Linkage
	Ident       Ident
	Align       int
	Initializer []DataInit
}

func (dd *DataDef) Accept(visitor Visitor) string {
	return visitor.VisitDataDef(dd)
}

func NewDataDef(loc lexer.Location, ident Ident, initializer ...DataInit) DataDef {
	return DataDef{Loc: loc, Ident: ident, Initializer: initializer}
}

func NewDataDefStringZ(loc lexer.Location, ident Ident, val string) DataDef {
	return NewDataDef(loc, ident,
		NewDataInitString(loc, val),
		NewDataInitExt(loc, ExtByte, NewDataItemInteger(loc, 0)),
	)
}

func (dd DataDef) WithLinkage(linkage Linkage) DataDef {
	dd.Linkage = &linkage

	return dd
}

func (dd DataDef) WithAlign(align int) DataDef {
	dd.Align = align

	return dd
}

type DataInit struct {
	Loc   lexer.Location
	Type  DataInitType
	ExtTy ExtTy
	Items []DataItem
	Size  int
}

func NewDataInitExt(loc lexer.Location, extTy ExtTy, items ...DataItem) DataInit {
	return DataInit{Loc: loc, Type: DataInitExt, ExtTy: extTy, Items: items}
}

func NewDataInitString(loc lexer.Location, val string) DataInit {
	return DataInit{
		Loc:   loc,
		Type:  DataInitExt,
		ExtTy: ExtByte,
		Items: []DataItem{NewDataItemString(loc, val)},
	}
}

func NewDataInitZero(loc lexer.Location, size int) DataInit {
	return DataInit{Loc: loc, Type: DataInitZero, Size: size}
}

type DataInitType string

const (
	DataInitExt  DataInitType = "ext"
	DataInitZero DataInitType = "zero"
)

type DataItem struct {
	Loc       lexer.Location
	Type      DataItemType
	Ident     Ident
	Offset    int
	StringVal string
	Const     Const
}

func NewDataItemConst(loc lexer.Location, c Const) DataItem {
	return DataItem{Loc: loc, Type: DataItemConst, Const: c}
}

func NewDataItemString(loc lexer.Location, val string) DataItem {
	return DataItem{Loc: loc, Type: DataItemString, StringVal: val}
}

func NewDataItemInteger(loc lexer.Location, i int64) DataItem {
	return NewDataItemConst(loc, NewConstInteger(loc, i))
}

func NewDataItemSymbol(loc lexer.Location, ident Ident, offset int) DataItem {
	return DataItem{Loc: loc, Type: DataItemSymbol, Ident: ident, Offset: offset}
}

type DataItemType string

const (
	DataItemSymbol DataItemType = "symbol"
	DataItemString DataItemType = "string"
	DataItemConst  DataItemType = "const"
)

type FuncDef struct {
	Loc      lexer.Location
	Linkage  *Linkage
	RetTy    *AbiTy
	Ident    Ident
	LinkName Ident
	Params   []*Param
	Blocks   []Block
}

func NewFuncDef(loc lexer.Location, ident Ident, params ...*Param) FuncDef {
	return FuncDef{Loc: loc, Ident: ident, Params: params}
}

func (fd *FuncDef) Accept(visitor Visitor) string {
	return visitor.VisitFuncDef(fd)
}

func (fd FuncDef) WithLinkage(linkage Linkage) FuncDef {
	fd.Linkage = &linkage
	return fd
}

func (fd FuncDef) WithRetTy(retTy AbiTy) FuncDef {
	fd.RetTy = &retTy
	return fd
}

func (fd FuncDef) WithBlocks(blocks ...Block) FuncDef {
	fd.Blocks = append(fd.Blocks, blocks...)
	return fd
}

type Param struct {
	Loc   lexer.Location
	Type  ParamType
	AbiTy AbiTy
	Ident Ident
}

func NewParamRegular(loc lexer.Location, abiTy AbiTy, ident Ident) *Param {
	return &Param{Loc: loc, Type: ParamRegular, AbiTy: abiTy, Ident: ident}
}

func NewParamEnv(loc lexer.Location, ident Ident) *Param {
	return &Param{Loc: loc, Type: ParamEnv, Ident: ident}
}

func NewParamVariadic(loc lexer.Location) *Param {
	return &Param{Loc: loc, Type: ParamVariadic}
}

type ParamType string

const (
	ParamRegular  ParamType = "regular"
	ParamEnv      ParamType = "env"
	ParamVariadic ParamType = "variadic"
)

type AbiTy struct {
	Type   AbiTyType
	BaseTy BaseTy
	SubWTy SubWTy
	Ident  Ident
}

func NewAbiTyBase(baseTy BaseTy) AbiTy {
	return AbiTy{Type: AbiTyBase, BaseTy: baseTy}
}

func NewAbiTySubW(subWTy SubWTy) AbiTy {
	return AbiTy{Type: AbiTySubW, SubWTy: subWTy}
}

func NewAbiTyIdent(ident Ident) AbiTy {
	return AbiTy{Type: AbiTyIdent, Ident: ident}
}

type AbiTyType string

const (
	AbiTyBase  AbiTyType = "base"
	AbiTySubW  AbiTyType = "subw"
	AbiTyIdent AbiTyType = "ident"
)

type SubWTy string

const (
	SubWSB SubWTy = "sb"
	SubWUB SubWTy = "ub"
	SubWSH SubWTy = "sh"
	SubWUH SubWTy = "uh"
)

type Block struct {
	Loc          lexer.Location
	Label        string
	Instructions []Instruction
}

func NewBlock(loc lexer.Location, label string, instructions []Instruction) Block {
	return Block{
		Loc:          loc,
		Label:        label,
		Instructions: instructions,
	}
}

// Instruction is a marker interface for all instruction types.
type Instruction interface {
	isInstruction()
	Accept(visitor Visitor) string
	Location() lexer.Location
}

var _ = []Instruction{
	(*Label)(nil),
	(*Ret)(nil),
	(*Call)(nil),
	(*Binop)(nil),
	(*Jmp)(nil),
	(*Jnz)(nil),
	(*Load)(nil),
	(*Store)(nil),
	(*Convert)(nil),
	(*Alloc)(nil),
}

// Label represents an SSA label.
type Label struct {
	Loc  lexer.Location
	Name string
}

func NewLabel(loc lexer.Location, name string) *Label {
	return &Label{Loc: loc, Name: name}
}

func (l *Label) isInstruction() {}

func (l *Label) Accept(visitor Visitor) string {
	return visitor.VisitLabel(l)
}

func (l *Label) Location() lexer.Location {
	return l.Loc
}

// Ret represents an SSA return instruction.
type Ret struct {
	Loc lexer.Location
	Val *Val
}

func (Ret) isInstruction() {}

func (r *Ret) Accept(visitor Visitor) string {
	return visitor.VisitRet(r)
}

func (r *Ret) Location() lexer.Location {
	return r.Loc
}

func NewRet(loc lexer.Location, val ...*Val) *Ret {
	if len(val) > 1 {
		panic("NewRet accepts at most one value")
	}

	if len(val) == 0 {
		return &Ret{}
	}

	return &Ret{Loc: loc, Val: val[0]}
}

// Call represents an SSA call instruction.
type Call struct {
	Loc   lexer.Location
	LHS   *Ident
	RetTy *AbiTy
	Val   *Val
	Args  []Arg
}

func (c *Call) isInstruction() {}

func (c *Call) Accept(visitor Visitor) string {
	return visitor.VisitCall(c)
}

func (c *Call) Location() lexer.Location {
	return c.Loc
}

func NewCall(loc lexer.Location, val *Val, args ...Arg) *Call {
	return &Call{Loc: loc, Val: val, Args: args}
}

func (c *Call) WithRet(lhs Ident, retTy AbiTy) *Call {
	c.LHS = &lhs
	c.RetTy = &retTy

	return c
}

// BinOpKind represents the kind of binary operation.
type BinOpKind string

const (
	BinOpAdd BinOpKind = "add"
	BinOpSub BinOpKind = "sub"
	BinOpMul BinOpKind = "mul"
	BinOpDiv BinOpKind = "div"
	BinOpMod BinOpKind = "mod"
	BinOpEq  BinOpKind = "eq"
	BinOpNe  BinOpKind = "ne"
	BinOpLt  BinOpKind = "lt"
	BinOpLe  BinOpKind = "le"
	BinOpGt  BinOpKind = "gt"
	BinOpGe  BinOpKind = "ge"
	BinOpShl BinOpKind = "shl"
	BinOpShr BinOpKind = "shr"
	BinOpAnd BinOpKind = "and"
	BinOpOr  BinOpKind = "or"
)

// Binop represents an SSA binary operation instruction (add, sub, etc).
type Binop struct {
	Loc      lexer.Location
	Op       BinOpKind
	Lhs, Rhs *Val
	Ret      *Val
}

func NewBinop(loc lexer.Location, op BinOpKind, ret, lhs, rhs *Val) *Binop {
	return &Binop{Loc: loc, Op: op, Lhs: lhs, Rhs: rhs, Ret: ret}
}

func (b *Binop) isInstruction() {}

func (b *Binop) Accept(visitor Visitor) string {
	return visitor.VisitBinop(b)
}

func (b *Binop) Location() lexer.Location {
	return b.Loc
}

type Arg struct {
	Loc  lexer.Location
	Type ArgType
	Val  *Val
}

func NewArgRegular(loc lexer.Location, val *Val) Arg {
	return Arg{Loc: loc, Type: ArgRegular, Val: val}
}

func NewArgEnv(loc lexer.Location, val *Val) Arg {
	return Arg{Loc: loc, Type: ArgEnv, Val: val}
}

func NewArgVariadic(loc lexer.Location) Arg {
	return Arg{Loc: loc, Type: ArgVariadic}
}

type ArgType string

const (
	ArgRegular  ArgType = "regular"
	ArgEnv      ArgType = "env"
	ArgVariadic ArgType = "variadic"
)

type Jmp struct {
	Loc   lexer.Location
	Label string
}

func NewJmp(loc lexer.Location, label string) *Jmp {
	return &Jmp{Loc: loc, Label: label}
}

func (j *Jmp) isInstruction() {}

func (j *Jmp) Accept(visitor Visitor) string {
	return visitor.VisitJmp(j)
}

func (j *Jmp) Location() lexer.Location {
	return j.Loc
}

type Jnz struct {
	Loc   lexer.Location
	Cond  *Val
	True  string
	False string
}

func NewJnz(loc lexer.Location, cond *Val, trueLabel, falseLabel string) *Jnz {
	return &Jnz{Loc: loc, Cond: cond, True: trueLabel, False: falseLabel}
}

func (j *Jnz) isInstruction() {}

func (j *Jnz) Accept(visitor Visitor) string {
	return visitor.VisitJnz(j)
}

func (j *Jnz) Location() lexer.Location {
	return j.Loc
}

// Load represents a load from memory (e.g., x = p^)
type Load struct {
	Loc  lexer.Location
	Ret  *Val // destination (SSA temp)
	Addr *Val // address to load from
}

func NewLoad(loc lexer.Location, ret, addr *Val) *Load {
	return &Load{Loc: loc, Ret: ret, Addr: addr}
}

func (l *Load) isInstruction() {}

func (l *Load) Accept(visitor Visitor) string {
	return visitor.VisitLoad(l)
}

func (l *Load) Location() lexer.Location {
	return l.Loc
}

// Store represents a store to memory (e.g., p^ = x)
type Store struct {
	Loc  lexer.Location
	Addr *Val // address to store to
	Val  *Val // value to store
}

func NewStore(loc lexer.Location, addr, val *Val) *Store {
	return &Store{Loc: loc, Addr: addr, Val: val}
}

func (s *Store) isInstruction() {}

func (s *Store) Accept(visitor Visitor) string {
	return visitor.VisitStore(s)
}

func (s *Store) Location() lexer.Location {
	return s.Loc
}

// Convert represents a type conversion instruction (e.g., int to float)
type Convert struct {
	Loc lexer.Location
	Ret *Val // destination (SSA temp)
	Val *Val // value to convert
}

func NewConvert(loc lexer.Location, ret, val *Val) *Convert {
	return &Convert{Loc: loc, Ret: ret, Val: val}
}

func (c *Convert) isInstruction() {}

func (c *Convert) Accept(visitor Visitor) string {
	return visitor.VisitConvert(c)
}

func (c *Convert) Location() lexer.Location {
	return c.Loc
}

// Alloc represents stack allocation (e.g., for arrays or structs)
type Alloc struct {
	Loc  lexer.Location
	Ret  *Val // destination (SSA temp)
	Size *Val // size in bytes (word or long)
}

func NewAlloc(loc lexer.Location, ret, size *Val) *Alloc {
	return &Alloc{Loc: loc, Ret: ret, Size: size}
}

func (a *Alloc) isInstruction() {}

func (a *Alloc) Accept(visitor Visitor) string {
	return visitor.VisitAlloc(a)
}

func (a *Alloc) Location() lexer.Location {
	return a.Loc
}
