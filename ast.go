package main

import (
	"fmt"
	"strings"
)

type CompilationUnit struct {
	Types    []TypeDef
	DataDefs []DataDef
	FuncDefs []FuncDef
}

func NewCompilationUnit() CompilationUnit {
	return CompilationUnit{
		Types:    []TypeDef{},
		DataDefs: []DataDef{},
		FuncDefs: []FuncDef{},
	}
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

func (cu CompilationUnit) String() string {
	var sb strings.Builder

	for _, typ := range cu.Types {
		fmt.Fprintln(&sb, typ.String())
	}

	for _, funcDef := range cu.FuncDefs {
		fmt.Fprintln(&sb, funcDef.String())
	}

	for _, dataDef := range cu.DataDefs {
		fmt.Fprintln(&sb, dataDef.String())
	}

	return sb.String()
}

type Ident string

type BaseTy string

const (
	BaseWord   BaseTy = "w" // 32-bit integer
	BaseLong   BaseTy = "l" // 64-bit integer
	BaseSingle BaseTy = "s" // 32-bit floating point
	BaseDouble BaseTy = "d" // 64-bit floating point
)

type ExtTy string

const (
	ExtByte   = ExtTy("b")        // 8-bit integer
	ExtHalf   = ExtTy("h")        // 16-bit integer
	ExtWord   = ExtTy(BaseWord)   // 32-bit integer
	ExtLong   = ExtTy(BaseLong)   // 64-bit integer
	ExtSingle = ExtTy(BaseSingle) // 32-bit floating point
	ExtDouble = ExtTy(BaseDouble) // 64-bit floating point
)

type SubTy struct {
	Type  SubTyType
	ExtTy ExtTy
	Ident Ident
}

func (s SubTy) String() string {
	switch s.Type {
	case SubTyExt:
		return string(s.ExtTy)
	case SubTyIdent:
		return fmt.Sprintf(":%s", s.Ident)
	default:
		panic("unknown subtype type: " + string(s.Type))
	}
}

type SubTyType string

const (
	SubTyExt   SubTyType = "ext"   // Extended type
	SubTyIdent SubTyType = "ident" // Identifier
)

type SubTySize struct {
	SubTy SubTy
	Size  int
}

func NewSubTyExtSize(extTy ExtTy, size int) SubTySize {
	return SubTySize{
		SubTy: SubTy{
			Type:  SubTyExt,
			ExtTy: extTy,
		},
		Size: size,
	}
}

func NewSubTyIdentSize(ident Ident, size int) SubTySize {
	return SubTySize{
		SubTy: SubTy{
			Type:  SubTyIdent,
			Ident: ident,
		},
		Size: size,
	}
}

func (s SubTySize) String() string {
	if s.Size > 1 {
		return fmt.Sprintf("%s %d", s.SubTy, s.Size)
	}

	return s.SubTy.String()
}

type Const struct {
	Type  ConstType
	F32   float32
	F64   float64
	I64   int64
	Ident Ident
}

func NewConstInteger(i int64) Const {
	return Const{
		Type: ConstInteger,
		I64:  i,
	}
}

func NewConstSingle(f float32) Const {
	return Const{
		Type: ConstSingle,
		F32:  f,
	}
}

func NewConstDouble(f float64) Const {
	return Const{
		Type: ConstDouble,
		F64:  f,
	}
}

func NewConstIdent(ident Ident) Const {
	return Const{
		Type:  ConstIdent,
		Ident: ident,
	}
}

func (c Const) String() string {
	switch c.Type {
	case ConstInteger:
		return fmt.Sprintf("%d", c.I64)
	case ConstSingle:
		return fmt.Sprintf("s_%f", c.F32)
	case ConstDouble:
		return fmt.Sprintf("d_%f", c.F64)
	case ConstIdent:
		return fmt.Sprintf("$%s", c.Ident)
	default:
		panic("unknown constant type: " + string(c.Type))
	}
}

type ConstType string

const (
	ConstInteger ConstType = "integer"
	ConstSingle  ConstType = "single"
	ConstDouble  ConstType = "double"
	ConstIdent   ConstType = "ident"
)

type DynConst struct {
	Type  DynConstType
	Const Const
	Ident Ident
}

func NewDynConst(constv Const) DynConst {
	return DynConst{
		Type:  DynConstConst,
		Const: constv,
	}
}

func NewDynConstThread(ident Ident) DynConst {
	return DynConst{
		Type:  DynConstThread,
		Ident: ident,
	}
}

func (dc DynConst) String() string {
	switch dc.Type {
	case DynConstConst:
		return dc.Const.String()
	case DynConstThread:
		return fmt.Sprintf("thread $%s", dc.Ident)
	default:
		panic("unknown dynamic constant type: " + string(dc.Type))
	}
}

type DynConstType string

const (
	DynConstConst  DynConstType = "const"  // Compile-time constant
	DynConstThread DynConstType = "thread" // Thread-local symbol
)

type Val struct {
	Type     ValType
	DynConst DynConst
	Ident    Ident
}

func NewValDynConst(dc DynConst) Val {
	return Val{
		Type:     ValDynConst,
		DynConst: dc,
	}
}

func NewValGlobal(ident Ident) Val {
	return NewValDynConst(NewDynConst(NewConstIdent(ident)))
}

func NewValInteger(i int64) Val {
	return NewValDynConst(NewDynConst(NewConstInteger(i)))
}

func NewValIdent(ident Ident) Val {
	return Val{
		Type:  ValIdent,
		Ident: ident,
	}
}

func (v Val) String() string {
	switch v.Type {
	case ValDynConst:
		return v.DynConst.String()
	case ValIdent:
		return fmt.Sprintf("%%%s", v.Ident)
	default:
		panic("unknown value type: " + string(v.Type))
	}
}

type ValType string

const (
	ValDynConst ValType = "dynconst" // Dynamic constant
	ValIdent    ValType = "ident"    // Identifier
)

type Linkage struct {
	Type     LinkageType
	SecName  string
	SecFlags string
}

func NewLinkageExport() Linkage {
	return Linkage{
		Type: LinkageExport,
	}
}

func NewLinkageThread() Linkage {
	return Linkage{
		Type: LinkageThread,
	}
}

func NewLinkageSection(secName, secFlags string) Linkage {
	return Linkage{
		Type:     LinkageSection,
		SecName:  secName,
		SecFlags: secFlags,
	}
}

func (l Linkage) String() string {
	switch l.Type {
	case LinkageExport, LinkageThread:
		return string(l.Type)
	case LinkageSection:
		if l.SecFlags == "" {
			return fmt.Sprintf("%s %q", l.Type, l.SecName)
		}

		return fmt.Sprintf("%s %q %q", l.Type, l.SecName, l.SecFlags)
	default:
		panic("unknown linkage type: " + string(l.Type))
	}
}

type LinkageType string

const (
	LinkageExport  LinkageType = "export"
	LinkageThread  LinkageType = "thread"
	LinkageSection LinkageType = "section"
)

type TypeDef struct {
	Type        TypeDefType
	Ident       Ident
	Align       int
	Fields      []SubTySize
	UnionFields [][]SubTySize
	OpaqueSize  int
}

func NewTypeDefRegular(ident Ident, fields ...SubTySize) TypeDef {
	return TypeDef{
		Type:   TypeDefRegular,
		Ident:  ident,
		Fields: fields,
	}
}

func NewTypeDefUnion(ident Ident, unionFields ...[]SubTySize) TypeDef {
	return TypeDef{
		Type:        TypeDefUnion,
		Ident:       ident,
		UnionFields: unionFields,
	}
}

func NewTypeDefOpaque(ident Ident, opaqueSize int) TypeDef {
	return TypeDef{
		Type:       TypeDefOpaque,
		Ident:      ident,
		OpaqueSize: opaqueSize,
	}
}

func (td TypeDef) WithAlign(align int) TypeDef {
	td.Align = align

	return td
}

func (td TypeDef) String() string {
	align := ""

	if td.Align > 0 {
		align = fmt.Sprintf("align %d ", td.Align)
	}

	switch td.Type {
	case TypeDefRegular:
		fields := make([]string, len(td.Fields))

		for i, field := range td.Fields {
			fields[i] = field.String()
		}

		return fmt.Sprintf("type :%s = %s{ %s }", td.Ident, align, strings.Join(fields, ", "))
	case TypeDefUnion:
		unionFields := make([]string, len(td.UnionFields))

		for i, unionField := range td.UnionFields {
			fields := make([]string, len(unionField))

			for j, field := range unionField {
				fields[j] = field.String()
			}

			unionFields[i] = fmt.Sprintf("{ %s }", strings.Join(fields, ", "))
		}

		return fmt.Sprintf("type :%s = %s{ %s }", td.Ident, align, strings.Join(unionFields, ", "))
	case TypeDefOpaque:
		return fmt.Sprintf("type :%s = %s{ %d }", td.Ident, align, td.OpaqueSize)
	default:
		panic("unknown type definition type: " + string(td.Type))
	}
}

type TypeDefType string

const (
	TypeDefRegular TypeDefType = "regular"
	TypeDefUnion   TypeDefType = "union"
	TypeDefOpaque  TypeDefType = "opaque"
)

type DataDef struct {
	Linkage     *Linkage
	Ident       Ident
	Align       int
	Initializer []DataInit
}

func NewDataDef(ident Ident, initializer ...DataInit) DataDef {
	return DataDef{
		Ident:       ident,
		Initializer: initializer,
	}
}

func NewDataDefStringZ(ident Ident, val string) DataDef {
	return NewDataDef(ident,
		NewDataInitString(val),
		NewDataInitExt(ExtByte, NewDataItemInteger(0)),
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

func (dd DataDef) String() string {
	linkage := ""
	align := ""

	if dd.Linkage != nil {
		linkage = dd.Linkage.String() + " "
	}

	if dd.Align > 0 {
		align = fmt.Sprintf("align %d ", dd.Align)
	}

	initializer := make([]string, len(dd.Initializer))

	for i, init := range dd.Initializer {
		initializer[i] = init.String()
	}

	return fmt.Sprintf("%sdata $%s = %s{ %s }", linkage, dd.Ident, align, strings.Join(initializer, ", "))
}

type DataInit struct {
	Type  DataInitType
	ExtTy ExtTy
	Items []DataItem
	Size  int
}

func NewDataInitExt(extTy ExtTy, items ...DataItem) DataInit {
	return DataInit{
		Type:  DataInitExt,
		ExtTy: extTy,
		Items: items,
	}
}

func NewDataInitString(val string) DataInit {
	return DataInit{
		Type:  DataInitExt,
		ExtTy: ExtByte,
		Items: []DataItem{NewDataItemString(val)},
	}
}

func NewDataInitZero(size int) DataInit {
	return DataInit{
		Type: DataInitZero,
		Size: size,
	}
}

func (di DataInit) String() string {
	switch di.Type {
	case DataInitExt:
		items := make([]string, len(di.Items))

		for i, item := range di.Items {
			items[i] = item.String()
		}

		return fmt.Sprintf("%s %s", di.ExtTy, strings.Join(items, " "))
	case DataInitZero:
		return fmt.Sprintf("z %d", di.Size)
	default:
		panic("unknown data initialization type: " + string(di.Type))
	}
}

type DataInitType string

const (
	DataInitExt  DataInitType = "ext"  // Extended type
	DataInitZero DataInitType = "zero" // Zero-initialized
)

type DataItem struct {
	Type      DataItemType
	Ident     Ident
	Offset    int
	StringVal string
	Const     Const
}

func NewDataItemConst(c Const) DataItem {
	return DataItem{
		Type:  DataItemConst,
		Const: c,
	}
}

func NewDataItemString(val string) DataItem {
	return DataItem{
		Type:      DataItemString,
		StringVal: val,
	}
}

func NewDataItemInteger(i int64) DataItem {
	return NewDataItemConst(NewConstInteger(i))
}

func NewDataItemSymbol(ident Ident, offset int) DataItem {
	return DataItem{
		Type:   DataItemSymbol,
		Ident:  ident,
		Offset: offset,
	}
}

func (di DataItem) String() string {
	switch di.Type {
	case DataItemSymbol:
		if di.Offset > 0 {
			return fmt.Sprintf("$%s + %d", di.Ident, di.Offset)
		}

		return fmt.Sprintf("$%s", di.Ident)
	case DataItemString:
		return fmt.Sprintf("\"%s\"", di.StringVal)
	case DataItemConst:
		return di.Const.String()
	default:
		panic("unknown data item type: " + string(di.Type))
	}
}

type DataItemType string

const (
	DataItemSymbol DataItemType = "symbol"
	DataItemString DataItemType = "string"
	DataItemConst  DataItemType = "const"
)

type FuncDef struct {
	Linkage *Linkage
	RetTy   *AbiTy
	Ident   Ident
	Params  []Param
	Blocks  []Block
}

func NewFuncDef(ident Ident, params ...Param) FuncDef {
	return FuncDef{
		Ident:  ident,
		Params: params,
	}
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

func (fd FuncDef) String() string {
	linkage := ""

	if fd.Linkage != nil {
		linkage = fd.Linkage.String() + " "
	}

	retTy := ""

	if fd.RetTy != nil {
		retTy = fd.RetTy.String() + " "
	}

	params := make([]string, len(fd.Params))

	for i, param := range fd.Params {
		params[i] = param.String()
	}

	blocks := make([]string, len(fd.Blocks))

	for i, block := range fd.Blocks {
		blocks[i] = block.String()
	}

	return fmt.Sprintf("%sfunction %s$%s(%s) {%s}",
		linkage, retTy, fd.Ident,
		strings.Join(params, ", "),
		strings.Join(blocks, "\n"))
}

type Param struct {
	Type  ParamType
	AbiTy AbiTy
	Ident Ident
}

func NewParamRegular(abiTy AbiTy, ident Ident) Param {
	return Param{
		Type:  ParamRegular,
		AbiTy: abiTy,
		Ident: ident,
	}
}

func NewParamEnv(ident Ident) Param {
	return Param{
		Type:  ParamEnv,
		Ident: ident,
	}
}

func NewParamVariadic() Param {
	return Param{
		Type: ParamVariadic,
	}
}

func (p Param) String() string {
	switch p.Type {
	case ParamRegular:
		return fmt.Sprintf("%s %%%s", p.AbiTy, p.Ident)
	case ParamEnv:
		return fmt.Sprintf("env %%%s", p.Ident)
	case ParamVariadic:
		return "..."
	default:
		panic("unknown parameter type: " + string(p.Type))
	}
}

type ParamType string

const (
	ParamRegular  ParamType = "regular"  // Regular parameter
	ParamEnv      ParamType = "env"      // Environment parameter (first)
	ParamVariadic ParamType = "variadic" // Variadic marker (last)
)

type AbiTy struct {
	Type   AbiTyType
	BaseTy BaseTy
	SubWTy SubWTy
	Ident  Ident
}

func NewAbiTyBase(baseTy BaseTy) AbiTy {
	return AbiTy{
		Type:   AbiTyBase,
		BaseTy: baseTy,
	}
}

func NewAbiTySubW(subWTy SubWTy) AbiTy {
	return AbiTy{
		Type:   AbiTySubW,
		SubWTy: subWTy,
	}
}

func NewAbiTyIdent(ident Ident) AbiTy {
	return AbiTy{
		Type:  AbiTyIdent,
		Ident: ident,
	}
}

func (a AbiTy) String() string {
	switch a.Type {
	case AbiTyBase:
		return string(a.BaseTy)
	case AbiTySubW:
		return string(a.SubWTy)
	case AbiTyIdent:
		return fmt.Sprintf(":%s", a.Ident)
	default:
		panic("unknown ABI type: " + string(a.Type))
	}
}

type AbiTyType string

const (
	AbiTyBase  AbiTyType = "base"  // Base type (e.g., word, long)
	AbiTySubW  AbiTyType = "subw"  // Sub-word type (e.g., signed byte, unsigned halfword)
	AbiTyIdent AbiTyType = "ident" // Identifier for custom types
)

type SubWTy string

const (
	SubWSB SubWTy = "sb" // signed byte
	SubWUB SubWTy = "ub" // unsigned byte
	SubWSH SubWTy = "sh" // signed halfword
	SubWUH SubWTy = "uh" // unsigned halfword
)

type Block struct {
	Label        string
	Instructions []Instruction
}

func (b Block) String() string {
	label := ""

	if b.Label != "" {
		label = fmt.Sprintf("@%s\n", b.Label)
	}

	instructions := make([]string, len(b.Instructions))

	for i, instr := range b.Instructions {
		instructions[i] = "\t" + instr.String()
	}

	return fmt.Sprintf("\n%s%s\n", label, strings.Join(instructions, "\n"))
}

type Instruction interface {
	String() string
}

type Ret struct {
	Val *Val
}

func NewRet(val ...Val) Ret {
	if len(val) > 1 {
		panic("NewRet accepts at most one value")
	}

	if len(val) == 0 {
		return Ret{}
	}

	return Ret{Val: &val[0]}
}

func (r Ret) String() string {
	if r.Val == nil {
		return "ret"
	}

	return fmt.Sprintf("ret %s", r.Val.String())
}

type Call struct {
	LHS   *Ident
	RetTy *AbiTy
	Val   Val
	Args  []Arg
}

func NewCall(val Val, args ...Arg) Call {
	return Call{
		Val:  val,
		Args: args,
	}
}

func (c Call) WithRet(lhs Ident, retTy AbiTy) Call {
	c.LHS = &lhs
	c.RetTy = &retTy

	return c
}

func (c Call) String() string {
	lhs := ""

	if c.LHS != nil && c.RetTy != nil {
		lhs = fmt.Sprintf("%%%s = %s ", *c.LHS, c.RetTy)
	}

	args := make([]string, len(c.Args))

	for i, arg := range c.Args {
		args[i] = arg.String()
	}

	return fmt.Sprintf("%scall %s(%s)", lhs, c.Val, strings.Join(args, ", "))
}

type Arg struct {
	Type  ArgType
	AbiTy AbiTy
	Val   Val
}

func NewArgRegular(abiTy AbiTy, val Val) Arg {
	return Arg{
		Type:  ArgRegular,
		AbiTy: abiTy,
		Val:   val,
	}
}

func NewArgEnv(val Val) Arg {
	return Arg{
		Type: ArgEnv,
		Val:  val,
	}
}

func NewArgVariadic() Arg {
	return Arg{
		Type: ArgVariadic,
	}
}

func (a Arg) String() string {
	switch a.Type {
	case ArgRegular:
		return fmt.Sprintf("%s %s", a.AbiTy.String(), a.Val.String())
	case ArgEnv:
		return fmt.Sprintf("env %s", a.Val.String())
	case ArgVariadic:
		return "..."
	default:
		panic("unknown argument type: " + string(a.Type))
	}
}

type ArgType string

const (
	ArgRegular  ArgType = "regular"  // Regular argument
	ArgEnv      ArgType = "env"      // Environment argument (first)
	ArgVariadic ArgType = "variadic" // Variadic marker
)
