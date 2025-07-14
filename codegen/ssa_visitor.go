package codegen

import (
	"fmt"
	"strings"

	"github.com/corani/cubit/ir"
)

// SsaGen implements ast.Visitor and generates SSA code.
type SsaGen struct{}

// NewSSAVisitor returns a new SSAVisitor.
func NewSSAVisitor() *SsaGen {
	return &SsaGen{}
}

func (v *SsaGen) VisitCompilationUnit(cu *ir.CompilationUnit) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("# package %s (%s)\n", cu.Package, cu.Loc))

	for i := range cu.Types {
		sb.WriteString(cu.Types[i].Accept(v))
		sb.WriteString("\n")
	}

	for i := range cu.FuncDefs {
		// TODO(daniel): Skip functions with the extern attribute. There's probably a better way
		// to handle this, other than assuming that if a function has no blocks, it is extern.
		if cu.FuncDefs[i].Blocks == nil {
			continue
		}

		sb.WriteString(cu.FuncDefs[i].Accept(v))
		sb.WriteString("\n")
	}

	for i := range cu.DataDefs {
		sb.WriteString(cu.DataDefs[i].Accept(v))
		sb.WriteString("\n")
	}

	return sb.String()
}

func (v *SsaGen) VisitTypeDef(td *ir.TypeDef) string {
	var align string
	if td.Align > 0 {
		align = fmt.Sprintf("align %d ", td.Align)
	}
	switch td.Type {
	case ir.TypeDefRegular:
		fields := make([]string, len(td.Fields))
		for i, field := range td.Fields {
			fields[i] = v.VisitSubTySize(field)
		}
		return fmt.Sprintf("type :%s = %s{ %s }", td.Ident, align, strings.Join(fields, ", "))
	case ir.TypeDefUnion:
		unionFields := make([]string, len(td.UnionFields))
		for i, unionField := range td.UnionFields {
			fields := make([]string, len(unionField))
			for j, field := range unionField {
				fields[j] = v.VisitSubTySize(field)
			}
			unionFields[i] = fmt.Sprintf("{ %s }", strings.Join(fields, ", "))
		}
		return fmt.Sprintf("type :%s = %s{ %s }", td.Ident, align, strings.Join(unionFields, ", "))
	case ir.TypeDefOpaque:
		return fmt.Sprintf("type :%s = %s{ %d }", td.Ident, align, td.OpaqueSize)
	default:
		panic("unknown type definition type: " + string(td.Type))
	}
}

func (v *SsaGen) VisitDataDef(dd *ir.DataDef) string {
	var linkage, align string
	if dd.Linkage != nil {
		linkage = v.VisitLinkage(*dd.Linkage) + " "
	}
	if dd.Align > 0 {
		align = fmt.Sprintf("align %d ", dd.Align)
	}
	initializer := make([]string, len(dd.Initializer))
	for i, init := range dd.Initializer {
		initializer[i] = v.VisitDataInit(init)
	}
	return fmt.Sprintf("%sdata $%s = %s{ %s }", linkage, dd.Ident, align, strings.Join(initializer, ", "))
}

func (v *SsaGen) VisitFuncDef(fd *ir.FuncDef) string {
	var linkage, retTy string
	if fd.Linkage != nil {
		linkage = v.VisitLinkage(*fd.Linkage) + " "
	}
	if fd.RetTy != nil {
		retTy = v.VisitAbiTy(*fd.RetTy) + " "
	}
	params := make([]string, len(fd.Params))
	blocks := make([]string, len(fd.Blocks))
	for i, param := range fd.Params {
		params[i] = v.VisitParam(*param)
	}
	for i, block := range fd.Blocks {
		blocks[i] = v.VisitBlock(block)
	}
	return fmt.Sprintf("\n# %s\n%sfunction %s$%s(%s) {%s}",
		fd.Loc, linkage, retTy, fd.Ident,
		strings.Join(params, ", "),
		strings.Join(blocks, "\n"))
}

// --- Helper visitor methods for nested types ---
func (v *SsaGen) VisitSubTySize(sts ir.SubTySize) string {
	if sts.Size > 1 {
		return fmt.Sprintf("%s %d", v.VisitSubTy(sts.SubTy), sts.Size)
	}

	return v.VisitSubTy(sts.SubTy)
}

func (v *SsaGen) VisitSubTy(st ir.SubTy) string {
	switch st.Type {
	case ir.SubTyExt:
		return string(st.ExtTy)
	case ir.SubTyIdent:
		return fmt.Sprintf(":%s", st.Ident)
	default:
		panic("unknown subtype type: " + string(st.Type))
	}
}

func (v *SsaGen) VisitLinkage(l ir.Linkage) string {
	switch l.Type {
	case ir.LinkageExport, ir.LinkageThread:
		return string(l.Type)
	case ir.LinkageSection:
		if l.SecFlags == "" {
			return fmt.Sprintf("%s %q", l.Type, l.SecName)
		}

		return fmt.Sprintf("%s %q %q", l.Type, l.SecName, l.SecFlags)
	default:
		panic("unknown linkage type: " + string(l.Type))
	}
}

func (v *SsaGen) VisitDataInit(di ir.DataInit) string {
	switch di.Type {
	case ir.DataInitExt:
		items := make([]string, len(di.Items))

		for i, item := range di.Items {
			items[i] = v.VisitDataItem(item)
		}

		return fmt.Sprintf("%s %s", di.ExtTy, strings.Join(items, " "))
	case ir.DataInitZero:
		return fmt.Sprintf("z %d", di.Size)
	default:
		panic("unknown data initialization type: " + string(di.Type))
	}
}

func (v *SsaGen) VisitDataItem(di ir.DataItem) string {
	switch di.Type {
	case ir.DataItemSymbol:
		if di.Offset > 0 {
			return fmt.Sprintf("$%s + %d", di.Ident, di.Offset)
		}

		return fmt.Sprintf("$%s", di.Ident)
	case ir.DataItemString:
		return fmt.Sprintf("\"%s\"", di.StringVal)
	case ir.DataItemConst:
		return v.VisitConst(di.Const)
	default:
		panic("unknown data item type: " + string(di.Type))
	}
}

func (v *SsaGen) VisitConst(c ir.Const) string {
	switch c.Type {
	case ir.ConstInteger:
		return fmt.Sprintf("%d", c.I64)
	case ir.ConstSingle:
		return fmt.Sprintf("s_%f", c.F32)
	case ir.ConstDouble:
		return fmt.Sprintf("d_%f", c.F64)
	case ir.ConstIdent:
		return fmt.Sprintf("$%s", c.Ident)
	default:
		panic("unknown constant type: " + string(c.Type))
	}
}

func (v *SsaGen) VisitParam(p ir.Param) string {
	switch p.Type {
	case ir.ParamRegular:
		// TODO(daniel): generate correct parameter type.
		return fmt.Sprintf("%s %%%s", v.VisitAbiTy(p.AbiTy), p.Ident)
		// return fmt.Sprintf("l %%%s", p.Ident)
	case ir.ParamEnv:
		return fmt.Sprintf("env %%%s", p.Ident)
	case ir.ParamVariadic:
		return "..."
	default:
		panic("unknown parameter type: " + string(p.Type))
	}
}

func (v *SsaGen) VisitAbiTy(a ir.AbiTy) string {
	switch a.Type {
	case ir.AbiTyBase:
		return string(a.BaseTy)
	case ir.AbiTySubW:
		return string(a.SubWTy)
	case ir.AbiTyIdent:
		return fmt.Sprintf(":%s", a.Ident)
	default:
		panic("unknown ABI type: " + string(a.Type))
	}
}

func (v *SsaGen) VisitBlock(b ir.Block) string {
	var label string

	if b.Label != "" {
		label = fmt.Sprintf("@%s\n", b.Label)
	}

	instructions := make([]string, len(b.Instructions))

	for i, instr := range b.Instructions {
		// TODO(daniel): we need something better for indentation...
		if _, ok := instr.(*ir.Label); ok {
			instructions[i] = instr.Accept(v)
		} else {
			instructions[i] = "\t" + instr.Accept(v)
		}
	}

	return fmt.Sprintf("\n%s%s\n", label, strings.Join(instructions, "\n"))
}

func (v *SsaGen) VisitLabel(l *ir.Label) string {
	if l.Name == "" {
		return ""
	}

	return fmt.Sprintf("@%s", l.Name)
}

func (v *SsaGen) VisitRet(r *ir.Ret) string {
	if r.Val == nil {
		return "ret"
	}

	return fmt.Sprintf("ret %s", v.VisitVal(r.Val))
}

func (v *SsaGen) VisitCall(c *ir.Call) string {
	var lhs string

	if c.LHS != nil && c.RetTy != nil {
		lhs = fmt.Sprintf("%%%s =%s ", *c.LHS, v.VisitAbiTy(*c.RetTy))
	}

	args := make([]string, len(c.Args))

	for i, arg := range c.Args {
		args[i] = v.VisitArg(arg)
	}

	return fmt.Sprintf("%scall %s(%s)", lhs, v.VisitVal(c.Val), strings.Join(args, ", "))
}

func (v *SsaGen) VisitBinop(b *ir.Binop) string {
	// Map ir.BinOpKind to SSA op string
	opMap := map[ir.BinOpKind]string{
		ir.BinOpAdd: "add",
		ir.BinOpSub: "sub",
		ir.BinOpMul: "mul",
		ir.BinOpDiv: "div",
		ir.BinOpMod: "rem",
		ir.BinOpEq:  "ceqw",
		ir.BinOpNe:  "cnew",
		ir.BinOpLt:  "csltw",
		ir.BinOpLe:  "cslew",
		ir.BinOpGt:  "csgtw",
		ir.BinOpGe:  "csgew",
		ir.BinOpShl: "shl",
		ir.BinOpShr: "shr",
		ir.BinOpAnd: "and",
		ir.BinOpOr:  "or",
	}

	op, ok := opMap[b.Op]
	if !ok {
		panic("unknown binop: " + string(b.Op))
	}

	return fmt.Sprintf("%s =%s %s %s, %s",
		v.VisitVal(b.Ret), v.VisitAbiTy(b.Ret.AbiTy), op, v.VisitVal(b.Lhs), v.VisitVal(b.Rhs))
}

func (v *SsaGen) VisitVal(val *ir.Val) string {
	switch val.Type {
	case ir.ValDynConst:
		return v.VisitDynConst(val.DynConst)
	case ir.ValIdent:
		return fmt.Sprintf("%%%s", val.Ident)
	default:
		panic("unknown value type: " + string(val.Type))
	}
}

func (v *SsaGen) VisitDynConst(dc ir.DynConst) string {
	switch dc.Type {
	case ir.DynConstConst:
		return v.VisitConst(dc.Const)
	case ir.DynConstThread:
		return fmt.Sprintf("thread $%s", dc.Ident)
	default:
		panic("unknown dynamic constant type: " + string(dc.Type))
	}
}

func (v *SsaGen) VisitArg(a ir.Arg) string {
	switch a.Type {
	case ir.ArgRegular:
		return fmt.Sprintf("%s %s", v.VisitAbiTy(a.Val.AbiTy), v.VisitVal(a.Val))
	case ir.ArgEnv:
		return fmt.Sprintf("env %s", v.VisitVal(a.Val))
	case ir.ArgVariadic:
		return "..."
	default:
		panic("unknown argument type: " + string(a.Type))
	}
}

func (v *SsaGen) VisitJmp(j *ir.Jmp) string {
	if j.Label == "" {
		return "jmp"
	}

	return fmt.Sprintf("jmp @%s", j.Label)
}

func (v *SsaGen) VisitJnz(j *ir.Jnz) string {
	return fmt.Sprintf("jnz %s, @%s, @%s", v.VisitVal(j.Cond), j.True, j.False)
}

// Implements QBE-style load: %ret =w loadw %addr
func (v *SsaGen) VisitLoad(l *ir.Load) string {
	// QBE: %ret =<type> load<suffix> %addr
	ret := v.VisitVal(l.Ret)
	addr := v.VisitVal(l.Addr)
	typeStr := v.VisitAbiTy(l.Ret.AbiTy)

	// Determine the correct load instruction suffix based on the type
	loadInstr := "loadw" // default
	switch typeStr {
	case "w":
		loadInstr = "loadw"
	case "l":
		loadInstr = "loadl"
	case "s":
		loadInstr = "loads"
	case "d":
		loadInstr = "loadd"
	case "sb":
		loadInstr = "loadsb"
	case "ub":
		loadInstr = "loadub"
	case "sh":
		loadInstr = "loadsh"
	case "uh":
		loadInstr = "loaduh"
		// Add more as needed for other types
	}

	return fmt.Sprintf("%s =%s %s %s", ret, typeStr, loadInstr, addr)
}

// Implements QBE-style store: storew %val, %addr
func (v *SsaGen) VisitStore(s *ir.Store) string {
	// QBE: store<suffix> %val, %addr
	val := v.VisitVal(s.Val)
	addr := v.VisitVal(s.Addr)
	typeStr := v.VisitAbiTy(s.Val.AbiTy)

	storeInstr := "storew" // default
	switch typeStr {
	case "w":
		storeInstr = "storew"
	case "l":
		storeInstr = "storel"
	case "s":
		storeInstr = "stores"
	case "d":
		storeInstr = "stored"
	case "sb", "ub":
		storeInstr = "storeb"
	case "sh", "uh":
		storeInstr = "storeh"
		// Add more as needed for other types
	}

	return fmt.Sprintf("%s %s, %s", storeInstr, val, addr)
}

func (v *SsaGen) VisitConvert(c *ir.Convert) string {
	ret := v.VisitVal(c.Ret)
	val := v.VisitVal(c.Val)

	// TODO(daniel): generate correct type, for now we assume 'l' (long) for the result, and
	// 'sw' (signed word) for the input value.
	return fmt.Sprintf("%s =l extsw %s", ret, val)
}

func (v *SsaGen) VisitAlloc(a *ir.Alloc) string {
	// QBE: %ret =l allocN size, where N is the alignment, and result is always a pointer (long)
	ret := v.VisitVal(a.Ret)
	size := v.VisitVal(a.Size)
	align := 4 // default alignment

	// Determine alignment based on the AbiTy of the allocated type
	switch t := a.Ret.AbiTy; t.Type {
	case ir.AbiTyBase:
		switch t.BaseTy {
		case ir.BaseWord:
			align = 4
		case ir.BaseLong:
			align = 8
		case ir.BaseSingle:
			align = 4
		case ir.BaseDouble:
			align = 8
		}
	case ir.AbiTySubW:
		switch t.SubWTy {
		case "sb", "ub":
			align = 1
		case "sh", "uh":
			align = 2
		case "sw", "uw":
			align = 4
		}
		// Add more cases as needed for other types
	}

	return fmt.Sprintf("%s =l alloc%d %s", ret, align, size)
}
