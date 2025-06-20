package codegen

import (
	"fmt"
	"strings"

	"github.com/corani/refactored-giggle/ast"
)

// SsaGen implements ast.SSAVisitor and generates SSA code.
type SsaGen struct{}

// NewSSAVisitor returns a new SSAVisitor.
func NewSSAVisitor() *SsaGen {
	return &SsaGen{}
}

func (v *SsaGen) VisitCompilationUnit(cu *ast.CompilationUnit) string {
	var sb strings.Builder
	for i := range cu.Types {
		sb.WriteString(cu.Types[i].Accept(v))
		sb.WriteString("\n")
	}
	for i := range cu.FuncDefs {
		sb.WriteString(cu.FuncDefs[i].Accept(v))
		sb.WriteString("\n")
	}
	for i := range cu.DataDefs {
		sb.WriteString(cu.DataDefs[i].Accept(v))
		sb.WriteString("\n")
	}
	return sb.String()
}

func (v *SsaGen) VisitTypeDef(td *ast.TypeDef) string {
	var align string
	if td.Align > 0 {
		align = fmt.Sprintf("align %d ", td.Align)
	}
	switch td.Type {
	case ast.TypeDefRegular:
		fields := make([]string, len(td.Fields))
		for i, field := range td.Fields {
			fields[i] = v.VisitSubTySize(field)
		}
		return fmt.Sprintf("type :%s = %s{ %s }", td.Ident, align, strings.Join(fields, ", "))
	case ast.TypeDefUnion:
		unionFields := make([]string, len(td.UnionFields))
		for i, unionField := range td.UnionFields {
			fields := make([]string, len(unionField))
			for j, field := range unionField {
				fields[j] = v.VisitSubTySize(field)
			}
			unionFields[i] = fmt.Sprintf("{ %s }", strings.Join(fields, ", "))
		}
		return fmt.Sprintf("type :%s = %s{ %s }", td.Ident, align, strings.Join(unionFields, ", "))
	case ast.TypeDefOpaque:
		return fmt.Sprintf("type :%s = %s{ %d }", td.Ident, align, td.OpaqueSize)
	default:
		panic("unknown type definition type: " + string(td.Type))
	}
}

func (v *SsaGen) VisitDataDef(dd *ast.DataDef) string {
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

func (v *SsaGen) VisitFuncDef(fd *ast.FuncDef) string {
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
		params[i] = v.VisitParam(param)
	}
	for i, block := range fd.Blocks {
		blocks[i] = v.VisitBlock(block)
	}
	return fmt.Sprintf("%sfunction %s$%s(%s) {%s}",
		linkage, retTy, fd.Ident,
		strings.Join(params, ", "),
		strings.Join(blocks, "\n"))
}

// --- Helper visitor methods for nested types ---
func (v *SsaGen) VisitSubTySize(sts ast.SubTySize) string {
	if sts.Size > 1 {
		return fmt.Sprintf("%s %d", v.VisitSubTy(sts.SubTy), sts.Size)
	}

	return v.VisitSubTy(sts.SubTy)
}

func (v *SsaGen) VisitSubTy(st ast.SubTy) string {
	switch st.Type {
	case ast.SubTyExt:
		return string(st.ExtTy)
	case ast.SubTyIdent:
		return fmt.Sprintf(":%s", st.Ident)
	default:
		panic("unknown subtype type: " + string(st.Type))
	}
}

func (v *SsaGen) VisitLinkage(l ast.Linkage) string {
	switch l.Type {
	case ast.LinkageExport, ast.LinkageThread:
		return string(l.Type)
	case ast.LinkageSection:
		if l.SecFlags == "" {
			return fmt.Sprintf("%s %q", l.Type, l.SecName)
		}

		return fmt.Sprintf("%s %q %q", l.Type, l.SecName, l.SecFlags)
	default:
		panic("unknown linkage type: " + string(l.Type))
	}
}

func (v *SsaGen) VisitDataInit(di ast.DataInit) string {
	switch di.Type {
	case ast.DataInitExt:
		items := make([]string, len(di.Items))

		for i, item := range di.Items {
			items[i] = v.VisitDataItem(item)
		}

		return fmt.Sprintf("%s %s", di.ExtTy, strings.Join(items, " "))
	case ast.DataInitZero:
		return fmt.Sprintf("z %d", di.Size)
	default:
		panic("unknown data initialization type: " + string(di.Type))
	}
}

func (v *SsaGen) VisitDataItem(di ast.DataItem) string {
	switch di.Type {
	case ast.DataItemSymbol:
		if di.Offset > 0 {
			return fmt.Sprintf("$%s + %d", di.Ident, di.Offset)
		}

		return fmt.Sprintf("$%s", di.Ident)
	case ast.DataItemString:
		return fmt.Sprintf("\"%s\"", di.StringVal)
	case ast.DataItemConst:
		return v.VisitConst(di.Const)
	default:
		panic("unknown data item type: " + string(di.Type))
	}
}

func (v *SsaGen) VisitConst(c ast.Const) string {
	switch c.Type {
	case ast.ConstInteger:
		return fmt.Sprintf("%d", c.I64)
	case ast.ConstSingle:
		return fmt.Sprintf("s_%f", c.F32)
	case ast.ConstDouble:
		return fmt.Sprintf("d_%f", c.F64)
	case ast.ConstIdent:
		return fmt.Sprintf("$%s", c.Ident)
	default:
		panic("unknown constant type: " + string(c.Type))
	}
}

func (v *SsaGen) VisitParam(p ast.Param) string {
	switch p.Type {
	case ast.ParamRegular:
		return fmt.Sprintf("%s %%%s", v.VisitAbiTy(p.AbiTy), p.Ident)
	case ast.ParamEnv:
		return fmt.Sprintf("env %%%s", p.Ident)
	case ast.ParamVariadic:
		return "..."
	default:
		panic("unknown parameter type: " + string(p.Type))
	}
}

func (v *SsaGen) VisitAbiTy(a ast.AbiTy) string {
	switch a.Type {
	case ast.AbiTyBase:
		return string(a.BaseTy)
	case ast.AbiTySubW:
		return string(a.SubWTy)
	case ast.AbiTyIdent:
		return fmt.Sprintf(":%s", a.Ident)
	default:
		panic("unknown ABI type: " + string(a.Type))
	}
}

func (v *SsaGen) VisitBlock(b ast.Block) string {
	var label string

	if b.Label != "" {
		label = fmt.Sprintf("@%s\n", b.Label)
	}

	instructions := make([]string, len(b.Instructions))

	for i, instr := range b.Instructions {
		instructions[i] = "\t" + v.VisitInstruction(instr)
	}

	return fmt.Sprintf("\n%s%s\n", label, strings.Join(instructions, "\n"))
}

func (v *SsaGen) VisitInstruction(instr ast.Instruction) string {
	switch i := instr.(type) {
	case ast.Ret:
		return v.VisitRet(i)
	case ast.Call:
		return v.VisitCall(i)
	case ast.Add:
		return v.VisitAdd(i)
	case ast.Instr:
		return v.VisitInstr(i)
	default:
		panic(fmt.Sprintf("unknown instruction type: %T", instr))
	}
}

func (v *SsaGen) VisitRet(r ast.Ret) string {
	if r.Val == nil {
		return "ret"
	}

	return fmt.Sprintf("ret %s", v.VisitVal(*r.Val))
}

func (v *SsaGen) VisitCall(c ast.Call) string {
	var lhs string

	if c.LHS != nil && c.RetTy != nil {
		lhs = fmt.Sprintf("%%%s = %s ", *c.LHS, v.VisitAbiTy(*c.RetTy))
	}

	args := make([]string, len(c.Args))

	for i, arg := range c.Args {
		args[i] = v.VisitArg(arg)
	}

	return fmt.Sprintf("%scall %s(%s)", lhs, v.VisitVal(c.Val), strings.Join(args, ", "))
}

func (v *SsaGen) VisitAdd(a ast.Add) string {
	return fmt.Sprintf("%s =w add %s, %s", v.VisitVal(a.Ret), v.VisitVal(a.Lhs), v.VisitVal(a.Rhs))
}

func (v *SsaGen) VisitInstr(i ast.Instr) string {
	return i.Str
}

func (v *SsaGen) VisitVal(val ast.Val) string {
	switch val.Type {
	case ast.ValDynConst:
		return v.VisitDynConst(val.DynConst)
	case ast.ValIdent:
		return fmt.Sprintf("%%%s", val.Ident)
	default:
		panic("unknown value type: " + string(val.Type))
	}
}

func (v *SsaGen) VisitDynConst(dc ast.DynConst) string {
	switch dc.Type {
	case ast.DynConstConst:
		return v.VisitConst(dc.Const)
	case ast.DynConstThread:
		return fmt.Sprintf("thread $%s", dc.Ident)
	default:
		panic("unknown dynamic constant type: " + string(dc.Type))
	}
}

func (v *SsaGen) VisitArg(a ast.Arg) string {
	switch a.Type {
	case ast.ArgRegular:
		return fmt.Sprintf("%s %s", v.VisitAbiTy(a.AbiTy), v.VisitVal(a.Val))
	case ast.ArgEnv:
		return fmt.Sprintf("env %s", v.VisitVal(a.Val))
	case ast.ArgVariadic:
		return "..."
	default:
		panic("unknown argument type: " + string(a.Type))
	}
}
