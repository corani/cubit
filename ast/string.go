package ast

import (
	"fmt"
	"strings"
)

func (t TypeKind) String() string {
	switch t {
	case TypeInt:
		return "int"
	case TypeString:
		return "string"
	case TypeVoid:
		return "void"
	default:
		return "unknown"
	}
}

func (cu CompilationUnit) String() string {
	var types, data, funcs []string

	for _, t := range cu.Types {
		types = append(types, t.String())
	}

	for _, d := range cu.Data {
		data = append(data, d.String())
	}

	for _, f := range cu.Funcs {
		funcs = append(funcs, f.String())
	}

	return fmt.Sprintf("(unit %q (%s) (%s) (%s) %s)",
		cu.Ident,
		strings.Join(types, " "),
		strings.Join(data, " "),
		strings.Join(funcs, " "),
		cu.Attributes)
}

func (td TypeDef) String() string {
	return fmt.Sprintf("(type %q %s %s)", td.Ident, td.Type, td.Attributes)
}

func (dd DataDef) String() string {
	return fmt.Sprintf("(data %q %s %s)", dd.Ident, dd.Type, dd.Attributes)
}

func (fd FuncDef) String() string {
	var params []string

	for _, param := range fd.Params {
		params = append(params, param.String())
	}

	body := ""

	if fd.Body != nil {
		body = fd.Body.String()
	}

	return fmt.Sprintf("\n\t(func %q (%s) (%s) %s (%s))",
		fd.Ident, strings.Join(params, " "), fd.ReturnType, fd.Attributes, body)
}

func (fp FuncParam) String() string {
	value := "()"
	if fp.Value != nil {
		value = fp.Value.String()
	}

	return fmt.Sprintf("(param %q %s %s %s)", fp.Ident, fp.Type, value, fp.Attributes)
}

func (b *Body) String() string {
	var instructions []string

	for _, instr := range b.Instructions {
		instructions = append(instructions, instr.String())
	}

	return "\n\t\t" + strings.Join(instructions, "\n\t\t")
}

func (c *Call) String() string {
	var args []string

	for _, arg := range c.Args {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("(call %q (%s) %s)", c.Ident, strings.Join(args, " "), c.Type)
}

func (a Arg) String() string {
	if a.Ident != "" {
		return fmt.Sprintf("%s=%s", a.Ident, a.Value)
	}

	return a.Value.String()
}

func (a *Assign) String() string {
	return fmt.Sprintf("(assign %s %s %s)", a.Ident, a.Type, a.Value)
}

func (r *Return) String() string {
	if r.Value != nil {
		return fmt.Sprintf("(return %s)", r.Value)
	}
	return "(return)"
}

func (v *VariableRef) String() string {
	return fmt.Sprintf("(ref %s %s)", v.Ident, v.Type)
}

func (l *Literal) String() string {
	switch l.Type {
	case TypeInt:
		return fmt.Sprintf("(lit %d %v)", l.IntValue, l.Type)
	case TypeString:
		return fmt.Sprintf("(lit %q %v)", l.StringValue, l.Type)
	default:
		return "(lit unknown unknown)"
	}
}

func (b *Binop) String() string {
	return fmt.Sprintf("(call %q %s %s %s)", b.Operation, b.Type, b.Lhs, b.Rhs)
}
