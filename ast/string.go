package ast

import (
	"fmt"
	"reflect"
	"strings"
)

func (cu CompilationUnit) String() string {
	s := NewStringer()
	cu.Accept(s)

	return s.String()
}

// Stringer is a visitor that formats the AST as a string representation.
type stringer struct {
	sb     strings.Builder
	indent int
}

func NewStringer() *stringer {
	return &stringer{}
}

func (s *stringer) String() string {
	return s.sb.String()
}

func (s *stringer) VisitCompilationUnit(cu *CompilationUnit) {
	s.writef("(unit %s\n", cu.Ident)
	s.writeIndented(func() {
		s.writef("\t%s\n\t(types", cu.Attributes)
		s.writeIndented(func() {
			for _, t := range cu.Types {
				t.Accept(s)
			}
		})
		s.write(")\n\t(data")
		s.writeIndented(func() {
			for _, d := range cu.Data {
				d.Accept(s)
			}
		})
		s.write(")\n\t(funcs")
		s.writeIndented(func() {
			for _, f := range cu.Funcs {
				f.Accept(s)
			}
		})
		s.write("\n\t)\n")
	})
	s.write(")\n")
}

func (s *stringer) VisitTypeDef(td *TypeDef) {
	s.writef("\n\t(type %s %s %s", td.Ident, td.Type, td.Attributes)
	s.writeOptional(td.Value, "<nil>")
	s.write(")")
}

func (s *stringer) VisitDataDef(dd *DataDef) {
	s.writef("\n\t(data %s %s %s ", dd.Ident, dd.Type, dd.Attributes)
	s.writeOptional(dd.Value, "<nil>")
	s.write(")")
}

func (s *stringer) VisitFuncDef(fd *FuncDef) {
	s.writef("\n\t(func %s %q\n", fd.ReturnType, fd.Ident)
	s.writeIndented(func() {
		s.writef("\t%s\n\t(params", fd.Attributes)
		if len(fd.Params) > 0 {
			s.writeIndented(func() {
				for _, param := range fd.Params {
					s.write("\n\t")
					param.Accept(s)
				}
			})
			s.write("\n\t")
		}
		s.write(")\n\t(body")
		s.writeOptional(fd.Body, "")
		s.write(")\n")
	})
	s.write("\t)")
}

func (s *stringer) VisitFuncParam(fp *FuncParam) {
	s.writef("(param %s %q ", fp.Type, fp.Ident)
	s.writeOptional(fp.Value, "<nil>")
	s.writef(" %s)", fp.Attributes)
}

func (s *stringer) VisitBody(b *Body) {
	s.writeIndented(func() {
		for _, instr := range b.Instructions {
			s.write("\n\t")
			instr.Accept(s)
		}
	})
}

func (s *stringer) VisitCall(c *Call) {
	s.writef("(call %s %q\n", c.Type, c.Ident)
	s.writeIndented(func() {
		s.write("\t(args\n")
		s.writeIndented(func() {
			for _, arg := range c.Args {
				s.writef("\t(arg %s %q ", arg.Type, arg.Ident)
				arg.Value.Accept(s)
				s.write(")\n")
			}
		})
		s.write("\t)\n")
	})
	s.write("\t)")
}

func (s *stringer) VisitDeclare(d *Declare) {
	s.writef("(declare %s %q)", d.Type, d.Ident)
}

func (s *stringer) VisitAssign(a *Assign) {
	s.writef("(assign %s ", a.Type)
	a.LHS.Accept(s)
	s.write(" ")
	a.Value.Accept(s)
	s.write(")")
}

func (s *stringer) VisitReturn(r *Return) {
	s.writef("(ret %s", r.Type)
	if r.Value != nil {
		s.write(" ")
		r.Value.Accept(s)
	}
	s.write(")")
}

func (s *stringer) VisitLiteral(l *Literal) {
	s.writef("(lit %s ", l.Type)
	switch l.Type.Kind {
	case TypeInt:
		s.writef("%d)", l.IntValue)
	case TypeString:
		s.writef("%q)", l.StringValue)
	case TypeBool:
		s.writef("%t)", l.BoolValue)
	default:
		s.write("unknown)")
	}
}

func (s *stringer) VisitBinop(b *Binop) {
	s.writef("(binop %s %q\n", b.Type, b.Operation)
	s.writeIndented(func() {
		s.write("\t")
		b.Lhs.Accept(s)
	})
	s.write("\n")
	s.writeIndented(func() {
		s.write("\t")
		b.Rhs.Accept(s)
	})
	s.write("\n\t)")
}

func (s *stringer) VisitVariableRef(v *VariableRef) {
	s.writef("(ref %s %q)", v.Type, v.Ident)
}

func (s *stringer) VisitDeref(d *Deref) {
	s.writef("(deref %s ", d.Type)
	d.Expr.Accept(s)
	s.write(")")
}

func (s *stringer) VisitIf(i *If) {
	s.write("(if\n")
	s.writeIndented(func() {
		s.write("\t")
		s.writeInstrList("init", i.Init, "; ")
		s.write("\t(cond ")
		i.Cond.Accept(s)
		s.write(")\n\t(then")
		i.Then.Accept(s)
		s.write("\n\t)\n\t(else")
		s.writeOptional(i.Else, "")
		s.write("\n\t)\n")
	})
	s.write("\t)")
}

func (s *stringer) VisitFor(f *For) {
	s.write("(for\n")
	s.writeIndented(func() {
		s.write("\t")
		s.writeInstrList("init", f.Init, "; ")
		s.write("\t(cond ")
		f.Cond.Accept(s)
		s.write(")\n\t")
		s.writeInstrList("post", f.Post, "; ")
		s.write("\t(body")
		f.Body.Accept(s)
		s.write("\n\t)\n")
	})
	s.write("\t)")
}

func (s *stringer) writeIndented(fn func()) {
	s.indent++
	fn()
	s.indent--
}

// write replaces any leading \t in the input string with the current indentation level.
func (s *stringer) write(text string) {
	text = strings.ReplaceAll(text, "\t", strings.Repeat("\t", s.indent))

	s.sb.WriteString(text)
}

func (s *stringer) writef(format string, args ...any) {
	s.write(fmt.Sprintf(format, args...))
}

func (s *stringer) writeOptional(node interface{ Accept(Visitor) }, orElse string) {
	if node != nil && !reflect.ValueOf(node).IsNil() {
		node.Accept(s)
	} else {
		s.write(orElse)
	}
}

func (s *stringer) writeInstrList(name string, list []Instruction, sep string) {
	s.writef("(%s", name)
	if len(list) > 0 {
		s.write(" ")
	}

	for i, instr := range list {
		if i > 0 {
			s.write(sep)
		}
		instr.Accept(s)
	}
	s.write(")\n")
}
