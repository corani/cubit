package ir

import "github.com/corani/cubit/internal/ast"

func (v *visitor) visitBuiltinCall(c *ast.Call) {
	switch c.Ident {
	case "len":
		v.visitBuiltinLen(c)
	default:
		c.Location().Errorf("unknown builtin function: %s", c.Ident)
	}
}

func (v *visitor) visitBuiltinLen(c *ast.Call) {
	if len(c.Args) != 1 {
		c.Location().Errorf("builtin 'len' expects 1 argument, got %d", len(c.Args))

		return
	}

	arg := c.Args[0]
	if arg.Type.Kind != ast.TypeArray {
		c.Location().Errorf("builtin 'len' expects an array, got %s", arg.Type)

		return
	}

	size := arg.Type.Size
	if size.Kind != ast.SizeLiteral {
		c.Location().Errorf("array size must be a literal, got %s", size)

		return
	}

	loc := c.Location()
	word := NewAbiTyBase(BaseWord)

	v.lastType = ast.NewType(ast.TypeInt, loc)
	v.lastVal = NewValIdent(loc, v.nextIdent("len"), word)

	v.appendInstruction(NewBinop(loc, BinOpAdd, v.lastVal,
		NewValInteger(loc, 0, word),
		NewValInteger(loc, int64(size.Value), word)))
}
