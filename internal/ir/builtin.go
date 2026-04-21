package ir

import "github.com/corani/cubit/internal/ast"

func (v *visitor) visitBuiltinCall(c *ast.Call) {
	c.Location().Errorf("unknown builtin function: %s", c.Ident)
}
