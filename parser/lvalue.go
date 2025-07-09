package parser

import (
	"fmt"

	"github.com/corani/cubit/ast"
	"github.com/corani/cubit/lexer"
)

// parseLValue parses an lvalue expression for assignment.
// Supports variable refs, derefs, and parenthesized/dereferenced expressions.
func (p *Parser) parseLValue() (ast.LValue, error) {
	// No need to save index here

	// Try to parse a parenthesized or deref expression
	first, err := p.nextToken()
	if err != nil {
		return nil, err // EOF
	}

	switch first.Type {
	case lexer.TypeIdent:
		// Could be a variable, or a deref (ident^), or a chain
		ident := first.StringVal
		lv := ast.LValue(ast.NewVariableRef(ident, ast.TypeUnknown, first.Location))

		next, err := p.peekType(lexer.TypeCaret)
		if err != nil {
			return nil, err // EOF
		}

		if next.Type == lexer.TypeCaret {
			// Deref: ident^
			lv = ast.NewDeref(lv, next.Location)
		}

		return lv, nil
	case lexer.TypeLparen:
		// Parenthesized lvalue, e.g. (a + 1)^
		expr, err := p.parseExpression(false)
		if err != nil {
			return nil, err
		}

		_, err = p.expectType(lexer.TypeRparen)
		if err != nil {
			return nil, err // EOF
		}

		next, err := p.peekType(lexer.TypeCaret)
		if err != nil {
			return nil, err // EOF
		}

		if next.Type == lexer.TypeCaret {
			// (expr)^
			return ast.NewDeref(expr, next.Location), nil
		}

		p.errorf(first.Location, "expected dereference after parenthesized expression")

		// error recovery:
		return ast.NewDeref(expr, next.Location), nil
	default:
		p.errorf(first.Location, "expected lvalue, got %s", first.StringVal)

		// TODO: error recovery
		return nil, fmt.Errorf("invalid lvalue start: %s", first.StringVal)
	}
}
