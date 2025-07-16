package parser

import (
	"fmt"

	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
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

		next, err := p.peekType(lexer.TypeCaret, lexer.TypeLBracket)
		if err != nil {
			return nil, err // EOF
		}

		switch next.Type {
		case lexer.TypeCaret:
			// Deref: ident^
			lv = ast.NewDeref(lv, next.Location)
		case lexer.TypeLBracket:
			// Array index: ident[expr]
			index, err := p.parseExpression(false)
			if err != nil {
				return nil, err // EOF
			}
			_, err = p.expectType(lexer.TypeRBracket)
			if err != nil {
				return nil, err // EOF
			}
			lv = ast.NewArrayIndex(lv, index, next.Location)
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

		first.Location.Errorf("expected dereference after parenthesized expression")

		// error recovery:
		return ast.NewDeref(expr, next.Location), nil
	default:
		first.Location.Errorf("expected lvalue, got %s", first.StringVal)

		// TODO: error recovery
		return nil, fmt.Errorf("invalid lvalue start: %s", first.StringVal)
	}
}
