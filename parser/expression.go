package parser

import (
	"errors"
	"fmt"
	"io"
	"slices"

	"github.com/corani/refactored-giggle/ast"
	"github.com/corani/refactored-giggle/lexer"
)

// Pratt parser operator info
type opInfo struct {
	precedence int
	rightAssoc bool
	kind       ast.BinOpKind
}

var opPrecedence = map[lexer.TokenType]opInfo{
	lexer.TypePlus:   {precedence: 10, rightAssoc: false, kind: ast.BinOpAdd},
	lexer.TypeMinus:  {precedence: 10, rightAssoc: false, kind: ast.BinOpSub},
	lexer.TypeStar:   {precedence: 20, rightAssoc: false, kind: ast.BinOpMul},
	lexer.TypeSlash:  {precedence: 20, rightAssoc: false, kind: ast.BinOpDiv},
	lexer.TypeShl:    {precedence: 15, rightAssoc: false, kind: ast.BinOpShl},
	lexer.TypeShr:    {precedence: 15, rightAssoc: false, kind: ast.BinOpShr},
	lexer.TypeBinAnd: {precedence: 8, rightAssoc: false, kind: ast.BinOpAnd},
	lexer.TypeBinOr:  {precedence: 6, rightAssoc: false, kind: ast.BinOpOr},
	lexer.TypeLogAnd: {precedence: 4, rightAssoc: false, kind: ast.BinOpLogAnd},
	lexer.TypeLogOr:  {precedence: 3, rightAssoc: false, kind: ast.BinOpLogOr},
	lexer.TypeEq:     {precedence: 5, rightAssoc: false, kind: ast.BinOpEq},
	lexer.TypeNe:     {precedence: 5, rightAssoc: false, kind: ast.BinOpNe},
	lexer.TypeLt:     {precedence: 7, rightAssoc: false, kind: ast.BinOpLt},
	lexer.TypeLe:     {precedence: 7, rightAssoc: false, kind: ast.BinOpLe},
	lexer.TypeGt:     {precedence: 7, rightAssoc: false, kind: ast.BinOpGt},
	lexer.TypeGe:     {precedence: 7, rightAssoc: false, kind: ast.BinOpGe},
}

func (p *Parser) parseExpression(optional bool) (ast.Expression, error) {
	return p.parseExpressionPratt(optional, 0)
}

func (p *Parser) parseExpressionPratt(optional bool, minPrec int) (ast.Expression, error) {
	lhs, err := p.parsePrimary(optional)
	if err != nil || lhs == nil {
		return lhs, err
	}

	// create a list containing all the binops in opPrecedence
	binops := make([]lexer.TokenType, 0, len(opPrecedence))
	for op := range opPrecedence {
		binops = append(binops, op)
	}

	for {
		peek, err := p.peekType(binops...)
		if err != nil || !slices.Contains(binops, peek.Type) {
			// If we hit EOF or a non-operator, just return lhs
			return lhs, nil
		}

		info, ok := opPrecedence[peek.Type]

		if !ok || info.precedence < minPrec {
			// If we *did* find a valid operator but it has lower precedence than the minimum
			// required, we roll back the index to re-parse this token higher up the stack.
			if ok {
				p.index--
			}

			// Not a valid operator or lower precedence, stop
			return lhs, nil
		}

		// Determine precedence for right-hand side
		nextMinPrec := info.precedence
		if !info.rightAssoc {
			nextMinPrec++
		}

		rhs, err := p.parseExpressionPratt(false, nextMinPrec)
		if err != nil {
			return nil, err
		}

		lhs = ast.NewBinop(info.kind, lhs, rhs, lhs.Location())
	}
}

func (p *Parser) parsePrimary(optional bool) (ast.Expression, error) {
	starters := []lexer.TokenType{
		lexer.TypeNumber,
		lexer.TypeBool,
		lexer.TypeString,
		lexer.TypeIdent,
		lexer.TypeLparen,
		lexer.TypeKeyword,
	}

	start, err := p.peekType(starters...)
	if err != nil {
		return nil, err
	}

	if !slices.Contains(starters, start.Type) {
		// If the expression was optional and we didn't find a valid start token,
		// this is not an error, so we return `nil, nil`.
		if optional {
			return nil, nil
		}

		return nil, fmt.Errorf("expected start of expression at %s, got %s",
			start.Location, start.StringVal)
	}

	var expr ast.Expression

	switch start.Type {
	case lexer.TypeKeyword:
		switch start.Keyword {
		case lexer.KeywordTrue:
			expr = ast.NewBoolLiteral(true, start.Location)
		case lexer.KeywordFalse:
			expr = ast.NewBoolLiteral(false, start.Location)
		default:
			return nil, fmt.Errorf("unexpected keyword %s at %s",
				start.Keyword, start.Location)
		}
	case lexer.TypeNumber:
		expr = ast.NewIntLiteral(start.NumberVal, start.Location)
	case lexer.TypeBool:
		if start.Keyword == lexer.KeywordTrue {
			expr = ast.NewBoolLiteral(true, start.Location)
		} else if start.Keyword == lexer.KeywordFalse {
			expr = ast.NewBoolLiteral(false, start.Location)
		} else {
			panic(fmt.Sprintf("unexpected boolean keyword %s at %s",
				start.Keyword, start.Location))
		}
	case lexer.TypeString:
		expr = ast.NewStringLiteral(start.StringVal, start.Location)
	case lexer.TypeIdent:
		// Peek to see if this is a function call or dereference
		next, err := p.peekType(lexer.TypeLparen, lexer.TypeCaret)
		if err != nil && !errors.Is(err, io.EOF) {
			return nil, err
		}

		switch next.Type {
		case lexer.TypeLparen:
			// It's a function call
			expr, err = p.parseCall(start)
			if err != nil {
				return nil, err
			}
		case lexer.TypeCaret:
			expr = ast.NewVariableRef(start.StringVal, ast.TypeUnknown, start.Location)
			expr = ast.NewDeref(expr, next.Location)
		default:
			expr = ast.NewVariableRef(start.StringVal, ast.TypeUnknown, start.Location)
		}
	case lexer.TypeLparen:
		// Parenthesized sub-expression
		expr, err = p.parseExpression(false)
		if err != nil {
			return nil, err
		}

		_, err = p.expectType(lexer.TypeRparen)
		if err != nil {
			return nil, err
		}

		// Check for dereference after parenthesized expression: (expr)^
		next, err := p.peekType(lexer.TypeCaret)
		if err != nil {
			return nil, err
		}

		if next.Type == lexer.TypeCaret {
			expr = ast.NewDeref(expr, next.Location)
		}
	default:
		panic("unreachable")
	}

	return expr, nil
}
