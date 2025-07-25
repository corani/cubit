package parser

import (
	"fmt"
	"slices"

	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
)

// Pratt parser operator info
type opInfo struct {
	precedence int
	rightAssoc bool
	kind       ast.BinOpKind
}

var opPrecedence = map[lexer.TokenType]opInfo{
	lexer.TypePlus:    {precedence: 10, rightAssoc: false, kind: ast.BinOpAdd},
	lexer.TypeMinus:   {precedence: 10, rightAssoc: false, kind: ast.BinOpSub},
	lexer.TypeStar:    {precedence: 20, rightAssoc: false, kind: ast.BinOpMul},
	lexer.TypeSlash:   {precedence: 20, rightAssoc: false, kind: ast.BinOpDiv},
	lexer.TypePercent: {precedence: 20, rightAssoc: false, kind: ast.BinOpMod},
	lexer.TypeShl:     {precedence: 15, rightAssoc: false, kind: ast.BinOpShl},
	lexer.TypeShr:     {precedence: 15, rightAssoc: false, kind: ast.BinOpShr},
	lexer.TypeBinAnd:  {precedence: 8, rightAssoc: false, kind: ast.BinOpAnd},
	lexer.TypeBinOr:   {precedence: 6, rightAssoc: false, kind: ast.BinOpOr},
	lexer.TypeLogAnd:  {precedence: 4, rightAssoc: false, kind: ast.BinOpLogAnd},
	lexer.TypeLogOr:   {precedence: 3, rightAssoc: false, kind: ast.BinOpLogOr},
	lexer.TypeEq:      {precedence: 5, rightAssoc: false, kind: ast.BinOpEq},
	lexer.TypeNe:      {precedence: 5, rightAssoc: false, kind: ast.BinOpNe},
	lexer.TypeLt:      {precedence: 7, rightAssoc: false, kind: ast.BinOpLt},
	lexer.TypeLe:      {precedence: 7, rightAssoc: false, kind: ast.BinOpLe},
	lexer.TypeGt:      {precedence: 7, rightAssoc: false, kind: ast.BinOpGt},
	lexer.TypeGe:      {precedence: 7, rightAssoc: false, kind: ast.BinOpGe},
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
		lexer.TypeMinus, // allow unary minus as a primary
		lexer.TypeBang,  // allow logical not as a primary
		lexer.TypeNumber,
		lexer.TypeBool,
		lexer.TypeString,
		lexer.TypeIdent,
		lexer.TypeLparen,
		lexer.TypeKeyword,
		lexer.TypeLBracket, // allow array literal as a primary
	}

	start, err := p.peekType(starters...)
	if err != nil {
		return nil, err // EOF
	}

	if !slices.Contains(starters, start.Type) {
		// If the expression was optional and we didn't find a valid start token,
		// this is not an error, so we return `nil, nil`.
		if optional {
			return nil, nil
		}

		start.Location.Errorf("expected start of expression, got %s", start.StringVal)

		// TODO: error recovery
		return nil, nil
	}

	var expr ast.Expression

	switch start.Type {
	case lexer.TypeMinus:
		operand, err := p.parsePrimary(false)
		if err != nil {
			return nil, err
		}
		expr = ast.NewUnaryOp(ast.UnaryOpMinus, operand, start.Location)
	case lexer.TypeBang:
		operand, err := p.parsePrimary(false)
		if err != nil {
			return nil, err
		}

		// TODO(daniel): should we create a unary operator for logical not?
		// Translate !expr to expr == false
		expr = ast.NewBinop(ast.BinOpEq, operand,
			ast.NewBoolLiteral(false, start.Location), start.Location)
	case lexer.TypeKeyword:
		switch start.Keyword {
		case lexer.KeywordTrue:
			expr = ast.NewBoolLiteral(true, start.Location)
		case lexer.KeywordFalse:
			expr = ast.NewBoolLiteral(false, start.Location)
		default:
			start.Location.Errorf("unexpected keyword %s in expression", start.Keyword)

			// TODO: error recovery
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
			start.Location.Errorf("unexpected boolean keyword %s in expression", start.Keyword)

			// error recovery:
			expr = ast.NewBoolLiteral(false, start.Location)
		}
	case lexer.TypeString:
		expr = ast.NewStringLiteral(start.StringVal, start.Location)
	case lexer.TypeIdent:
		// Check for namespaced reference: Identifier . Identifier
		namespace := ""

		dot, err := p.peekType(lexer.TypeDot)
		if err != nil {
			return nil, err // EOF
		}

		if dot.Type == lexer.TypeDot {
			namespace = start.StringVal

			start, err = p.expectType(lexer.TypeIdent)
			if err != nil {
				return nil, err // EOF
			}
		}

		// Peek to see if this is a function call or dereference
		next, err := p.peekType(lexer.TypeLparen, lexer.TypeCaret, lexer.TypeLBracket)
		if err != nil {
			return nil, err // EOF
		}

		switch next.Type {
		case lexer.TypeLparen:
			// It's a function call
			expr, err = p.parseCall(namespace, start)
			if err != nil {
				return nil, err
			}
		case lexer.TypeCaret:
			expr = ast.NewVariableRef(namespace, start.StringVal, ast.TypeUnknown, start.Location)
			expr = ast.NewDeref(expr, next.Location)
		case lexer.TypeLBracket:
			size, err := p.parseExpression(false)
			if err != nil {
				return nil, err // EOF
			}
			if _, err := p.expectType(lexer.TypeRBracket); err != nil {
				return nil, err // EOF
			}
			expr = ast.NewVariableRef(namespace, start.StringVal, ast.TypeUnknown, start.Location)
			expr = ast.NewArrayIndex(expr, size, start.Location)
		default:
			expr = ast.NewVariableRef(namespace, start.StringVal, ast.TypeUnknown, start.Location)
		}
	case lexer.TypeLparen:
		// Parenthesized sub-expression
		expr, err = p.parseExpression(false)
		if err != nil {
			return nil, err
		}

		_, err = p.expectType(lexer.TypeRparen)
		if err != nil {
			return nil, err // EOF
		}

		// Check for dereference after parenthesized expression: (expr)^
		next, err := p.peekType(lexer.TypeCaret)
		if err != nil {
			return nil, err // EOF
		}

		if next.Type == lexer.TypeCaret {
			expr = ast.NewDeref(expr, next.Location)
		}
	case lexer.TypeLBracket:
		// Array literal: [size]type{...}
		sizeExpr, err := p.parseExpression(false)
		if err != nil {
			return nil, err
		}

		if _, err := p.expectType(lexer.TypeRBracket); err != nil {
			return nil, err // EOF
		}

		typeTok, err := p.expectType(lexer.TypeKeyword)
		if err != nil {
			return nil, err
		}

		var elemType *ast.Type

		switch typeTok.Keyword {
		case lexer.KeywordInt:
			elemType = ast.NewType(ast.TypeInt, typeTok.Location)
		case lexer.KeywordBool:
			elemType = ast.NewType(ast.TypeBool, typeTok.Location)
		case lexer.KeywordString:
			elemType = ast.NewType(ast.TypeString, typeTok.Location)
		case lexer.KeywordAny:
			elemType = ast.NewType(ast.TypeAny, typeTok.Location)
		default:
			typeTok.Location.Errorf("unexpected type %s in array literal", typeTok.StringVal)

			// error recovery:
			elemType = ast.NewType(ast.TypeUnknown, typeTok.Location)
		}

		if _, err := p.expectType(lexer.TypeLbrace); err != nil {
			return nil, err
		}

		// Parse elements: [size]type{1,2,3}
		var elements []ast.Literal
		for {
			tok, err := p.peekType(lexer.TypeRbrace)
			if err != nil {
				return nil, err // EOF
			}

			if tok.Type == lexer.TypeRbrace {
				break
			}

			elemExpr, err := p.parseExpression(false)
			if err != nil {
				return nil, err
			}

			// Only allow scalar literals for now
			lit, ok := elemExpr.(*ast.Literal)
			if !ok {
				tok.Location.Errorf("array literal elements must be literals")

				// error recovery: ignore element
			} else {
				elements = append(elements, *lit)
			}

			tok, err = p.peekType(lexer.TypeComma, lexer.TypeRbrace)
			if err != nil {
				return nil, err // EOF
			}

			if tok.Type == lexer.TypeComma {
				continue
			} else if tok.Type == lexer.TypeRbrace {
				break
			}
		}

		// Build the array type
		sizeLit, ok := sizeExpr.(*ast.Literal)
		if !ok || sizeLit.Type.Kind != ast.TypeInt {
			start.Location.Errorf("array size must be an integer literal")

			// error recovery
			sizeLit = ast.NewIntLiteral(0, start.Location)
		}

		arrType := ast.NewArrayType(elemType, ast.NewSizeLiteral(sizeLit.IntValue), start.Location)
		expr = ast.NewArrayLiteral(arrType, elements, start.Location)
	default:
		start.Location.Errorf("unexpected token %s in expression", start.StringVal)
	}

	return expr, nil
}
