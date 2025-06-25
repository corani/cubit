package parser

import (
	"errors"
	"fmt"
	"io"
	"maps"
	"slices"
	"strings"

	"github.com/corani/refactored-giggle/ast"
	"github.com/corani/refactored-giggle/lexer"
)

type Parser struct {
	tok        []lexer.Token
	index      int
	unit       *ast.CompilationUnit
	attributes ast.Attributes
	localID    int
}

func New(tok []lexer.Token) *Parser {
	// TODO(daniel): instead of accepting all tokens, maybe we should accept a
	// lexer and pull in the tokens on demand.
	return &Parser{
		tok:        tok,
		index:      0,
		unit:       ast.NewCompilationUnit(),
		attributes: ast.Attributes{},
		localID:    0,
	}
}

func (p *Parser) Parse() (*ast.CompilationUnit, error) {
	for {
		start, err := p.expectType(lexer.TypeKeyword, lexer.TypeIdent, lexer.TypeAt)
		if err != nil {
			return p.unit, err
		}

		switch start.Type {
		case lexer.TypeAt:
			if err := p.parseAttributes(start); err != nil {
				return p.unit, err
			}
		case lexer.TypeKeyword:
			switch start.Keyword {
			case lexer.KeywordPackage:
				if err := p.parsePackage(start); err != nil {
					return p.unit, err
				}
			default:
				return p.unit, fmt.Errorf("expected package keyword at %s, got %s",
					start.Location, start.StringVal)
			}
		case lexer.TypeIdent:
			if p.unit.Ident == "" {
				return p.unit, fmt.Errorf("package must be defined before any other declarations at %s",
					start.Location)
			}

			if _, err := p.expectType(lexer.TypeColon); err != nil {
				return p.unit, err
			}

			// TODO(daniel): parse optional type.

			if _, err := p.expectType(lexer.TypeColon); err != nil {
				return p.unit, err
			}

			if _, err := p.expectKeyword(lexer.KeywordFunc); err != nil {
				return p.unit, err
			}

			if err := p.parseFunc(start); err != nil {
				return p.unit, err
			}
		}
	}
}

func (p *Parser) parsePackage(start lexer.Token) error {
	_ = start

	if p.unit.Ident != "" {
		return fmt.Errorf("package already defined at %s, cannot redefine",
			p.tok[p.index-1].Location)
	}

	pkgName, err := p.expectType(lexer.TypeIdent)
	if err != nil {
		return err
	}

	// Store any attributes collected before the package in the unit's Attributes
	p.unit.Attributes = maps.Clone(p.attributes)
	p.unit.Ident = pkgName.StringVal

	clear(p.attributes)

	return nil
}

func (p *Parser) parseAttributes(start lexer.Token) error {
	_ = start

	if _, err := p.expectType(lexer.TypeLparen); err != nil {
		return err
	}

	for {
		tok, err := p.expectType(lexer.TypeRparen, lexer.TypeIdent)
		if err != nil {
			return err
		}

		if tok.Type == lexer.TypeRparen {
			break
		}

		key := tok.StringVal

		validKey, err := ast.ParseAttrKey(key)
		if err != nil {
			return err
		}

		var value ast.AttrValue

		next, err := p.expectType(lexer.TypeEquals, lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err
		}

		if next.Type == lexer.TypeEquals {
			valTok, err := p.expectType(lexer.TypeString, lexer.TypeNumber)
			if err != nil {
				return err
			}

			switch valTok.Type {
			case lexer.TypeString:
				value = ast.AttrString(valTok.StringVal)
			case lexer.TypeNumber:
				value = ast.AttrInt(valTok.NumberVal)
			}

			next, err = p.expectType(lexer.TypeComma, lexer.TypeRparen)
			if err != nil {
				return err
			}
		}

		p.attributes[validKey] = value

		if next.Type == lexer.TypeRparen {
			break
		}
	}

	return nil
}

func (p *Parser) parseFunc(name lexer.Token) error {
	if _, err := p.expectType(lexer.TypeLparen); err != nil {
		return err
	}

	def := ast.NewFuncDef(name.StringVal, p.attributes)
	clear(p.attributes)

	for {
		param, err := p.parseFuncParam()
		if err != nil {
			return err
		}

		if param == nil {
			break
		}

		def.Params = append(def.Params, param)

		tok, err := p.expectType(lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err
		}

		if tok.Type == lexer.TypeRparen {
			break
		}
	}

	retType, err := p.parseFuncReturnType()
	if err != nil {
		return fmt.Errorf("error parsing return type at %s: %w", name.Location, err)
	}

	def.ReturnType = p.mapKeywordToType(retType.Keyword)

	// If the function is not `extern`, we expect a body.
	if _, ok := def.Attributes["extern"]; !ok {
		lbrace, err := p.expectType(lexer.TypeLbrace)
		if err != nil {
			return err
		}

		instructions, err := p.parseFuncBody(lbrace, *retType)
		if err != nil {
			return err
		}

		if _, err := p.expectType(lexer.TypeRbrace); err != nil {
			return err
		}

		def.Body = &ast.Body{
			Instructions: instructions,
		}
	}

	p.unit.Funcs = append(p.unit.Funcs, def)

	return nil
}

func (p *Parser) parseFuncParam() (*ast.FuncParam, error) {
	// Check for optional attributes before parameter
	var attrs ast.Attributes

	nextTok, err := p.expectType(lexer.TypeRparen, lexer.TypeAt, lexer.TypeIdent)
	if err != nil {
		return nil, err
	}

	if nextTok.Type == lexer.TypeRparen {
		return nil, nil
	}

	if nextTok.Type == lexer.TypeAt {
		// Parse parameter attributes
		if err := p.parseAttributes(nextTok); err != nil {
			return nil, err
		}

		// Copy and clear parser attributes for this param
		attrs = maps.Clone(p.attributes)
		clear(p.attributes)

		// Now expect identifier
		nextTok, err = p.expectType(lexer.TypeIdent)
		if err != nil {
			return nil, err
		}
	}

	if _, err := p.expectType(lexer.TypeColon); err != nil {
		return nil, err
	}

	equal, err := p.peekType(lexer.TypeEquals)
	if err != nil {
		return nil, err
	}

	argType := lexer.Token{}

	if equal.Type != lexer.TypeEquals {
		argType, err = p.expectKeyword(lexer.KeywordInt, lexer.KeywordString)
		if err != nil {
			return nil, err
		}

		equal, err = p.peekType(lexer.TypeEquals)
		if err != nil {
			return nil, err
		}
	}

	var value ast.Expression

	if equal.Type == lexer.TypeEquals {
		// If we have an equals sign, we expect a default value
		value, err = p.parseExpression(false)
		if err != nil {
			return nil, err
		}
	}

	return &ast.FuncParam{
		Ident:      nextTok.StringVal,
		Type:       p.mapKeywordToType(argType.Keyword),
		Attributes: attrs,
		Value:      value,
	}, nil
}

func (p *Parser) parseFuncReturnType() (*lexer.Token, error) {
	arrow, err := p.peekType(lexer.TypeArrow)
	if err != nil {
		return nil, err
	}

	retType := lexer.Token{
		Keyword: lexer.KeywordVoid,
	}

	if arrow.Type == lexer.TypeArrow {
		retType, err = p.expectKeyword(lexer.KeywordInt, lexer.KeywordString, lexer.KeywordVoid)
		if err != nil {
			return nil, err
		}
	}

	return &retType, nil
}

func (p *Parser) parseFuncBody(start, retType lexer.Token) ([]ast.Instruction, error) {
	if start.Type != lexer.TypeLbrace {
		return nil, fmt.Errorf("expected { at %s, got %s",
			start.Location, start.StringVal)
	}

	var instructions []ast.Instruction

	for {
		first, err := p.nextToken()
		if err != nil {
			return nil, err
		}

		switch first.Type {
		case lexer.TypeRbrace:
			p.index--
			addRet := false

			if len(instructions) == 0 {
				addRet = true
			} else {
				_, hasRet := instructions[len(instructions)-1].(*ast.Return)
				addRet = !hasRet
			}

			if addRet {
				switch retType.Keyword {
				case lexer.KeywordVoid:
					instructions = append(instructions, &ast.Return{})
				default:
					return nil, fmt.Errorf("expected return statement at %s", first.Location)
				}
			}

			return instructions, nil
		case lexer.TypeKeyword:
			switch first.Keyword {
			case lexer.KeywordReturn:
				if retType.Keyword == lexer.KeywordVoid {
					instructions = append(instructions, ast.NewReturn())
				} else {
					expr, err := p.parseExpression(false)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, ast.NewReturn(expr))
				}
			}
		case lexer.TypeIdent:
			token, err := p.nextToken()
			if err != nil {
				return nil, err
			}

			switch token.Type {
			case lexer.TypeLparen:
				inst, err := p.parseCall(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			case lexer.TypeColon:
				inst, err := p.parseAssign(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			default:
				return nil, fmt.Errorf("expected ( after identifier at %s, got %s",
					token.Location, token.StringVal)
			}
		}
	}
}

func (p *Parser) parseAssign(name lexer.Token) (ast.Instruction, error) {
	next, err := p.peekType(lexer.TypeEquals, lexer.TypeKeyword)
	if err != nil {
		return nil, err
	}

	returnType := ast.TypeUnknown

	// type
	if next.Type != lexer.TypeEquals {
		p.index--

		ty, err := p.expectKeyword(lexer.KeywordInt, lexer.KeywordString)
		if err != nil {
			return nil, err
		}

		if _, err := p.expectType(lexer.TypeEquals); err != nil {
			return nil, err
		}

		returnType = p.mapKeywordToType(ty.Keyword)
	}

	// value
	expr, err := p.parseExpression(false)
	if err != nil {
		return nil, err
	}

	return &ast.Assign{
		Ident: name.StringVal,
		Type:  returnType,
		Value: expr,
	}, nil
}

// parseCall parses the argument list of a function call. It expects `first` to be the identifier
// of the function being called. The left-parenthesis `(` should have already been consumed. It
// parses a comma-separated list of expressions until it encounters a right-parenthesis `)`.
func (p *Parser) parseCall(first lexer.Token) (*ast.Call, error) {
	var (
		args []ast.Arg
		next lexer.Token
	)

	for next.Type != lexer.TypeRparen {
		expr, err := p.parseExpression(true)
		if err != nil {
			return nil, err
		}

		if expr != nil {
			// We successfully parsed an expression, this should be followed by either
			// a comma or a right parenthesis.
			args = append(args, ast.Arg{Value: expr})

			next, err = p.expectType(lexer.TypeRparen, lexer.TypeComma)
			if err != nil {
				return nil, err
			}
		} else {
			// We didn't parse an expression, so we expect a right parenthesis to form `()`.
			next, err = p.expectType(lexer.TypeRparen)
			if err != nil {
				return nil, err
			}
		}
	}

	return ast.NewCall(first.StringVal, args...), nil
}

func (p *Parser) parseExpression(optional bool) (ast.Expression, error) {
	starters := []lexer.TokenType{lexer.TypeNumber, lexer.TypeString, lexer.TypeIdent}

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
	case lexer.TypeNumber:
		expr = ast.NewIntLiteral(start.NumberVal)
	case lexer.TypeString:
		expr = ast.NewStringLiteral(start.StringVal)
	case lexer.TypeIdent:
		// Peek to see if this is a function call (ident followed by Lparen)
		lparen, err := p.peekType(lexer.TypeLparen)
		if err != nil && !errors.Is(err, io.EOF) {
			return nil, err
		}

		if lparen.Type == lexer.TypeLparen {
			// It's a function call
			expr, err = p.parseCall(start)
			if err != nil {
				return nil, err
			}
		} else {
			expr = ast.NewVariableRef(start.StringVal, ast.TypeUnknown)
		}
	default:
		panic("unreachable")
	}

	// We currently only support addition as a binary operator. This needs to be refactored into
	// a proper recursive expression parser.
	peek, err := p.peekType(lexer.TypePlus)
	if err != nil {
		return nil, err
	}

	switch peek.Type {
	case lexer.TypePlus:
		rhs, err := p.parseExpression(false)
		if err != nil {
			return nil, err
		}

		return ast.NewBinop("+", expr, rhs), nil
	default:
		return expr, nil
	}
}

func (p *Parser) expectKeyword(kws ...lexer.Keyword) (lexer.Token, error) {
	token, err := p.expectType(lexer.TypeKeyword)
	if err != nil {
		return token, err
	}

	var kwnames []string

	for _, kw := range kws {
		kwnames = append(kwnames, string(kw))

		if token.Keyword == kw {
			return token, nil
		}
	}

	return token, fmt.Errorf("expected %s at %s, got %s",
		strings.Join(kwnames, " or "), token.Location, token.Keyword)
}

func (p *Parser) peekType(tts ...lexer.TokenType) (lexer.Token, error) {
	tok, err := p.expectType(tts...)

	if errors.Is(err, io.EOF) {
		return tok, err
	} else if err != nil {
		p.index-- // Rollback index if not EOF
	}

	return tok, nil
}

func (p *Parser) expectType(tts ...lexer.TokenType) (lexer.Token, error) {
	token, err := p.nextToken()
	if err != nil {
		return token, err
	}

	var ttnames []string

	for _, tt := range tts {
		ttnames = append(ttnames, string(tt))
		if token.Type == tt {
			return token, nil
		}
	}

	return token, fmt.Errorf("expected %s at %s, got %s",
		strings.Join(ttnames, " or "), token.Location, token.Type)
}

func (p *Parser) nextToken() (lexer.Token, error) {
	if p.index >= len(p.tok) {
		return lexer.Token{}, io.EOF
	}

	token := p.tok[p.index]
	p.index++

	return token, nil
}

func (p *Parser) mapKeywordToType(kw lexer.Keyword) ast.TypeKind {
	switch kw {
	case lexer.KeywordInt:
		return ast.TypeInt
	case lexer.KeywordString:
		return ast.TypeString
	case lexer.KeywordVoid:
		return ast.TypeVoid
	default:
		return ast.TypeUnknown
	}
}
