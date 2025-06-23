package parser

import (
	"errors"
	"fmt"
	"io"
	"maps"
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
		// Check for optional attributes before parameter
		var attrs ast.Attributes

		nextTok, err := p.expectType(lexer.TypeRparen, lexer.TypeAt, lexer.TypeIdent)
		if err != nil {
			return err
		}

		if nextTok.Type == lexer.TypeRparen {
			break
		}

		if nextTok.Type == lexer.TypeAt {
			// Parse parameter attributes
			if err := p.parseAttributes(nextTok); err != nil {
				return err
			}

			// Copy and clear parser attributes for this param
			attrs = maps.Clone(p.attributes)
			clear(p.attributes)

			// Now expect identifier
			nextTok, err = p.expectType(lexer.TypeIdent)
			if err != nil {
				return err
			}
		}

		if _, err := p.expectType(lexer.TypeColon); err != nil {
			return err
		}

		equal, err := p.peekType(lexer.TypeEquals)
		if err != nil {
			return err
		}

		argType := lexer.Token{}

		if equal.Type != lexer.TypeEquals {
			argType, err = p.expectKeyword(lexer.KeywordInt, lexer.KeywordString)
			if err != nil {
				return err
			}

			equal, err = p.peekType(lexer.TypeEquals)
			if err != nil {
				return err
			}
		}

		var value ast.Expression

		if equal.Type == lexer.TypeEquals {
			// If we have an equals sign, we expect a default value
			defaultValue, err := p.expectType(lexer.TypeNumber, lexer.TypeString)
			if err != nil {
				return err
			}

			switch defaultValue.Type {
			case lexer.TypeNumber:
				value = ast.NewIntLiteral(defaultValue.NumberVal)
			case lexer.TypeString:
				value = ast.NewStringLiteral(defaultValue.StringVal)
			}

			value, err = p.parseExpression(value)
			if err != nil {
				return err
			}
		}

		def.Params = append(def.Params, ast.FuncParam{
			Ident:      nextTok.StringVal,
			Type:       p.mapKeywordToType(argType.Keyword),
			Attributes: attrs,
			Value:      value,
		})

		tok, err := p.expectType(lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err
		}

		if tok.Type == lexer.TypeRparen {
			break
		}
	}

	arrow, err := p.peekType(lexer.TypeArrow)
	if err != nil {
		return err
	}

	retType := lexer.Token{
		Keyword: lexer.KeywordVoid,
	}

	if arrow.Type == lexer.TypeArrow {
		retType, err = p.expectKeyword(lexer.KeywordInt, lexer.KeywordString, lexer.KeywordVoid)
		if err != nil {
			return err
		}

		def.ReturnType = p.mapKeywordToType(retType.Keyword)
	}

	// If the function is not `extern`, we expect a body.
	if _, ok := def.Attributes["extern"]; !ok {
		lbrace, err := p.expectType(lexer.TypeLbrace)
		if err != nil {
			return err
		}

		instructions, err := p.parseBody(lbrace, retType)
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

func (p *Parser) parseBody(start, retType lexer.Token) ([]ast.Instruction, error) {
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
					instructions = append(instructions,
						ast.NewReturn())
				} else {
					ret, err := p.expectType(lexer.TypeString, lexer.TypeNumber, lexer.TypeIdent)
					if err != nil {
						return nil, err
					}

					switch ret.Type {
					case lexer.TypeNumber:
						if retType.Keyword != lexer.KeywordInt {
							return nil, fmt.Errorf("unexpected return type %s at %s, expected %s",
								ret.Type, ret.Location, retType.Keyword)
						}

						instructions = append(instructions,
							ast.NewReturn(ast.NewIntLiteral(ret.NumberVal)))
					default:
						// TODO(daniel): handle string and ident return types
						panic(fmt.Sprintf("unexpected return type %s at %s, expected number",
							ret.Type, ret.Location))
					}

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
	lhs, err := p.expectType(lexer.TypeNumber, lexer.TypeIdent)
	if err != nil {
		return nil, err
	}

	var val ast.Expression

	switch lhs.Type {
	case lexer.TypeNumber:
		val = ast.NewIntLiteral(lhs.NumberVal)
	case lexer.TypeIdent:
		val = ast.NewVariableRef(lhs.StringVal, ast.TypeUnknown)
	}

	val, err = p.parseExpression(val)
	if err != nil {
		return nil, err
	}

	return &ast.Assign{
		Ident: name.StringVal,
		Type:  returnType,
		Value: val,
	}, nil
}

func (p *Parser) parseCall(first lexer.Token) (ast.Instruction, error) {
	arg, err := p.nextToken()
	if err != nil {
		return nil, err
	}

	var args []ast.Arg

	for arg.Type != lexer.TypeRparen {
		switch arg.Type {
		case lexer.TypeString:
			expr, err := p.parseExpression(ast.NewStringLiteral(arg.StringVal))
			if err != nil {
				return nil, fmt.Errorf("error parsing expression at %s: %w", arg.Location, err)
			}

			args = append(args, ast.Arg{Value: expr})
		case lexer.TypeNumber:
			expr, err := p.parseExpression(ast.NewIntLiteral(arg.NumberVal))
			if err != nil {
				return nil, fmt.Errorf("error parsing expression at %s: %w", arg.Location, err)
			}

			args = append(args, ast.Arg{Value: expr})
		case lexer.TypeIdent:
			expr, err := p.parseExpression(ast.NewVariableRef(arg.StringVal, ast.TypeUnknown))
			if err != nil {
				return nil, fmt.Errorf("error parsing expression at %s: %w", arg.Location, err)
			}

			args = append(args, ast.Arg{Value: expr})
		default:
			return nil, fmt.Errorf("unexpected argument type %s at %s, expected string or number",
				arg.Type, arg.Location)
		}

		arg, err = p.expectType(lexer.TypeRparen, lexer.TypeComma)
		if err != nil {
			return nil, err
		}

		if arg.Type == lexer.TypeComma {
			arg, err = p.nextToken()
			if err != nil {
				return nil, err
			}
		}
	}

	return ast.NewCall(first.StringVal, args...), nil
}

func (p *Parser) parseExpression(start ast.Expression) (ast.Expression, error) {
	peek, err := p.peekType(lexer.TypePlus)
	if err != nil {
		return nil, err
	}

	switch peek.Type {
	case lexer.TypePlus:
		rhs, err := p.expectType(lexer.TypeNumber, lexer.TypeIdent)
		if err != nil {
			return nil, err
		}

		switch rhs.Type {
		case lexer.TypeNumber:
			return ast.NewBinop("+", start, ast.NewIntLiteral(rhs.NumberVal)), nil
		case lexer.TypeIdent:
			return ast.NewBinop("+", start, ast.NewVariableRef(rhs.StringVal, ast.TypeUnknown)), nil
		default:
			panic("unreachable")
		}
	default:
		return start, nil
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
