package parser

import (
	"fmt"
	"io"
	"maps"
	"strings"

	"github.com/corani/cubit/ast"
	"github.com/corani/cubit/lexer"
)

type Parser struct {
	tok            []lexer.Token
	index          int
	unit           *ast.CompilationUnit
	attributes     ast.Attributes
	localID        int
	currentRetType *ast.Type
	errors         []error
}

func New(tok []lexer.Token) *Parser {
	var location lexer.Location

	// Initial location, we'll update this to the location of 'package' later.
	if len(tok) > 0 {
		location = tok[0].Location
	}

	// TODO(daniel): instead of accepting all tokens, maybe we should accept a
	// lexer and pull in the tokens on demand.
	return &Parser{
		tok:            tok,
		index:          0,
		unit:           ast.NewCompilationUnit(location),
		attributes:     ast.Attributes{},
		localID:        0,
		currentRetType: nil,
	}
}

func (p *Parser) Parse() (*ast.CompilationUnit, error) {
	for {
		start, err := p.expectType(lexer.TypeKeyword, lexer.TypeIdent, lexer.TypeAt)
		if err != nil {
			return p.unit, err // EOF
		}

		switch start.Type {
		case lexer.TypeAt:
			if err := p.parseAttributes(start); err != nil {
				return p.unit, err // EOF
			}
		case lexer.TypeKeyword:
			switch start.Keyword {
			case lexer.KeywordPackage:
				if err := p.parsePackage(start); err != nil {
					return p.unit, err // EOF
				}
			default:
				start.Location.Errorf("expected package keyword, got %s",
					start.StringVal)

				// TODO: error recovery
				return p.unit, fmt.Errorf("expected package keyword at %s, got %s",
					start.Location, start.StringVal)
			}
		case lexer.TypeIdent:
			if p.unit.Ident == "" {
				start.Location.Errorf("package must be defined before any other declarations")

				// error recovery: just continue parsing
			}

			if _, err := p.expectType(lexer.TypeColon); err != nil {
				return p.unit, err // EOF
			}

			// TODO(daniel): parse optional type.

			if _, err := p.expectType(lexer.TypeColon); err != nil {
				return p.unit, err // EOF
			}

			if _, err := p.expectKeyword(lexer.KeywordFunc); err != nil {
				return p.unit, err // EOF
			}

			if err := p.parseFunc(start); err != nil {
				return p.unit, err
			}
		}
	}
}

// parsePackage parses a package declaration.
// It returns io.EOF when there are no more tokens.
func (p *Parser) parsePackage(start lexer.Token) error {
	if p.unit.Ident != "" {
		start.Location.Errorf("package already defined, cannot redefine")
		p.unit.Loc.Infof("previous definition was here")

		// error recovery: just ignore the new package definition.
		_, err := p.expectType(lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}
	} else {
		pkgName, err := p.expectType(lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}

		// Store any attributes collected before the package in the unit's Attributes
		p.unit.Attributes = maps.Clone(p.attributes)
		p.unit.Ident = pkgName.StringVal
		p.unit.Loc = start.Location
	}

	if _, err := p.expectType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	clear(p.attributes)

	return nil
}

// parseAttributes parses attributes in the form `@(...)`.
// It returns io.EOF when there are no more tokens.
func (p *Parser) parseAttributes(atToken lexer.Token) error {
	_ = atToken

	lparen, err := p.expectType(lexer.TypeLparen)
	if err != nil {
		return err // EOF
	}

	if lparen.Type != lexer.TypeLparen {
		lparen.Location.Errorf("expected ( after @, got %s", lparen.StringVal)

		// TODO: error recovery
	}

	for {
		tok, err := p.expectType(lexer.TypeRparen, lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}

		if tok.Type == lexer.TypeRparen {
			break
		}

		key, ok := ast.ParseAttrKey(tok.StringVal)
		if !ok {
			tok.Location.Errorf("invalid attribute key: %s", tok.StringVal)
		}

		value := ast.AttrValue(ast.AttrBool(true))

		next, err := p.expectType(lexer.TypeAssign, lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err // EOF
		}

		if next.Type == lexer.TypeAssign {
			valTok, err := p.expectType(lexer.TypeString, lexer.TypeNumber)
			if err != nil {
				return err // EOF
			}

			switch valTok.Type {
			case lexer.TypeString:
				value = ast.AttrString(valTok.StringVal)
			case lexer.TypeNumber:
				value = ast.AttrInt(valTok.NumberVal)
			}

			next, err = p.expectType(lexer.TypeComma, lexer.TypeRparen)
			if err != nil {
				return err // EOF
			}
		}

		// ignore invalid attributes
		if ok {
			p.attributes[key] = value
		}

		if next.Type == lexer.TypeRparen {
			break
		}
	}

	// parse optional semicolon
	if _, err := p.expectType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	return nil
}

func (p *Parser) parseFunc(name lexer.Token) error {
	if _, err := p.expectType(lexer.TypeLparen); err != nil {
		return err // EOF
	}

	def := ast.NewFuncDef(name.StringVal, p.attributes, name.Location)
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
			return err // EOF
		}

		if tok.Type == lexer.TypeRparen {
			break
		}
	}

	retType, err := p.parseFuncReturnType()
	if err != nil {
		name.Location.Errorf("error parsing return type: %v", err)

		// error recovery:
		retType = ast.NewType(ast.TypeVoid, name.Location)
	}

	p.currentRetType = retType
	def.ReturnType = retType

	if def.Attributes.Has(ast.AttrKeyExtern) || def.Attributes.Has(ast.AttrKeyBuiltin) {
		// If the function is extern or builtin, we don't parse the body.
	} else {
		lbrace, err := p.expectType(lexer.TypeLbrace)
		if err != nil {
			return err // EOF
		}

		instructions, err := p.parseBlock(lbrace)
		if err != nil {
			return err
		}

		// Add implicit return if needed
		addRet := false
		if len(instructions) == 0 {
			addRet = true
		} else {
			_, hasRet := instructions[len(instructions)-1].(*ast.Return)
			addRet = !hasRet
		}

		if addRet {
			switch retType.Kind {
			case ast.TypeVoid:
				// If the return type is void, we can just add an empty return.
				instructions = append(instructions, ast.NewReturn(lbrace.Location, retType))
			default:
				name.Location.Errorf("function %s has return type %s but no return statement",
					def.Ident, retType.String())

				// error recovery:
				instructions = append(instructions, ast.NewReturn(lbrace.Location, retType))
			}
		}

		if _, err := p.expectType(lexer.TypeRbrace); err != nil {
			return err // EOF
		}

		def.Body = ast.NewBody(instructions, lbrace.Location)
	}

	p.unit.Funcs = append(p.unit.Funcs, def)

	if _, err := p.peekType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	return nil
}

func (p *Parser) parseFuncParam() (*ast.FuncParam, error) {
	// Check for optional attributes before parameter
	var attrs ast.Attributes

	nextTok, err := p.expectType(lexer.TypeRparen, lexer.TypeAt, lexer.TypeIdent)
	if err != nil {
		return nil, err // EOF
	}

	if nextTok.Type == lexer.TypeRparen {
		// Empty parameter list.
		return nil, nil
	}

	if nextTok.Type == lexer.TypeAt {
		// Parse parameter attributes
		if err := p.parseAttributes(nextTok); err != nil {
			return nil, err // EOF
		}

		// Copy and clear parser attributes for this param
		attrs = maps.Clone(p.attributes)
		clear(p.attributes)

		// Now expect identifier
		nextTok, err = p.expectType(lexer.TypeIdent)
		if err != nil {
			return nil, err // EOF
		}
	}

	if _, err := p.expectType(lexer.TypeColon); err != nil {
		return nil, err // EOF
	}

	equal, err := p.peekType(lexer.TypeAssign)
	if err != nil {
		return nil, err // EOF
	}

	var paramType *ast.Type

	if equal.Type != lexer.TypeAssign {
		paramType = p.parseType()

		equal, err = p.peekType(lexer.TypeAssign)
		if err != nil {
			return nil, err // EOF
		}
	} else {
		paramType = ast.NewType(ast.TypeUnknown, equal.Location)
	}

	var value ast.Expression

	if equal.Type == lexer.TypeAssign {
		// If we have an equals sign, we expect a default value
		value, err = p.parseExpression(false)
		if err != nil {
			return nil, err
		}
	}

	return ast.NewFuncParam(nextTok.StringVal, paramType, value,
		attrs, nextTok.Location), nil
}

// parseFuncReturnType parses the return type of a function.
// If the return type is not specified, it defaults to void.
// It returns io.EOF when there are no more tokens.
func (p *Parser) parseFuncReturnType() (*ast.Type, error) {
	arrow, err := p.peekType(lexer.TypeArrow)
	if err != nil {
		return nil, err
	}

	if arrow.Type == lexer.TypeArrow {
		return p.parseType(), nil
	}

	// Default to void
	return ast.NewType(ast.TypeVoid, arrow.Location), nil
}

func (p *Parser) parseBlock(start lexer.Token) ([]ast.Instruction, error) {
	_ = start

	var instructions []ast.Instruction

	for {
		first, err := p.nextToken()
		if err != nil {
			return nil, err // EOF
		}

		switch first.Type {
		case lexer.TypeRbrace:
			// End of block
			p.index--
			return instructions, nil
		case lexer.TypeSemicolon:
			// Empty statement, just continue
			continue
		case lexer.TypeKeyword:
			switch first.Keyword {
			case lexer.KeywordReturn:
				inst, err := p.parseReturn(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			case lexer.KeywordIf:
				inst, err := p.parseIf(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			case lexer.KeywordFor:
				inst, err := p.parseFor(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			}
		case lexer.TypeIdent, lexer.TypeLparen:
			// Try to parse a declaration (ident : ...)
			if first.Type == lexer.TypeIdent {
				next, err := p.peekType(lexer.TypeColon)
				if err != nil {
					return nil, err // EOF
				}

				if next.Type == lexer.TypeColon {
					instr, err := p.parseDeclare(first)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, instr...)

					continue
				}
			}

			// Otherwise, try to parse an lvalue expression followed by '='
			p.index-- // Unconsume first token

			lvalueExpr, err := p.parseLValue()
			if err == nil {
				next, err := p.peekType(lexer.TypeAssign)
				if err != nil {
					return nil, err // EOF
				}

				if next.Type == lexer.TypeAssign {
					instr, err := p.parseAssign(lvalueExpr)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, instr...)

					continue
				}
			}

			// If not assignment, try to parse as a function call (ident(...))
			if first.Type == lexer.TypeIdent {
				next, err := p.peekType(lexer.TypeLparen)
				if err != nil {
					return nil, err // EOF
				}

				if next.Type == lexer.TypeLparen {
					inst, err := p.parseCall(first)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, inst)

					continue
				}
			}

			first.Location.Errorf("expected statement, got %s", first.StringVal)

			// TODO: error recovery
			return nil, fmt.Errorf("unexpected statement at %s", first.Location)
		}
	}
}

// parseType parses a type, supporting arbitrary nesting of arrays and pointers
// (e.g., [N]^int, ^[N]int, [N][M]^int, etc.)
func (p *Parser) parseType() *ast.Type {
	typeModifier := []func(*ast.Type) *ast.Type{}

	for {
		// Pointer(s)
		if tok, err := p.peekType(lexer.TypeCaret); err == nil && tok.Type == lexer.TypeCaret {
			loc := tok.Location // TODO(daniel): I think this is not needed?

			typeModifier = append(typeModifier, func(inner *ast.Type) *ast.Type {
				return ast.NewPointerType(inner, 1, loc)
			})

			continue
		}

		// Array(s)
		if tok, err := p.peekType(lexer.TypeLBracket); err == nil && tok.Type == lexer.TypeLBracket {
			sizeTok, err := p.expectType(lexer.TypeNumber)
			if err != nil {
				tok.Location.Errorf("expected array size after '['")
				sizeTok.NumberVal = 0
			}

			if _, err := p.expectType(lexer.TypeRBracket); err != nil {
				tok.Location.Errorf("expected ']' after array size")
			}

			loc := tok.Location // TODO(daniel): I think this is not needed?
			size := sizeTok.NumberVal
			typeModifier = append(typeModifier, func(inner *ast.Type) *ast.Type {
				return ast.NewArrayType(inner, ast.NewSizeLiteral(size), loc)
			})

			continue
		}

		break
	}

	base := p.parseBaseType()

	// Apply modifiers in reverse order (so the base type is wrapped by the last modifier parsed)
	for i := len(typeModifier) - 1; i >= 0; i-- {
		base = typeModifier[i](base)
	}

	return base
}

// parseBaseType parses the base type (int, bool, string, void, etc.)
func (p *Parser) parseBaseType() *ast.Type {
	tok, err := p.expectType(lexer.TypeKeyword)
	if err != nil {
		tok.Location.Errorf("expected type keyword, got %s", tok.Type)

		// error recover:
		tok = lexer.Token{
			Type:     lexer.TypeKeyword,
			Keyword:  lexer.KeywordVoid,
			Location: tok.Location, // FIXME(daniel): pass in the default location?
		}
	}

	switch tok.Keyword {
	case lexer.KeywordInt:
		return ast.NewType(ast.TypeInt, tok.Location)
	case lexer.KeywordString:
		return ast.NewType(ast.TypeString, tok.Location)
	case lexer.KeywordBool:
		return ast.NewType(ast.TypeBool, tok.Location)
	case lexer.KeywordVoid:
		return ast.NewType(ast.TypeVoid, tok.Location)
	default:
		tok.Location.Errorf("unexpected type keyword %s", tok.Keyword)

		// error recovery:
		return ast.NewType(ast.TypeVoid, tok.Location)
	}
}

// expectKeyword checks if the next token is one of the expected keywords.
// If it is, it consumes and returns the token. Otherwise it returns the first
// expected keyword as a fallback and records an error.
// It returns io.EOF when there are no more tokens.
func (p *Parser) expectKeyword(kws ...lexer.Keyword) (lexer.Token, error) {
	token, err := p.nextToken()
	if err != nil {
		return token, err
	}

	if token.Type != lexer.TypeKeyword {
		token.Location.Errorf("expected keyword, got %s", token.Type)

		// error recovery:
		return lexer.Token{
			Type:       lexer.TypeKeyword,
			Keyword:    kws[0], // Return the first keyword as a fallback
			Identifier: string(kws[0]),
			StringVal:  string(kws[0]),
			Location:   token.Location, // Use the current token's location
		}, nil
	}

	var kwnames []string

	for _, kw := range kws {
		kwnames = append(kwnames, string(kw))

		if token.Keyword == kw {
			return token, nil
		}
	}

	token.Location.Errorf("expected %s, got %s", strings.Join(kwnames, " or "), token.Keyword)

	// error recovery:
	return lexer.Token{
		Type:       lexer.TypeKeyword,
		Keyword:    kws[0], // Return the first keyword as a fallback
		Identifier: string(kws[0]),
		StringVal:  string(kws[0]),
		Location:   token.Location, // Use the current token's location
	}, nil
}

// peekType checks if the next token is of the expected type(s). If it is, it
// consumes and returns the token. Otherwise it doesn't consume the token.
// It returns io.EOF when there are no more tokens.
func (p *Parser) peekType(tts ...lexer.TokenType) (lexer.Token, error) {
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

	// If we reach here, the token was not of the expected type(s)
	p.index--

	return token, nil
}

// expectType checks if the next token is of the expected type(s). If it is, it
// consumes and returns the token. Otherwise it returns the first expected type
// as a fallback and records an error.
// It returns io.EOF when there are no more tokens.
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

	token.Location.Errorf("expected %s, got %s", strings.Join(ttnames, " or "), token.Type)

	// error recover:
	p.index--

	return lexer.Token{
		Type:     tts[0],
		Location: token.Location,
	}, nil
}

// nextToken retrieves the next token from the parser's token stream.
// It returns io.EOF when there are no more tokens.
func (p *Parser) nextToken() (lexer.Token, error) {
	if p.index >= len(p.tok) {
		return lexer.Token{}, io.EOF
	}

	token := p.tok[p.index]
	p.index++

	return token, nil
}
