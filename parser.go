package main

import (
	"errors"
	"fmt"
	"io"
	"strings"
)

type parser struct {
	tok        []Token
	index      int
	unit       *CompilationUnit
	blocks     []Block
	attributes map[string]string
	pkgName    string
}

func NewParser(tok []Token) *parser {
	return &parser{
		tok:        tok,
		index:      0,
		unit:       new(CompilationUnit),
		blocks:     nil,
		attributes: make(map[string]string),
		pkgName:    "",
	}
}

func (p *parser) Parse() (*CompilationUnit, error) {
	for {
		start, err := p.expectType(TypeKeyword, TypeIdent, TypeAt)
		if err != nil {
			return p.unit, err
		}

		switch start.Type {
		case TypeAt:
			if err := p.parseAttributes(start); err != nil {
				return p.unit, err
			}
		case TypeKeyword:
			switch start.Keyword {
			case KeywordPackage:
				if err := p.parsePackage(start); err != nil {
					return p.unit, err
				}
			default:
				return p.unit, fmt.Errorf("expected package keyword at %s, got %s",
					start.Location, start.StringVal)
			}
		case TypeIdent:
			if p.pkgName == "" {
				return p.unit, fmt.Errorf("package must be defined before any other declarations at %s",
					start.Location)
			}

			if _, err := p.expectType(TypeColon); err != nil {
				return p.unit, err
			}

			// TODO(daniel): parse optional type.

			if _, err := p.expectType(TypeColon); err != nil {
				return p.unit, err
			}

			if _, err := p.expectKeyword(KeywordFunc); err != nil {
				return p.unit, err
			}

			if err := p.parseFunc(start); err != nil {
				return p.unit, err
			}
		}
	}
}

func (p *parser) parsePackage(start Token) error {
	_ = start

	if p.pkgName != "" {
		return fmt.Errorf("package already defined at %s, cannot redefine",
			p.tok[p.index-1].Location)
	}

	// Expect package name
	pkgName, err := p.expectType(TypeIdent)
	if err != nil {
		return err
	}

	p.pkgName = pkgName.StringVal

	return nil
}

func (p *parser) parseAttributes(start Token) error {
	_ = start

	// Expect opening parenthesis
	_, err := p.expectType(TypeLparen)
	if err != nil {
		return err
	}

	// Parse attributes until closing parenthesis
	for {
		tok, err := p.expectType(TypeRparen, TypeIdent)
		if err != nil {
			return err
		}
		if tok.Type == TypeRparen {
			break
		}

		// Attribute key (string)
		key := tok.StringVal
		value := ""

		// Check for '='
		next, err := p.expectType(TypeEquals, TypeComma, TypeRparen)
		if err != nil {
			return err
		}

		if next.Type == TypeEquals {
			// Value must be a quoted string
			valTok, err := p.expectType(TypeString)
			if err != nil {
				return err
			}

			value = valTok.StringVal

			// After value, expect comma or rparen
			next, err = p.expectType(TypeComma, TypeRparen)
			if err != nil {
				return err
			}
		}

		p.attributes[key] = value

		if next.Type == TypeRparen {
			break
		}
		// else, next.Type == TypeComma, continue
	}

	return nil
}

func (p *parser) parseFunc(name Token) error {
	defer func() {
		clear(p.attributes)
	}()

	if _, err := p.expectType(TypeLparen); err != nil {
		return err
	}

	var params []Param

	// Parse parameters
	for {
		arg, err := p.expectType(TypeRparen, TypeIdent)
		if err != nil {
			return err
		}

		if arg.Type == TypeRparen {
			break
		}

		if _, err := p.expectType(TypeColon); err != nil {
			return err
		}

		argType, err := p.expectKeyword(KeywordInt, KeywordString)
		if err != nil {
			return err
		}

		switch argType.Keyword {
		case KeywordInt:
			params = append(params, NewParamRegular(NewAbiTyBase(BaseWord), Ident(arg.StringVal)))
		case KeywordString:
			params = append(params, NewParamRegular(NewAbiTyBase(BaseLong), Ident(arg.StringVal)))
		}

		tok, err := p.expectType(TypeComma, TypeRparen)
		if err != nil {
			return err
		}

		if tok.Type == TypeRparen {
			break
		}
	}

	arrow, err := p.peekType(TypeArrow)
	if err != nil {
		return err
	}

	retType := Token{
		Keyword: KeywordVoid,
	}

	if arrow.Type == TypeArrow {
		// read the return type
		retType, err = p.expectKeyword(KeywordInt, KeywordString, KeywordVoid)
		if err != nil {
			return err
		}
	}

	if _, ok := p.attributes["extern"]; ok {
		// no body
		return nil
	} else {
		lbrace, err := p.expectType(TypeLbrace)
		if err != nil {
			return err
		}

		if err := p.parseBody(lbrace, retType); err != nil {
			return err
		}

		_, err = p.expectType(TypeRbrace)

		fn := NewFuncDef(Ident(name.StringVal), params...).WithBlocks(p.blocks...)

		if _, ok := p.attributes["export"]; ok {
			fn = fn.WithLinkage(NewLinkageExport())
		}

		if retType.Keyword == KeywordInt {
			fn = fn.WithRetTy(NewAbiTyBase(BaseWord))
		}

		p.unit.WithFuncDefs(fn)

		return err
	}
}

func (p *parser) parseBody(start, retType Token) error {
	if start.Type != TypeLbrace {
		return fmt.Errorf("expected { at %s, got %s",
			start.Location, start.StringVal)
	}

	block := Block{Label: "start"}

	for {
		first, err := p.nextToken()
		if err != nil {
			return err
		}

		switch first.Type {
		case TypeRbrace:
			p.index--

			addRet := false

			if len(block.Instructions) == 0 {
				addRet = true
			} else {
				_, hasRet := block.Instructions[len(block.Instructions)-1].(Ret)
				addRet = !hasRet
			}

			if addRet {
				switch retType.Keyword {
				case KeywordVoid:
					block.Instructions = append(block.Instructions, NewRet())
				default:
					return fmt.Errorf("expected return statement at %s", first.Location)
				}
			}

			p.blocks = []Block{block}

			return nil
		case TypeKeyword:
			switch first.Keyword {
			case KeywordReturn:
				// parse return value
				ret, err := p.expectType(TypeString, TypeNumber, TypeIdent)
				if err != nil {
					return err
				}

				var val Val

				switch ret.Type {
				case TypeNumber:
					val = NewValInteger(int64(ret.NumberVal))
				default:
					panic(fmt.Sprintf("unexpected return type %s at %s, expected number",
						ret.Type, ret.Location))
				}

				block.Instructions = append(block.Instructions, NewRet(val))
			}
		case TypeIdent:
			// Check if it's a function call
			token, err := p.nextToken()
			if err != nil {
				return err
			}

			if token.Type != TypeLparen {
				return fmt.Errorf("expected ( after identifier at %s, got %s",
					token.Location, token.StringVal)
			}

			arg, err := p.nextToken()
			if err != nil {
				return err
			}

			var args []Arg

			// Read function arguments
			for arg.Type != TypeRparen {
				switch arg.Type {
				case TypeString:
					id := fmt.Sprintf("data_%s%d", first.StringVal, len(args))
					p.unit.WithDataDefs(NewDataDefStringZ(Ident(id), arg.StringVal))
					args = append(args, NewArgRegular(NewAbiTyBase(BaseLong), NewValGlobal(Ident(id))))
				case TypeNumber:
					args = append(args, NewArgRegular(NewAbiTyBase(BaseWord), NewValInteger(int64(arg.NumberVal))))
				case TypeIdent:
					args = append(args, NewArgRegular(NewAbiTyBase(BaseWord), NewValIdent(Ident(arg.StringVal))))
				default:
					return fmt.Errorf("unexpected argument type %s at %s, expected string or number",
						arg.Type, arg.Location)
				}

				arg, err = p.expectType(TypeRparen, TypeComma)
				if err != nil {
					return err
				}

				if arg.Type == TypeComma {
					arg, err = p.nextToken()
					if err != nil {
						return err
					}
				}
			}

			block.Instructions = append(block.Instructions,
				NewCall(NewValGlobal(Ident(first.StringVal)), args...))
		}
	}
}

func (p *parser) expectKeyword(kws ...Keyword) (Token, error) {
	token, err := p.expectType(TypeKeyword)
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
		strings.Join(kwnames, " or "), token.Location, token.StringVal)
}

func (p *parser) peekType(tts ...TokenType) (Token, error) {
	tok, err := p.expectType(tts...)
	if errors.Is(err, io.EOF) {
		return tok, err
	} else if err != nil {
		p.index-- // Rollback index if not EOF
	}

	return tok, nil
}

func (p *parser) expectType(tts ...TokenType) (Token, error) {
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
		strings.Join(ttnames, " or "), token.Location, token.StringVal)
}

func (p *parser) nextToken() (Token, error) {
	if p.index >= len(p.tok) {
		return Token{}, io.EOF
	}

	token := p.tok[p.index]
	p.index++

	return token, nil
}
