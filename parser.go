package main

import (
	"errors"
	"fmt"
	"io"
	"strings"

	"github.com/corani/refactored-giggle/ast"
	"github.com/corani/refactored-giggle/lexer"
)

type parser struct {
	tok        []lexer.Token
	index      int
	unit       *ast.CompilationUnit
	blocks     []ast.Block
	attributes map[ast.AttrKey]ast.AttrValue
	pkgName    string
	localID    int
}

func NewParser(tok []lexer.Token) *parser {
	// Accepts []lexer.Token, but for compatibility with old code, allow []Token as input and convert if needed
	toks := make([]lexer.Token, len(tok))
	for i, t := range tok {
		toks[i] = lexer.Token{
			Type:       lexer.TokenType(t.Type),
			Keyword:    lexer.Keyword(t.Keyword),
			Identifier: t.Identifier,
			StringVal:  t.StringVal,
			NumberVal:  t.NumberVal,
			Location:   lexer.Location(t.Location),
		}
	}
	return &parser{
		tok:        toks,
		index:      0,
		unit:       new(ast.CompilationUnit),
		blocks:     nil,
		attributes: make(map[ast.AttrKey]ast.AttrValue),
		pkgName:    "",
		localID:    0,
	}
}

func (p *parser) Parse() (*ast.CompilationUnit, error) {
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
			if p.pkgName == "" {
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

func (p *parser) parsePackage(start lexer.Token) error {
	_ = start

	if p.pkgName != "" {
		return fmt.Errorf("package already defined at %s, cannot redefine",
			p.tok[p.index-1].Location)
	}

	// Expect package name
	pkgName, err := p.expectType(lexer.TypeIdent)
	if err != nil {
		return err
	}

	p.pkgName = pkgName.StringVal

	return nil
}

func (p *parser) parseAttributes(start lexer.Token) error {
	_ = start

	// Expect opening parenthesis
	_, err := p.expectType(lexer.TypeLparen)
	if err != nil {
		return err
	}

	// Parse attributes until closing parenthesis
	for {
		tok, err := p.expectType(lexer.TypeRparen, lexer.TypeIdent)
		if err != nil {
			return err
		}
		if tok.Type == lexer.TypeRparen {
			break
		}

		// Attribute key (string)

		key := tok.StringVal
		validKey, err := ast.ParseAttrKey(key)
		if err != nil {
			return err
		}
		var value ast.AttrValue

		// Check for '='
		next, err := p.expectType(lexer.TypeEquals, lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err
		}

		if next.Type == lexer.TypeEquals {
			// Value can be a string or number
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

			// After value, expect comma or rparen
			next, err = p.expectType(lexer.TypeComma, lexer.TypeRparen)
			if err != nil {
				return err
			}
		}

		p.attributes[validKey] = value

		if next.Type == lexer.TypeRparen {
			break
		}
		// else, next.Type == TypeComma, continue
	}

	return nil
}

// removed duplicate definition
func (p *parser) parseFunc(name lexer.Token) error {
	defer func() {
		clear(p.attributes)
	}()

	if _, err := p.expectType(lexer.TypeLparen); err != nil {
		return err
	}

	var params []ast.Param

	// Parse parameters
	for {
		arg, err := p.expectType(lexer.TypeRparen, lexer.TypeIdent)
		if err != nil {
			return err
		}

		if arg.Type == lexer.TypeRparen {
			break
		}

		if _, err := p.expectType(lexer.TypeColon); err != nil {
			return err
		}

		argType, err := p.expectKeyword(lexer.KeywordInt, lexer.KeywordString)
		if err != nil {
			return err
		}

		switch argType.Keyword {
		case lexer.KeywordInt:
			params = append(params, ast.NewParamRegular(ast.NewAbiTyBase(ast.BaseWord), ast.Ident(arg.StringVal)))
		case lexer.KeywordString:
			params = append(params, ast.NewParamRegular(ast.NewAbiTyBase(ast.BaseLong), ast.Ident(arg.StringVal)))
		}

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
		// read the return type
		retType, err = p.expectKeyword(lexer.KeywordInt, lexer.KeywordString, lexer.KeywordVoid)
		if err != nil {
			return err
		}
	}

	if _, ok := p.attributes["extern"]; ok {
		// no body
		return nil
	} else {
		lbrace, err := p.expectType(lexer.TypeLbrace)
		if err != nil {
			return err
		}

		if err := p.parseBody(lbrace, retType); err != nil {
			return err
		}

		_, err = p.expectType(lexer.TypeRbrace)

		fn := ast.NewFuncDef(ast.Ident(name.StringVal), params...).WithBlocks(p.blocks...)

		if _, ok := p.attributes["export"]; ok {
			fn = fn.WithLinkage(ast.NewLinkageExport())
		}

		if retType.Keyword == lexer.KeywordInt {
			fn = fn.WithRetTy(ast.NewAbiTyBase(ast.BaseWord))
		}

		p.unit.WithFuncDefs(fn)

		return err
	}
}

// removed duplicate definition
func (p *parser) parseBody(start, retType lexer.Token) error {
	if start.Type != lexer.TypeLbrace {
		return fmt.Errorf("expected { at %s, got %s",
			start.Location, start.StringVal)
	}

	block := &ast.Block{Label: "start"}

	for {
		first, err := p.nextToken()
		if err != nil {
			return err
		}

		switch first.Type {
		case lexer.TypeRbrace:
			p.index--

			addRet := false

			if len(block.Instructions) == 0 {
				addRet = true
			} else {
				_, hasRet := block.Instructions[len(block.Instructions)-1].(ast.Ret)
				addRet = !hasRet
			}

			if addRet {
				switch retType.Keyword {
				case lexer.KeywordVoid:
					block.Instructions = append(block.Instructions, ast.NewRet())
				default:
					return fmt.Errorf("expected return statement at %s", first.Location)
				}
			}

			p.blocks = []ast.Block{*block}

			return nil
		case lexer.TypeKeyword:
			switch first.Keyword {
			case lexer.KeywordReturn:
				// parse return value
				ret, err := p.expectType(lexer.TypeString, lexer.TypeNumber, lexer.TypeIdent)
				if err != nil {
					return err
				}

				var val ast.Val

				switch ret.Type {
				case lexer.TypeNumber:
					val = ast.NewValInteger(int64(ret.NumberVal))
				default:
					panic(fmt.Sprintf("unexpected return type %s at %s, expected number",
						ret.Type, ret.Location))
				}

				block.Instructions = append(block.Instructions, ast.NewRet(val))
			}
		case lexer.TypeIdent:
			// Check if it's a function call
			token, err := p.nextToken()
			if err != nil {
				return err
			}

			switch token.Type {
			case lexer.TypeLparen:
				if err := p.parseCall(first, block); err != nil {
					return err
				}
			case lexer.TypeColon:
				if err := p.parseDecl(first, block); err != nil {
					return err
				}
			default:
				return fmt.Errorf("expected ( after identifier at %s, got %s",
					token.Location, token.StringVal)
			}
		}
	}
}

func (p *parser) parseDecl(first lexer.Token, block *ast.Block) error {
	// Expect type
	ty, err := p.expectKeyword(lexer.KeywordInt, lexer.KeywordString)
	if err != nil {
		return err
	}

	var abiTy ast.AbiTy

	switch ty.Keyword {
	case lexer.KeywordInt:
		abiTy = ast.NewAbiTyBase(ast.BaseWord)
	case lexer.KeywordString:
		abiTy = ast.NewAbiTyBase(ast.BaseLong)
	default:
		return fmt.Errorf("unexpected type %s at %s", ty.Keyword, ty.Location)
	}

	if _, err := p.expectType(lexer.TypeEquals); err != nil {
		return err
	}

	lhs, err := p.expectType(lexer.TypeNumber, lexer.TypeIdent)
	if err != nil {
		return err
	}

	var val ast.Val

	switch lhs.Type {
	case lexer.TypeNumber:
		val, err = p.parseVal(ast.NewValInteger(int64(lhs.NumberVal)), block)
		if err != nil {
			return err
		}
	case lexer.TypeIdent:
		val, err = p.parseVal(ast.NewValIdent(ast.Ident(lhs.StringVal)), block)
		if err != nil {
			return err
		}
	}

	// generate an assignment instruction
	// TODO(daniel): this is a hack to assign the generated local name to the local variable name.
	block.Instructions = append(block.Instructions,
		ast.NewInstr(fmt.Sprintf("%%%s =%s add 0, %%%s", first.StringVal, abiTy.String(), val.Ident)))

	return nil
}

func (p *parser) parseCall(first lexer.Token, block *ast.Block) error {
	arg, err := p.nextToken()
	if err != nil {
		return err
	}

	var args []ast.Arg

	// Read function arguments
	for arg.Type != lexer.TypeRparen {
		switch arg.Type {
		case lexer.TypeString:
			id := fmt.Sprintf("data_%s%d", first.StringVal, len(args))
			p.unit.WithDataDefs(ast.NewDataDefStringZ(ast.Ident(id), arg.StringVal))
			args = append(args, ast.NewArgRegular(ast.NewAbiTyBase(ast.BaseLong), ast.NewValGlobal(ast.Ident(id))))
		case lexer.TypeNumber:
			lhs := ast.NewValInteger(int64(arg.NumberVal))

			lhs, err := p.parseVal(lhs, block)
			if err != nil {
				return err
			}

			args = append(args, ast.NewArgRegular(ast.NewAbiTyBase(ast.BaseWord), lhs))
		case lexer.TypeIdent:
			lhs := ast.NewValIdent(ast.Ident(arg.StringVal))

			lhs, err := p.parseVal(lhs, block)
			if err != nil {
				return err
			}

			args = append(args, ast.NewArgRegular(ast.NewAbiTyBase(ast.BaseWord), lhs))
		default:
			return fmt.Errorf("unexpected argument type %s at %s, expected string or number",
				arg.Type, arg.Location)
		}

		arg, err = p.expectType(lexer.TypeRparen, lexer.TypeComma)
		if err != nil {
			return err
		}

		if arg.Type == lexer.TypeComma {
			arg, err = p.nextToken()
			if err != nil {
				return err
			}
		}
	}

	block.Instructions = append(block.Instructions,
		ast.NewCall(ast.NewValGlobal(ast.Ident(first.StringVal)), args...))

	return nil
}

func (p *parser) parseVal(lhs ast.Val, block *ast.Block) (ast.Val, error) {
	// TODO(daniel): This is a hack to get '+' with two numbers to work here. This
	// needs to be factored out in a proper expression parser.
	next, err := p.peekType(lexer.TypePlus)
	if err != nil {
		return ast.Val{}, err
	}
	if next.Type == lexer.TypePlus {
		next, err := p.expectType(lexer.TypeNumber)
		if err != nil {
			return ast.Val{}, err
		}
		rhs := ast.NewValInteger(int64(next.NumberVal))
		ret := ast.NewValIdent(ast.Ident(fmt.Sprintf("local_%d", p.localID)))
		p.localID++
		block.Instructions = append(block.Instructions, ast.NewAdd(ret, lhs, rhs))
		lhs = ret
	}

	return lhs, nil
}

func (p *parser) expectKeyword(kws ...lexer.Keyword) (lexer.Token, error) {
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

func (p *parser) peekType(tts ...lexer.TokenType) (lexer.Token, error) {
	tok, err := p.expectType(tts...)
	if errors.Is(err, io.EOF) {
		return tok, err
	} else if err != nil {
		p.index-- // Rollback index if not EOF
	}

	return tok, nil
}

func (p *parser) expectType(tts ...lexer.TokenType) (lexer.Token, error) {
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

func (p *parser) nextToken() (lexer.Token, error) {
	if p.index >= len(p.tok) {
		return lexer.Token{}, io.EOF
	}

	token := p.tok[p.index]
	p.index++

	return token, nil
}
