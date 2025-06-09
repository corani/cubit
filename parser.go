package main

import (
	"fmt"
	"io"
	"strings"
)

type OpType string

const (
	OpTypeFunc   OpType = "Func"
	OpTypeCall   OpType = "Call"
	OpTypeReturn OpType = "Return"
)

type Op struct {
	Type     OpType
	Location Location
	Args     []Token
}

func (op Op) String() string {
	switch op.Type {
	case OpTypeFunc:
		var sb strings.Builder

		if len(op.Args) < 2 {
			return fmt.Sprintf("Func %s() @ %s\n", op.Args[0].Identifier, op.Location)
		} else {
			fmt.Fprintf(&sb, "Func %s(\n", op.Args[0].Identifier)
			for _, arg := range op.Args[1:] {
				fmt.Fprintf(&sb, "  %s\n", arg)
			}
			fmt.Fprintf(&sb, ") @ %s\n", op.Location)
		}

		return sb.String()
	case OpTypeCall:
		var sb strings.Builder

		if len(op.Args) < 2 {
			return fmt.Sprintf("Call %s() @ %s\n", op.Args[0].Identifier, op.Location)
		} else {
			fmt.Fprintf(&sb, "Call %s(\n", op.Args[0].Identifier)
			for _, arg := range op.Args[1:] {
				fmt.Fprintf(&sb, "  %s\n", arg)
			}
			fmt.Fprintf(&sb, ") @ %s\n", op.Location)
		}

		return sb.String()
	case OpTypeReturn:
		return fmt.Sprintf("Return @ %s\n", op.Location)
	default:
		return fmt.Sprintf("Unknown %s @ %s\n", op.Type, op.Location)
	}
}

type parser struct {
	tok   []Token
	index int
	ops   []Op
}

func NewParser(tok []Token) *parser {
	return &parser{
		tok:   tok,
		index: 0,
		ops:   nil,
	}
}

func (p *parser) Parse() ([]Op, error) {
	for {
		token, err := p.expectKeyword(KeywordFunc)
		if err != nil {
			return p.ops, err
		}

		if err := p.parseFunc(token); err != nil {
			return p.ops, err
		}
	}
}

func (p *parser) nextToken() (Token, error) {
	if p.index >= len(p.tok) {
		return Token{}, io.EOF
	}

	token := p.tok[p.index]
	p.index++

	return token, nil
}

func (p *parser) parseFunc(start Token) error {
	name, err := p.expectType(TypeIdent)
	if err != nil {
		return err
	}

	if _, err := p.expectType(TypeLparen); err != nil {
		return err
	}

	args := []Token{name}

	arg, err := p.nextToken()
	if err != nil {
		return err
	}

	for arg.Type != TypeRparen {
		if arg.Type != TypeIdent {
			return fmt.Errorf("unexpected argument type %s at %s, expected identifier, string or number",
				arg.Type, arg.Location)
		}

		argType, err := p.expectKeyword(KeywordInt, KeywordString)
		if err != nil {
			return err
		}

		// TODO(daniel): this is a hack.
		switch argType.Keyword {
		case KeywordInt:
			arg.Type = TypeNumber
		case KeywordString:
			arg.Type = TypeString
		}

		args = append(args, arg)

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

	lbrace, err := p.expectType(TypeLbrace, TypeArrow)
	if err != nil {
		return err
	}

	retType := Token{
		Type:     TypeKeyword,
		Location: lbrace.Location,
		Keyword:  KeywordVoid,
	}

	if lbrace.Type == TypeArrow {
		// read the return type
		retType, err = p.expectKeyword(KeywordInt, KeywordString, KeywordVoid)
		if err != nil {
			return err
		}

		// read the lbrace
		lbrace, err = p.expectType(TypeLbrace)
		if err != nil {
			return err
		}
	}

	// insert retType after the first argument (function name)
	if len(args) < 2 {
		args = append(args, retType)
	} else {
		args = append(args[:1], append([]Token{retType}, args[1:]...)...)
	}

	p.ops = append(p.ops, Op{
		Type:     OpTypeFunc,
		Location: start.Location,
		Args:     args,
	})

	if err := p.parseBody(lbrace); err != nil {
		return err
	}

	_, err = p.expectType(TypeRbrace)

	return err
}

func (p *parser) parseBody(start Token) error {
	if start.Type != TypeLbrace {
		return fmt.Errorf("expected { at %s, got %s",
			start.Location, start.StringVal)
	}

	for {
		first, err := p.nextToken()
		if err != nil {
			return err
		}

		switch first.Type {
		case TypeRbrace:
			p.index--

			// if there's no 'return' at the end, add one:
			if len(p.ops) > 0 && p.ops[len(p.ops)-1].Type != OpTypeReturn {
				p.ops = append(p.ops, Op{
					Type:     OpTypeReturn,
					Location: first.Location,
					Args: []Token{{
						Type:     TypeKeyword,
						Keyword:  KeywordVoid,
						Location: first.Location,
					}},
				})
			}

			return nil
		case TypeKeyword:
			switch first.Keyword {
			case KeywordReturn:
				// parse return value
				ret, err := p.expectType(TypeString, TypeNumber, TypeIdent)
				if err != nil {
					return err
				}

				p.ops = append(p.ops, Op{
					Type:     OpTypeReturn,
					Location: first.Location,
					Args:     []Token{ret},
				})
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

			args := []Token{first}

			arg, err := p.nextToken()
			if err != nil {
				return err
			}

			// Read function arguments
			for arg.Type != TypeRparen {
				switch arg.Type {
				case TypeString, TypeNumber, TypeIdent:
					args = append(args, arg)
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

			p.ops = append(p.ops, Op{
				Type:     OpTypeCall,
				Location: first.Location,
				Args:     args,
			})
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
