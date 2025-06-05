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
		token, err := p.nextToken()
		if err != nil {
			return p.ops, err
		}

		if token.Type != TypeKeyword {
			return p.ops, fmt.Errorf("unexpected token %s at %s",
				token.StringVal, token.Location)
		}

		if token.Keyword != KeywordFunc {
			return p.ops, fmt.Errorf("unexpected keyword %s at %s",
				token.Keyword, token.Location)
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
	name, err := p.expect(TypeIdent)
	if err != nil {
		return err
	}

	if _, err := p.expect(TypeLparen); err != nil {
		return err
	}

	args := []Token{name}

	arg, err := p.nextToken()
	if err != nil {
		return err
	}

	for arg.Type != TypeRparen {
		switch arg.Type {
		case TypeIdent:
			args = append(args, arg)
		default:
			return fmt.Errorf("unexpected argument type %s at %s, expected identifier, string or number",
				arg.Type, arg.Location)
		}

		arg, err = p.expect(TypeRparen, TypeComma)
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

	lbrace, err := p.expect(TypeLbrace)
	if err != nil {
		return err
	}

	p.ops = append(p.ops, Op{
		Type:     OpTypeFunc,
		Location: start.Location,
		Args:     args,
	})

	if err := p.parseBody(lbrace); err != nil {
		return err
	}

	ret, err := p.expect(TypeRbrace)
	if err != nil {
		return err
	}

	p.ops = append(p.ops, Op{
		Type:     OpTypeReturn,
		Location: ret.Location,
		Args:     nil,
	})

	return nil
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

			return nil
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

				arg, err = p.expect(TypeRparen, TypeComma)
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

func (p *parser) expect(tt ...TokenType) (Token, error) {
	token, err := p.nextToken()
	if err != nil {
		return token, err
	}

	var ttnames []string

	for _, t := range tt {
		ttnames = append(ttnames, string(t))
		if token.Type == t {
			return token, nil
		}
	}

	return token, fmt.Errorf("expected %s at %s, got %s",
		strings.Join(ttnames, " or "), token.Location, token.StringVal)
}
