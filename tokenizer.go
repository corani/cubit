package main

import (
	"errors"
	"io"
	"strconv"
)

type TokenType string

const (
	TypeEOF     TokenType = "EOF"
	TypeIdent   TokenType = "Identifier"
	TypeKeyword TokenType = "Keyword"
	TypeNumber  TokenType = "Number"
	TypeString  TokenType = "String"
	TypeLparen  TokenType = "LeftParen"
	TypeRparen  TokenType = "RightParen"
	TypeLbrace  TokenType = "LeftBrace"
	TypeRbrace  TokenType = "RightBrace"
	TypeComma   TokenType = "Comma"
	TypeArrow   TokenType = "Arrow"
	TypeColon   TokenType = "Colon"
	TypeAt      TokenType = "At"
	TypeEquals  TokenType = "Equals"
	TypePlus    TokenType = "Plus"
)

type Keyword string

const (
	KeywordFunc    Keyword = "func"
	KeywordReturn  Keyword = "return"
	KeywordInt     Keyword = "int"
	KeywordString  Keyword = "string"
	KeywordVoid    Keyword = "void"
	KeywordPackage Keyword = "package"
)

type Token struct {
	Type       TokenType
	Keyword    Keyword
	Identifier string
	StringVal  string
	NumberVal  int
	Location   Location
}

func (t Token) String() string {
	switch t.Type {
	case TypeEOF:
		return "EOF @ " + t.Location.String()
	case TypeIdent:
		return "Identifier(" + t.Identifier + ") @ " + t.Location.String()
	case TypeKeyword:
		return "Keyword(" + string(t.Keyword) + ") @ " + t.Location.String()
	case TypeNumber:
		return "Number(" + strconv.Itoa(t.NumberVal) + ") @ " + t.Location.String()
	case TypeString:
		return "String(\"" + t.StringVal + "\") @ " + t.Location.String()
	case TypeLparen:
		return "LeftParen @ " + t.Location.String()
	case TypeRparen:
		return "RightParen @ " + t.Location.String()
	case TypeLbrace:
		return "LeftBrace @ " + t.Location.String()
	case TypeRbrace:
		return "RightBrace @ " + t.Location.String()
	case TypeComma:
		return "Comma @ " + t.Location.String()
	case TypeArrow:
		return "Arrow @ " + t.Location.String()
	case TypeColon:
		return "Colon @ " + t.Location.String()
	case TypeAt:
		return "At @ " + t.Location.String()
	case TypeEquals:
		return "Equals @ " + t.Location.String()
	case TypePlus:
		return "Plus @ " + t.Location.String()
	default:
		return "Unknown @ " + t.Location.String()
	}
}

func checkKeyword(ident string) (Keyword, bool) {
	switch ident {
	case "func":
		return KeywordFunc, true
	case "return":
		return KeywordReturn, true
	case "int":
		return KeywordInt, true
	case "string":
		return KeywordString, true
	case "void":
		return KeywordVoid, true
	case "package":
		return KeywordPackage, true
	default:
		return "", false
	}
}

type tokenizer struct {
	scan   *scanner
	buffer []Token
}

func NewTokenizer(scan *scanner) *tokenizer {
	return &tokenizer{
		scan:   scan,
		buffer: nil,
	}
}

func (t *tokenizer) Tokens() ([]Token, error) {
	var tokens []Token

	for {
		token, err := t.next()
		if err != nil {
			if errors.Is(err, io.EOF) {
				return tokens, nil
			}

			return nil, err
		}

		tokens = append(tokens, token)
	}
}

func (t *tokenizer) next() (Token, error) {
	if len(t.buffer) > 0 {
		token := t.buffer[0]
		t.buffer = t.buffer[1:]

		return token, nil
	}

	var buf []byte

	for {
		c, err := t.scan.Next()
		if err != nil {
			return Token{}, err
		}

		start := t.scan.Location()

		switch {
		case c == '=':
			return Token{
				Type:      TypeEquals,
				StringVal: "=",
				Location:  start,
			}, nil
		case c == '(':
			return Token{
				Type:      TypeLparen,
				StringVal: "(",
				Location:  start,
			}, nil
		case c == ')':
			return Token{
				Type:      TypeRparen,
				StringVal: ")",
				Location:  start,
			}, nil
		case c == '{':
			return Token{
				Type:      TypeLbrace,
				StringVal: "{",
				Location:  start,
			}, nil
		case c == '}':
			return Token{
				Type:      TypeRbrace,
				StringVal: "}",
				Location:  start,
			}, nil
		case c == ',':
			return Token{
				Type:      TypeComma,
				StringVal: ",",
				Location:  start,
			}, nil
		case c == ':':
			return Token{
				Type:      TypeColon,
				StringVal: ":",
				Location:  start,
			}, nil
		case c == '@':
			return Token{
				Type:      TypeAt,
				StringVal: "@",
				Location:  start,
			}, nil
		case c == '+':
			return Token{
				Type:      TypePlus,
				StringVal: "+",
				Location:  start,
			}, nil
		case c == '/':
			c, err := t.scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c == '/':
				// read a single-line comment
				for {
					c, err = t.scan.Next()
					if err != nil {
						return Token{}, err
					}
					if c == '\n' || c == '\r' {
						break
					}
				}
			default:
				t.scan.Unread(1) // unread the character
			}
		case c == '-':
			c, err := t.scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c == '>':
				return Token{
					Type:      TypeArrow,
					StringVal: "->",
					Location:  start,
				}, nil
			case c >= '0' && c <= '9':
				// it's a negative number
				buf = append(buf, '-')
				buf = append(buf, c)
				continue
			default:
				t.scan.Unread(1) // unread the character
			}
		case isWhitespace(c):
			continue
		case c == '"':
			// don't add the quotes to the buffer
			for {
				c, err = t.scan.Next()
				if err != nil {
					return Token{}, err
				}

				if c == '"' {
					break
				} else if c == '\\' {
					// handle escape sequences
					c, err = t.scan.Next()
					if err != nil {
						return Token{}, err
					}
					buf = append(buf, '\\', c)
				} else {
					buf = append(buf, c)
				}
			}

			return Token{
				Type:      TypeString,
				StringVal: string(buf),
				Location:  start,
			}, nil
		case isNumeric(c):
			buf = append(buf, c)

			for {
				c, err = t.scan.Next()
				if err != nil {
					return Token{}, err
				}

				if isNumeric(c) {
					buf = append(buf, c)
				} else {
					t.scan.Unread(1)
					break
				}
			}

			num, err := strconv.Atoi(string(buf))
			if err != nil {
				return Token{}, err
			}

			return Token{
				Type:      TypeNumber,
				NumberVal: num,
				StringVal: string(buf),
				Location:  start,
			}, nil
		case isAlpha(c):
			buf = append(buf, c)

			for {
				c, err = t.scan.Next()
				if err != nil {
					return Token{}, err
				}

				if isAlphanumeric(c) {
					buf = append(buf, c)
				} else {
					t.scan.Unread(1)
					break
				}
			}

			if kw, ok := checkKeyword(string(buf)); ok {
				return Token{
					Type:       TypeKeyword,
					Keyword:    kw,
					Identifier: string(buf),
					StringVal:  string(buf),
					Location:   start,
				}, nil
			}

			return Token{
				Type:       TypeIdent,
				Identifier: string(buf),
				StringVal:  string(buf),
				Location:   start,
			}, nil
		}
	}
}

func isAlphanumeric(a byte) bool {
	return isAlpha(a) || isNumeric(a)
}

func isAlpha(a byte) bool {
	return (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a == '_'
}

func isNumeric(d byte) bool {
	return d >= '0' && d <= '9'
}

func isWhitespace(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r'
}
