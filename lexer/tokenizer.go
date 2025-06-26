package lexer

import (
	"errors"
	"io"
	"slices"
	"strconv"
)

type TokenType string

const (
	TypeEOF       TokenType = "EOF"
	TypeIdent     TokenType = "Identifier"
	TypeKeyword   TokenType = "Keyword"
	TypeNumber    TokenType = "Number"
	TypeBool      TokenType = "Bool"
	TypeString    TokenType = "String"
	TypeLparen    TokenType = "LeftParen"
	TypeRparen    TokenType = "RightParen"
	TypeLbrace    TokenType = "LeftBrace"
	TypeRbrace    TokenType = "RightBrace"
	TypeLBracket  TokenType = "LeftBracket"
	TypeRBracket  TokenType = "RightBracket"
	TypeDot       TokenType = "Dot"
	TypeComma     TokenType = "Comma"
	TypeArrow     TokenType = "Arrow"
	TypeColon     TokenType = "Colon"
	TypeSemicolon TokenType = "Semicolon"
	TypeAt        TokenType = "At"
	TypeAssign    TokenType = "Assign"
	TypePlus      TokenType = "Plus"
	TypeMinus     TokenType = "Minus"
	TypeStar      TokenType = "Star"
	TypeSlash     TokenType = "Slash"
	TypeEq        TokenType = "Eq"
	TypeDollar    TokenType = "Dollar"
	TypeCaret     TokenType = "Caret"
)

type Keyword string

const (
	KeywordFunc     Keyword = "func"
	KeywordReturn   Keyword = "return"
	KeywordInt      Keyword = "int"
	KeywordString   Keyword = "string"
	KeywordVoid     Keyword = "void"
	KeywordPackage  Keyword = "package"
	KeywordFalse    Keyword = "false"
	KeywordTrue     Keyword = "true"
	KeywordFor      Keyword = "for"
	KeywordIf       Keyword = "if"
	KeywordElse     Keyword = "else"
	KeywordBreak    Keyword = "break"
	KeywordContinue Keyword = "continue"
	KeywordIn       Keyword = "in"
	KeywordStruct   Keyword = "struct"
	KeywordEnum     Keyword = "enum"
	KeywordImport   Keyword = "import"
	KeywordSwitch   Keyword = "switch"
	KeywordCase     Keyword = "case"
	KeywordDefault  Keyword = "default"
)

var keywords = []Keyword{
	KeywordFunc,
	KeywordReturn,
	KeywordInt,
	KeywordString,
	KeywordVoid,
	KeywordPackage,
	KeywordFalse,
	KeywordTrue,
	KeywordFor,
	KeywordIf,
	KeywordElse,
	KeywordBreak,
	KeywordContinue,
	KeywordIn,
	KeywordStruct,
	KeywordEnum,
	KeywordImport,
	KeywordSwitch,
	KeywordCase,
	KeywordDefault,
}

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
	case TypeBool:
		return "Bool(" + string(t.Keyword) + ") @ " + t.Location.String()
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
	case TypeLBracket:
		return "LeftBracket @ " + t.Location.String()
	case TypeRBracket:
		return "RightBracket @ " + t.Location.String()
	case TypeDot:
		return "Dot @ " + t.Location.String()
	case TypeComma:
		return "Comma @ " + t.Location.String()
	case TypeArrow:
		return "Arrow @ " + t.Location.String()
	case TypeColon:
		return "Colon @ " + t.Location.String()
	case TypeSemicolon:
		return "Semicolon @ " + t.Location.String()
	case TypeAt:
		return "At @ " + t.Location.String()
	case TypeAssign:
		return "Assign @ " + t.Location.String()
	case TypePlus:
		return "Plus @ " + t.Location.String()
	case TypeMinus:
		return "Minus @ " + t.Location.String()
	case TypeStar:
		return "Star @ " + t.Location.String()
	case TypeSlash:
		return "Slash @ " + t.Location.String()
	case TypeEq:
		return "Eq @ " + t.Location.String()
	case TypeDollar:
		return "Dollar @ " + t.Location.String()
	case TypeCaret:
		return "Caret @ " + t.Location.String()
	default:
		return "Unknown @ " + t.Location.String()
	}
}

func checkKeyword(ident string) (Keyword, bool) {
	if slices.Contains(keywords, Keyword(ident)) {
		return Keyword(ident), true
	}

	return "", false
}

type Tokenizer struct {
	Scan   *Scanner
	Buffer []Token
}

func NewTokenizer(scan *Scanner) *Tokenizer {
	return &Tokenizer{
		Scan:   scan,
		Buffer: nil,
	}
}

func (t *Tokenizer) Tokens() ([]Token, error) {
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

func (t *Tokenizer) next() (Token, error) {
	if len(t.Buffer) > 0 {
		token := t.Buffer[0]
		t.Buffer = t.Buffer[1:]

		return token, nil
	}

	// Define a map to translate single-character tokens to TokenType. This contains only
	// tokens that can be mapped unambiguously (e.g., '=', '(', ')', but not '-', '/').
	translate := map[byte]TokenType{
		'(': TypeLparen,
		')': TypeRparen,
		'{': TypeLbrace,
		'}': TypeRbrace,
		'[': TypeLBracket,
		']': TypeRBracket,
		'.': TypeDot,
		',': TypeComma,
		':': TypeColon,
		';': TypeSemicolon,
		'@': TypeAt,
		'+': TypePlus,
		'*': TypeStar,
		'$': TypeDollar,
		'^': TypeCaret,
	}

	var buf []byte

	for {
		c, err := t.Scan.Next()
		if err != nil {
			return Token{}, err
		}

		start := t.Scan.Location()

		if t, ok := translate[c]; ok {
			// If we have a single-character token, return it immediately
			return Token{Type: t, StringVal: string(c), Location: start}, nil
		}

		switch {
		case c == '=':
			c, err := t.Scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c == '=':
				return Token{Type: TypeEq, StringVal: "==", Location: start}, nil
			default:
				// Unread whatever we read after the equals sign
				t.Scan.Unread(1)

				return Token{Type: TypeAssign, StringVal: "=", Location: start}, nil
			}
		case c == '/':
			c, err := t.Scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c == '/':
				for {
					c, err = t.Scan.Next()
					if err != nil {
						return Token{}, err
					}

					if c == '\n' || c == '\r' {
						break
					}
				}
			default:
				// Unread whatever we read after the slash
				t.Scan.Unread(1)

				return Token{Type: TypeSlash, StringVal: "/", Location: start}, nil
			}
		case c == '-':
			c, err := t.Scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c == '>':
				return Token{Type: TypeArrow, StringVal: "->", Location: start}, nil
			case c >= '0' && c <= '9':
				buf = append(buf, '-')

				// Unread the number, so we'll fall into the numeric case on continue
				t.Scan.Unread(1)

				continue
			default:
				// Unread whatever we read after the minus sign
				t.Scan.Unread(1)

				return Token{Type: TypeMinus, StringVal: "-", Location: start}, nil
			}
		case isWhitespace(c):
			continue
		case c == '"':
			for {
				c, err = t.Scan.Next()
				if err != nil {
					return Token{}, err
				}

				if c == '"' {
					break
				}

				if c == '\\' {
					c, err = t.Scan.Next()
					if err != nil {
						return Token{}, err
					}

					buf = append(buf, '\\', c)
				} else {
					buf = append(buf, c)
				}
			}

			return Token{Type: TypeString, StringVal: string(buf), Location: start}, nil
		case isNumeric(c):
			buf = append(buf, c)
			for {
				c, err = t.Scan.Next()
				if err != nil {
					return Token{}, err
				}

				if isNumeric(c) {
					buf = append(buf, c)
				} else {
					t.Scan.Unread(1)

					break
				}
			}

			num, err := strconv.Atoi(string(buf))
			if err != nil {
				return Token{}, err
			}

			return Token{Type: TypeNumber, NumberVal: num, StringVal: string(buf), Location: start}, nil
		case isAlpha(c):
			buf = append(buf, c)

			for {
				c, err = t.Scan.Next()
				if err != nil {
					return Token{}, err
				}

				if isAlphanumeric(c) {
					buf = append(buf, c)
				} else {
					t.Scan.Unread(1)

					break
				}
			}

			kw, ok := checkKeyword(string(buf))
			if !ok {
				return Token{Type: TypeIdent, Identifier: string(buf), StringVal: string(buf), Location: start}, nil
			}

			switch kw {
			case KeywordFalse, KeywordTrue:
				return Token{Type: TypeBool, Keyword: kw, Identifier: string(buf), StringVal: string(buf), Location: start}, nil
			default:
				return Token{Type: TypeKeyword, Keyword: kw, Identifier: string(buf), StringVal: string(buf), Location: start}, nil
			}
		}
	}
}

func isAlphanumeric(a byte) bool { return isAlpha(a) || isNumeric(a) }
func isAlpha(a byte) bool        { return (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a == '_' }
func isNumeric(d byte) bool      { return d >= '0' && d <= '9' }
func isWhitespace(c byte) bool   { return c == ' ' || c == '\t' || c == '\n' || c == '\r' }
