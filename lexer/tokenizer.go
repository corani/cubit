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
	TypeNe        TokenType = "Ne"
	TypeLt        TokenType = "Lt"
	TypeLe        TokenType = "Le"
	TypeGt        TokenType = "Gt"
	TypeGe        TokenType = "Ge"
	TypeDollar    TokenType = "Dollar"
	TypeCaret     TokenType = "Caret"
	TypeShl       TokenType = "ShiftLeft"  // <<
	TypeShr       TokenType = "ShiftRight" // >>
	TypeBinAnd    TokenType = "BinaryAnd"  // &
	TypeBinOr     TokenType = "BinaryOr"   // |
	TypeLogAnd    TokenType = "LogicalAnd" // &&
	TypeLogOr     TokenType = "LogicalOr"  // ||
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
	KeywordNil      Keyword = "nil"
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
	KeywordNil,
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
	case TypeNe:
		return "Ne @ " + t.Location.String()
	case TypeLt:
		return "Lt @ " + t.Location.String()
	case TypeLe:
		return "Le @ " + t.Location.String()
	case TypeGt:
		return "Gt @ " + t.Location.String()
	case TypeGe:
		return "Ge @ " + t.Location.String()
	case TypeDollar:
		return "Dollar @ " + t.Location.String()
	case TypeCaret:
		return "Caret @ " + t.Location.String()
	case TypeShl:
		return "ShiftLeft @ " + t.Location.String()
	case TypeShr:
		return "ShiftRight @ " + t.Location.String()
	case TypeBinAnd:
		return "BinaryAnd @ " + t.Location.String()
	case TypeBinOr:
		return "BinaryOr @ " + t.Location.String()
	case TypeLogAnd:
		return "LogicalAnd @ " + t.Location.String()
	case TypeLogOr:
		return "LogicalOr @ " + t.Location.String()
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

	// Maximum Munch Map of symbolic tokens
	tokens := map[string]TokenType{
		"(":  TypeLparen,
		")":  TypeRparen,
		"{":  TypeLbrace,
		"}":  TypeRbrace,
		"[":  TypeLBracket,
		"]":  TypeRBracket,
		".":  TypeDot,
		",":  TypeComma,
		":":  TypeColon,
		";":  TypeSemicolon,
		"@":  TypeAt,
		"+":  TypePlus,
		"*":  TypeStar,
		"$":  TypeDollar,
		"^":  TypeCaret,
		"=":  TypeAssign,
		"==": TypeEq,
		"!=": TypeNe,
		"<":  TypeLt,
		"<=": TypeLe,
		"<<": TypeShl,
		">":  TypeGt,
		">=": TypeGe,
		">>": TypeShr,
		"&":  TypeBinAnd,
		"&&": TypeLogAnd,
		"|":  TypeBinOr,
		"||": TypeLogOr,
	}

	var buf []byte

	for {
		c, err := t.Scan.Next()
		if err != nil {
			return Token{}, err
		}

		start := t.Scan.Location()

		// Special handling for comments, minus, slash, exclamation, string, whitespace, numbers, identifiers
		switch {
		case c == '/':
			c2, err := t.Scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c2 == '/':
				// Skip comment
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
				t.Scan.Unread(1)

				return Token{Type: TypeSlash, StringVal: "/", Location: start}, nil
			}
		case c == '-':
			c2, err := t.Scan.Next()
			if err != nil {
				return Token{}, err
			}

			switch {
			case c2 == '>':
				return Token{Type: TypeArrow, StringVal: "->", Location: start}, nil
			case isNumeric(c2):
				buf = append(buf, '-')

				// Unread the number and continue, we'll fall into the numeric literal case
				// with a '-' already in the buffer.
				t.Scan.Unread(1)
			default:
				t.Scan.Unread(1)

				return Token{Type: TypeMinus, StringVal: "-", Location: start}, nil
			}
		case isWhitespace(c):
			continue
		case c == '"':
			// Handle string literals
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
			// Handle numeric literals
			// TODO(daniel): Handle floating point numbers
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
			// Handle identifiers and keywords
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

			// Turn keywords `true` and `false` into boolean literal tokens.
			switch kw {
			case KeywordFalse, KeywordTrue:
				return Token{Type: TypeBool, Keyword: kw, Identifier: string(buf), StringVal: string(buf), Location: start}, nil
			default:
				return Token{Type: TypeKeyword, Keyword: kw, Identifier: string(buf), StringVal: string(buf), Location: start}, nil
			}
		default:
			// Maximal munch for symbolic tokens
			mmType := TypeEOF
			mmToken := ""
			prefix := []byte{c}

			for {
				foundPrefix := false
				for k, v := range tokens {
					if len(k) >= len(prefix) && k[:len(prefix)] == string(prefix) {
						foundPrefix = true

						if k == string(prefix) {
							mmToken = k
							mmType = v
						}
					}
				}

				if !foundPrefix {
					break
				}

				c2, err := t.Scan.Next()
				if err != nil {
					break
				}

				prefix = append(prefix, c2)
			}

			if mmToken != "" {
				// Unread any extra characters
				if count := len(prefix) - len(mmToken); count > 0 {
					t.Scan.Unread(count)
				}

				return Token{Type: mmType, StringVal: mmToken, Location: start}, nil
			}
		}
	}
}

func isAlphanumeric(a byte) bool { return isAlpha(a) || isNumeric(a) }
func isAlpha(a byte) bool        { return (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a == '_' }
func isNumeric(d byte) bool      { return d >= '0' && d <= '9' }
func isWhitespace(c byte) bool   { return c == ' ' || c == '\t' || c == '\n' || c == '\r' }
