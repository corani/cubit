package lexer

import (
	"errors"
	"io"
)

type Lexer struct {
	Scan         *Scanner
	Buffer       []Token
	parenDepth   int
	bracketDepth int
	prevToken    *Token
}

func NewLexer(scan *Scanner) *Lexer {
	return &Lexer{
		Scan:         scan,
		Buffer:       nil,
		parenDepth:   0,
		bracketDepth: 0,
		prevToken:    nil,
	}
}

func (t *Lexer) Tokens() ([]Token, error) {
	var tokens []Token

	for {
		token, err := t.Next()
		if err != nil {
			if errors.Is(err, io.EOF) {
				return tokens, nil
			}

			return nil, err
		}

		tokens = append(tokens, token)
	}
}

func (t *Lexer) Next() (Token, error) {
	if len(t.Buffer) > 0 {
		token := t.Buffer[0]
		t.Buffer = t.Buffer[1:]
		return token, nil
	}

	var buf []byte

	for {
		c, err := t.Scan.Next()
		if err != nil {
			return Token{}, err // EOF
		}

		start := t.Scan.Location()

		switch {
		// NOTE(daniel): we track parentheses and brackets depth to manage virtual
		// semicolon insertion.
		case c == '(':
			t.parenDepth++
			return Token{Type: TypeLparen, StringVal: "(", Location: start}, nil
		case c == ')':
			if t.parenDepth > 0 {
				t.parenDepth--
			}
			return Token{Type: TypeRparen, StringVal: ")", Location: start}, nil
		case c == '[':
			t.bracketDepth++
			return Token{Type: TypeLBracket, StringVal: "[", Location: start}, nil
		case c == ']':
			if t.bracketDepth > 0 {
				t.bracketDepth--
			}
			return Token{Type: TypeRBracket, StringVal: "]", Location: start}, nil
		case c == '/':
			c2, err := t.Scan.Next()
			if err != nil { // EOF, we still want to return the token
				t.prevToken = &Token{Type: TypeSlash, StringVal: "/", Location: start}
				return *t.prevToken, nil
			}

			switch {
			case c2 == '/':
				// Skip comment
				for {
					c, err = t.Scan.Next()
					if err != nil {
						break // EOF
					}
					if c == '\n' || c == '\r' {
						t.Scan.Unread(1) // Unread the newline character
						break
					}
				}
				continue
			default:
				t.Scan.Unread(1)
				t.prevToken = &Token{Type: TypeSlash, StringVal: "/", Location: start}
				return *t.prevToken, nil
			}
		case c == '\n':
			// Only emit a semicolon if not inside parens or brackets
			if t.shouldInsertSemicolon() {
				t.prevToken = &Token{Type: TypeSemicolon, StringVal: ";", Location: start}
				return *t.prevToken, nil
			}

			// Otherwise, skip the newline
			continue
		case isWhitespace(c):
			continue
		case c == '"':
			// Handle string literals
			for {
				c, err = t.Scan.Next()
				if err != nil { // EOF, we still want to return the token
					t.prevToken = &Token{Type: TypeString, StringVal: string(buf), Location: start}
					return *t.prevToken, nil
				}
				if c == '"' {
					break
				}
				if c == '\\' {
					c, err = t.Scan.Next()
					if err != nil {
						return Token{}, err // EOF
					}
					buf = append(buf, '\\', c)
				} else {
					buf = append(buf, c)
				}
			}
			tok, err := NewStringToken(string(buf), start)
			t.prevToken = &tok
			return tok, err
		case isNumeric(c):
			// Handle numeric literals, including 0x (hex), 0b (bin), 0o (octal)
			buf = append(buf, c)
			base := byte('d') // default to decimal

			c2, err := t.Scan.Next()
			if err != nil {
				break // EOF, we still want to return the token
			}

			if buf[0] == '0' && (c2 == 'x' || c2 == 'b' || c2 == 'o') {
				buf = append(buf, c2)
				base = c2
			} else {
				t.Scan.Unread(1) // unread the second character
			}

			for {
				c, err = t.Scan.Next()
				if err != nil {
					break // EOF, we still want to return the token
				}

				if c == '_' {
					// Skip underscores in numeric literals
					continue
				}

				ok := false

				switch base {
				case 'd':
					ok = (c >= '0' && c <= '9')
				case 'x':
					ok = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
				case 'b':
					ok = (c == '0' || c == '1')
				case 'o':
					ok = (c >= '0' && c <= '7')
				}

				if ok {
					buf = append(buf, c)
				} else {
					t.Scan.Unread(1)
					break
				}
			}

			tok, err := NewNumberToken(string(buf), start)
			t.prevToken = &tok
			return tok, err
		case isAlpha(c):
			// Handle identifiers and keywords
			buf = append(buf, c)
			for {
				c, err = t.Scan.Next()
				if err != nil {
					break // EOF, we still want to return the token
				}
				if isAlphanumeric(c) {
					buf = append(buf, c)
				} else {
					t.Scan.Unread(1)
					break
				}
			}
			tok, err := NewIdentOrKeywordToken(string(buf), start)
			t.prevToken = &tok
			return tok, err
		default:
			// Maximal munch for symbolic tokens
			mmType := TypeEOF
			mmToken := ""
			prefix := []byte{c}
			for {
				foundPrefix := false
				for k, v := range symbols {
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
				if count := len(prefix) - len(mmToken); count > 0 {
					t.Scan.Unread(count)
				}
				t.prevToken = &Token{Type: mmType, StringVal: mmToken, Location: start}
				return *t.prevToken, nil
			}
		}
	}
}

// shouldInsertSemicolon returns true if a semicolon should be inserted after the given
// token type.
func (t *Lexer) shouldInsertSemicolon() bool {
	if t.parenDepth > 0 || t.bracketDepth > 0 || t.prevToken == nil {
		return false
	}

	switch t.prevToken.Type {
	case TypeIdent, TypeNumber, TypeString, TypeBool, TypeRparen, TypeRBracket, TypeRbrace:
		return true
	default:
		return false
	}
}

func isAlphanumeric(a byte) bool { return isAlpha(a) || isNumeric(a) }
func isAlpha(a byte) bool        { return (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z') || a == '_' }
func isNumeric(d byte) bool      { return d >= '0' && d <= '9' }
func isWhitespace(c byte) bool   { return c == ' ' || c == '\t' || c == '\n' || c == '\r' }
