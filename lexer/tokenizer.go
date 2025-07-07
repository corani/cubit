package lexer

import (
	"errors"
	"io"
)

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

func (t *Tokenizer) Next() (Token, error) {
	if len(t.Buffer) > 0 {
		token := t.Buffer[0]
		t.Buffer = t.Buffer[1:]

		return token, nil
	}

	var buf []byte

	for {
		c, err := t.Scan.Next()
		if err != nil {
			return Token{}, err
		}

		start := t.Scan.Location()

		// Special handling for comments, minus, slash, string, numbers, keywords and identifiers.
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

			return NewStringToken(string(buf), start)
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

			return NewNumberToken(string(buf), start)
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

			return NewIdentOrKeywordToken(string(buf), start)
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
