package lexer

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestLexerTokens(t *testing.T) {
	cases := []struct {
		name   string
		input  string
		tokens []TokenType
	}{
		{
			name:   "identifiers and numbers",
			input:  "foo 123 bar",
			tokens: []TokenType{TypeIdent, TypeNumber, TypeIdent},
		},
		{
			name:   "parens and semicolon",
			input:  "(foo)\nbar",
			tokens: []TokenType{TypeLparen, TypeIdent, TypeRparen, TypeSemicolon, TypeIdent},
		},
		{
			name:   "string literal",
			input:  "\"hello\"",
			tokens: []TokenType{TypeString},
		},
		{
			name:   "operators",
			input:  "+ - * / == != <= >=",
			tokens: []TokenType{TypePlus, TypeMinus, TypeStar, TypeSlash, TypeEq, TypeNe, TypeLe, TypeGe},
		},
		{
			name:   "comment and code",
			input:  "foo // comment\nbar",
			tokens: []TokenType{TypeIdent, TypeSemicolon, TypeIdent},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			s, err := NewScanner("test.go", bytes.NewReader([]byte(tc.input)))
			require.NoError(t, err)
			lx := NewLexer(s)
			toks, err := lx.Tokens()
			require.NoError(t, err)
			t.Logf("got tokens: %v", toks)
			require.Equal(t, len(tc.tokens), len(toks), "token count")
			for i, wantType := range tc.tokens {
				require.Equal(t, wantType, toks[i].Type, "token %d", i)
			}
		})
	}
}

func TestShouldInsertSemicolon(t *testing.T) {
	t.Parallel()

	loc := Location{"foo.go", 1, 1}

	tt := []struct {
		name      string
		paren     int
		bracket   int
		prevToken *Token
		want      bool
	}{
		{
			name:      "inside parens",
			paren:     1,
			bracket:   0,
			prevToken: &Token{Type: TypeIdent, Location: loc},
			want:      false,
		},
		{
			name:      "inside brackets",
			paren:     0,
			bracket:   1,
			prevToken: &Token{Type: TypeIdent, Location: loc},
			want:      false,
		},
		{
			name:      "no prev token",
			paren:     0,
			bracket:   0,
			prevToken: nil,
			want:      false,
		},
		{
			name:      "identifier",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeIdent, Location: loc},
			want:      true,
		},
		{
			name:      "number",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeNumber, Location: loc},
			want:      true,
		},
		{
			name:      "string",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeString, Location: loc},
			want:      true,
		},
		{
			name:      "bool",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeBool, Location: loc},
			want:      true,
		},
		{
			name:      "right paren",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeRparen, Location: loc},
			want:      true,
		},
		{
			name:      "right bracket",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeRBracket, Location: loc},
			want:      true,
		},
		{
			name:      "right brace",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypeRbrace, Location: loc},
			want:      true,
		},
		{
			name:      "other type",
			paren:     0,
			bracket:   0,
			prevToken: &Token{Type: TypePlus, Location: loc},
			want:      false,
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			lx := &Lexer{
				parenDepth:   tc.paren,
				bracketDepth: tc.bracket,
				prevToken:    tc.prevToken,
			}
			got := lx.shouldInsertSemicolon()
			require.Equal(t, tc.want, got)
		})
	}
}
