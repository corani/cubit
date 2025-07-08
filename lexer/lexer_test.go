package lexer

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestLexer_EdgeCases(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    string
		expected []TokenType
		values   []string
	}{
		// Escape sequences in strings
		{
			name:     "string with newline escape",
			input:    `"foo\nbar"`,
			expected: []TokenType{TypeString},
			values:   []string{"foo\\nbar"},
		},
		{
			name:     "string with quote escape",
			input:    `"a\"b"`,
			expected: []TokenType{TypeString},
			values:   []string{`a\"b`},
		},
		// Negative numbers
		{
			name:     "negative number",
			input:    `-123`,
			expected: []TokenType{TypeNumber},
			values:   []string{"-123"},
		},
		{
			name:     "assign negative number",
			input:    `x = -42`,
			expected: []TokenType{TypeIdent, TypeAssign, TypeNumber},
			values:   []string{"x", "=", "-42"},
		},
		// Arrow
		{
			name:     "arrow operator",
			input:    `a->b`,
			expected: []TokenType{TypeIdent, TypeArrow, TypeIdent},
			values:   []string{"a", "->", "b"},
		},
		// Brackets
		{
			name:     "brackets with ident",
			input:    `[x]`,
			expected: []TokenType{TypeLBracket, TypeIdent, TypeRBracket},
			values:   []string{"[", "x", "]"},
		},
		{
			name:     "ident with brackets",
			input:    `foo[bar]`,
			expected: []TokenType{TypeIdent, TypeLBracket, TypeIdent, TypeRBracket},
			values:   []string{"foo", "[", "bar", "]"},
		},
		// Newlines
		{
			name:     "single newline",
			input:    "foo\nbar",
			expected: []TokenType{TypeIdent, TypeSemicolon, TypeIdent},
			values:   []string{"foo", ";", "bar"},
		},
		{
			name:     "multiple newlines",
			input:    "foo\n\nbar",
			expected: []TokenType{TypeIdent, TypeSemicolon, TypeIdent},
			values:   []string{"foo", ";", "bar"},
		},
		{
			name:     "newline with brackets",
			input:    "[foo\nbar]",
			expected: []TokenType{TypeLBracket, TypeIdent, TypeIdent, TypeRBracket},
			values:   []string{"[", "foo", "bar", "]"},
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			scan, err := NewScanner("test.in", strings.NewReader(tc.input))
			require.NoError(t, err)

			lex := NewLexer(scan)
			for i, wantType := range tc.expected {
				tok, err := lex.Next()
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				if tok.Type != wantType {
					t.Errorf("input %q: token %d: got type %v, want %v", tc.input, i, tok.Type, wantType)
				}
				if tc.values != nil && i < len(tc.values) && tok.StringVal != tc.values[i] {
					t.Errorf("input %q: token %d: got val %q, want %q", tc.input, i, tok.StringVal, tc.values[i])
				}
			}
		})
	}
}

func TestLexerTokens(t *testing.T) {
	t.Parallel()

	tt := []struct {
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
		{
			name:   "slash EOF",
			input:  "/",
			tokens: []TokenType{TypeSlash},
		},
		{
			name:   "minus EOF",
			input:  "-",
			tokens: []TokenType{TypeMinus},
		},
		{
			name:   "unterminated string",
			input:  "\"hello",
			tokens: []TokenType{TypeString},
		},
		{
			name:   "unterminated comment",
			input:  "foo// this is a comment",
			tokens: []TokenType{TypeIdent},
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			s, err := NewScanner("test.in", strings.NewReader(tc.input))
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
