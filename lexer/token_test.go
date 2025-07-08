package lexer

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestNewStringToken(t *testing.T) {
	loc := Location{"file.go", 1, 2}
	tok, err := NewStringToken("hello", loc)
	require.NoError(t, err)
	require.Equal(t, TypeString, tok.Type)
	require.Equal(t, "hello", tok.StringVal)
	require.Equal(t, loc, tok.Location)
}

func TestNewNumberToken(t *testing.T) {
	loc := Location{"file.go", 2, 3}
	tok, err := NewNumberToken("123", loc)
	require.NoError(t, err)
	require.Equal(t, TypeNumber, tok.Type)
	require.Equal(t, 123, tok.NumberVal)
	require.Equal(t, "123", tok.StringVal)
	require.Equal(t, loc, tok.Location)

	_, err = NewNumberToken("notanumber", loc)
	require.Error(t, err)
}

func TestNewIdentOrKeywordToken(t *testing.T) {
	loc := Location{"file.go", 3, 4}

	tok, err := NewIdentOrKeywordToken("if", loc)
	require.NoError(t, err)
	require.Equal(t, TypeKeyword, tok.Type)
	require.Equal(t, KeywordIf, tok.Keyword)
	require.Equal(t, "if", tok.Identifier)
	require.Equal(t, "if", tok.StringVal)
	require.Equal(t, loc, tok.Location)

	tok, err = NewIdentOrKeywordToken("true", loc)
	require.NoError(t, err)
	require.Equal(t, TypeBool, tok.Type)
	require.Equal(t, KeywordTrue, tok.Keyword)
	require.Equal(t, "true", tok.Identifier)
	require.Equal(t, "true", tok.StringVal)
	require.Equal(t, loc, tok.Location)

	tok, err = NewIdentOrKeywordToken("foobar", loc)
	require.NoError(t, err)
	require.Equal(t, TypeIdent, tok.Type)
	require.Equal(t, "foobar", tok.Identifier)
	require.Equal(t, "foobar", tok.StringVal)
	require.Equal(t, loc, tok.Location)
}
