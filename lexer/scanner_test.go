package lexer

import (
	"bytes"
	"io"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestNewScannerAndNext(t *testing.T) {
	t.Parallel()

	input := []byte("abc\nxyz")
	s, err := NewScanner("test.txt", bytes.NewReader(input))
	require.NoError(t, err, "unexpected error")

	for i, expected := range input {
		b, err := s.Next()
		require.NoErrorf(t, err, "unexpected error at index %d", i)
		require.Equalf(t, expected, b, "expected %q, got %q at index %d", expected, b, i)
	}

	_, err = s.Next()
	require.ErrorIs(t, err, io.EOF, "expected EOF")
}

func TestUnread(t *testing.T) {
	t.Parallel()

	input := []byte("hello")
	s, err := NewScanner("test.txt", bytes.NewReader(input))
	require.NoError(t, err)

	b, err := s.Next()
	require.NoError(t, err)
	require.Equal(t, byte('h'), b, "expected 'h'")

	s.Unread(1)
	b, err = s.Next()
	require.NoError(t, err)
	require.Equal(t, byte('h'), b, "expected 'h' after unread")

	s.Unread(0) // should do nothing
	b, err = s.Next()
	require.NoError(t, err)
	require.Equal(t, byte('e'), b, "expected 'e'")

	s.Unread(10) // invalid, should do nothing
	b, err = s.Next()
	require.NoError(t, err)
	require.Equal(t, byte('l'), b, "expected 'l'")
}

func TestLocation(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name       string
		input      string
		steps      int
		filename   string
		wantString string
	}{
		{
			name:       "middle of second line",
			input:      "a\nbc\nde",
			steps:      4,
			filename:   "foo.txt",
			wantString: "foo.txt:2:2",
		},
		{
			name:       "start of file",
			input:      "abc",
			steps:      0,
			filename:   "bar.txt",
			wantString: "bar.txt:1:0",
		},
		{
			name:       "after newline",
			input:      "a\nb",
			steps:      2,
			filename:   "baz.txt",
			wantString: "baz.txt:2:0",
		},
		{
			name:       "end of file",
			input:      "x\ny\nz",
			steps:      5,
			filename:   "end.txt",
			wantString: "end.txt:3:1",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			s, err := NewScanner(tc.filename, bytes.NewReader([]byte(tc.input)))
			require.NoError(t, err)

			for i := 0; i < tc.steps; i++ {
				_, err := s.Next()
				require.NoError(t, err)
			}

			loc := s.Location()

			require.Equal(t, tc.wantString, loc.String(), "Location.String()")
		})
	}
}
