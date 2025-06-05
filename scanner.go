package main

import (
	"fmt"
	"io"
)

type Location struct {
	Filename     string
	Line, Column int
}

func (l Location) String() string {
	return fmt.Sprintf("%s:%d:%d", l.Filename, l.Line, l.Column)
}

type scanner struct {
	filename string
	data     []byte
	index    int
}

func NewScanner(filename string, r io.Reader) (*scanner, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}

	return &scanner{
		filename: filename,
		data:     data,
		index:    0,
	}, nil
}

func (s *scanner) Next() (byte, error) {
	if s.index >= len(s.data) {
		return 0, io.EOF
	}

	b := s.data[s.index]
	s.index++

	return b, nil
}

func (s *scanner) Unread(count int) {
	if count < 0 || count > s.index {
		return // Invalid count, do nothing
	}

	s.index -= count

	if s.index < 0 {
		s.index = 0 // Ensure index does not go negative
	}
}

func (s *scanner) Location() Location {
	loc := Location{
		Filename: s.filename,
		Line:     1,
		Column:   0,
	}

	// calculate the line/column based on the current index
	for i := range s.index {
		if s.data[i] == '\n' {
			loc.Line++
			loc.Column = 0
		} else {
			loc.Column++
		}
	}

	return loc
}
