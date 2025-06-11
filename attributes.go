package main

import "fmt"

// AttrKey is a type for attribute keys.
type AttrKey string

const (
	AttrKeyExport AttrKey = "export"
	AttrKeyExtern AttrKey = "extern"
)

// ParseAttrKey validates and returns an AttrKey or an error if invalid.
func ParseAttrKey(s string) (AttrKey, error) {
	switch s {
	case string(AttrKeyExport):
		return AttrKeyExport, nil
	case string(AttrKeyExtern):
		return AttrKeyExtern, nil
	default:
		return "", fmt.Errorf("invalid attribute key: %s", s)
	}
}

// AttrValue is a union type for attribute values (string or int).
type AttrValue interface {
	Type() AttrValueType
}

type AttrValueType int

const (
	AttrStringType AttrValueType = iota
	AttrIntType
)

type AttrString string

func (AttrString) Type() AttrValueType {
	return AttrStringType
}

type AttrInt int

func (AttrInt) Type() AttrValueType {
	return AttrIntType
}
