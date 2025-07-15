package ast

import (
	"fmt"
	"slices"
	"strings"
)

// AttrKey is a type for attribute keys.
type AttrKey string

const (
	AttrKeyExport   AttrKey = "export"
	AttrKeyExtern   AttrKey = "extern"
	AttrKeyBuiltin  AttrKey = "builtin"
	AttrKeyPrivate  AttrKey = "private"
	AttrKeyPure     AttrKey = "pure"
	AttrKeyLinkname AttrKey = "link_name"
	AttrKeyNoMangle AttrKey = "no_mangle"
)

var attrKeys = []AttrKey{
	AttrKeyExport,
	AttrKeyExtern,
	AttrKeyBuiltin,
	AttrKeyPrivate,
	AttrKeyPure,
	AttrKeyLinkname,
	AttrKeyNoMangle,
}

// ParseAttrKey validates and returns an AttrKey or an error if invalid.
func ParseAttrKey(s string) (AttrKey, bool) {
	if slices.Contains(attrKeys, AttrKey(s)) {
		return AttrKey(s), true
	}

	return AttrKey(s), false
}

// AttrValue is a union type for attribute values (string or int).
type AttrValue interface {
	Type() AttrValueType
}

type AttrValueType int

const (
	AttrStringType AttrValueType = iota
	AttrIntType
	AttrBoolType
)

type AttrString string

func (AttrString) Type() AttrValueType {
	return AttrStringType
}

type AttrInt int

func (AttrInt) Type() AttrValueType {
	return AttrIntType
}

type AttrBool bool

func (AttrBool) Type() AttrValueType {
	return AttrBoolType
}

type Attributes map[AttrKey]AttrValue

func (a Attributes) Has(key AttrKey) bool {
	_, exists := a[key]
	return exists
}

func (a Attributes) String() string {
	if len(a) == 0 {
		return "(attr)"
	}

	var attrs []string

	for k, v := range a {
		switch v := v.(type) {
		case AttrString:
			attrs = append(attrs, fmt.Sprintf("%s=%q", k, v))
		case AttrInt:
			attrs = append(attrs, fmt.Sprintf("%s=%d", k, v))
		case AttrBool:
			attrs = append(attrs, fmt.Sprintf("%s=%t", k, v))
		default:
			panic(fmt.Sprintf("unknown attribute value type: %T", v))
		}
	}

	return fmt.Sprintf("(attr %s)", strings.Join(attrs, " "))
}
