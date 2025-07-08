package lexer

import (
	"fmt"
	"strconv"
)

type TokenType string

const (
	TypeEOF       TokenType = "EOF"
	TypeIdent     TokenType = "Identifier"
	TypeKeyword   TokenType = "Keyword"
	TypeNumber    TokenType = "Number"
	TypeBool      TokenType = "Bool"         // "true" / "false"
	TypeString    TokenType = "String"       // Double-quoted string
	TypeLparen    TokenType = "LeftParen"    // "("
	TypeRparen    TokenType = "RightParen"   // ")"
	TypeLbrace    TokenType = "LeftBrace"    // "{"
	TypeRbrace    TokenType = "RightBrace"   // "}"
	TypeLBracket  TokenType = "LeftBracket"  // "["
	TypeRBracket  TokenType = "RightBracket" // "]"
	TypeDot       TokenType = "Dot"          // "."
	TypeComma     TokenType = "Comma"        // ","
	TypeArrow     TokenType = "Arrow"        // "->"
	TypeColon     TokenType = "Colon"        // ":"
	TypeSemicolon TokenType = "Semicolon"    // ";"
	TypeAt        TokenType = "At"           // "@"
	TypeAssign    TokenType = "Assign"       // ":="
	TypePlus      TokenType = "Plus"         // "+"
	TypeMinus     TokenType = "Minus"        // "-"
	TypeStar      TokenType = "Star"         // "*"
	TypeSlash     TokenType = "Slash"        // "/"
	TypeEq        TokenType = "Eq"           // "=="
	TypeNe        TokenType = "Ne"           // "!="
	TypeLt        TokenType = "Lt"           // "<"
	TypeLe        TokenType = "Le"           // "<="
	TypeGt        TokenType = "Gt"           // ">"
	TypeGe        TokenType = "Ge"           // ">="
	TypeDollar    TokenType = "Dollar"       // "$"
	TypeCaret     TokenType = "Caret"        // "^"
	TypeShl       TokenType = "ShiftLeft"    // "<<"
	TypeShr       TokenType = "ShiftRight"   // ">>"
	TypeBinAnd    TokenType = "BinaryAnd"    // "&"
	TypeBinOr     TokenType = "BinaryOr"     // "|"
	TypeLogAnd    TokenType = "LogicalAnd"   // "&&"
	TypeLogOr     TokenType = "LogicalOr"    // "||"
)

// Symbols is a map of string to TokenType for maximal munch.
var symbols = map[string]TokenType{
	"{":  TypeLbrace,
	"}":  TypeRbrace,
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

type Token struct {
	Type       TokenType
	Keyword    Keyword
	Identifier string
	StringVal  string
	NumberVal  int
	Location   Location
}

func NewStringToken(val string, location Location) (Token, error) {
	return Token{
		Type:      TypeString,
		StringVal: val,
		Location:  location,
	}, nil
}

func NewNumberToken(val string, location Location) (Token, error) {
	num, err := strconv.Atoi(string(val))
	if err != nil {
		return Token{}, err
	}

	return Token{
		Type:      TypeNumber,
		NumberVal: num,
		StringVal: string(val),
		Location:  location,
	}, nil
}

func NewIdentOrKeywordToken(val string, location Location) (Token, error) {
	kw, ok := checkKeyword(string(val))
	if !ok {
		// If it's not a keyword, it's an identifier.
		return Token{
			Type:       TypeIdent,
			Identifier: string(val),
			StringVal:  string(val),
			Location:   location,
		}, nil
	}

	switch kw {
	case KeywordFalse, KeywordTrue:
		// Turn keywords `true` and `false` into boolean literal tokens.
		return Token{
			Type:       TypeBool,
			Keyword:    kw,
			Identifier: string(val),
			StringVal:  string(val),
			Location:   location,
		}, nil
	default:
		return Token{
			Type:       TypeKeyword,
			Keyword:    kw,
			Identifier: string(val),
			StringVal:  string(val),
			Location:   location,
		}, nil
	}

}

func (t Token) String() string {
	switch t.Type {
	case TypeEOF:
		return fmt.Sprintf("%s: EOF", t.Location)
	case TypeIdent:
		return fmt.Sprintf("%s: Identifier(%s)", t.Location, t.Identifier)
	case TypeKeyword:
		return fmt.Sprintf("%s: Keyword(%s)", t.Location, t.Keyword)
	case TypeNumber:
		return fmt.Sprintf("%s: Number(%d)", t.Location, t.NumberVal)
	case TypeBool:
		return fmt.Sprintf("%s: Bool(%s)", t.Location, t.Keyword)
	case TypeString:
		return fmt.Sprintf("%s: String(%q)", t.Location, t.StringVal)
	case TypeLparen:
		return fmt.Sprintf("%s: LParen", t.Location)
	case TypeRparen:
		return fmt.Sprintf("%s: RParen", t.Location.String())
	case TypeLbrace:
		return fmt.Sprintf("%s: LBrace", t.Location.String())
	case TypeRbrace:
		return fmt.Sprintf("%s: RBrace", t.Location.String())
	case TypeLBracket:
		return fmt.Sprintf("%s: LBracket", t.Location.String())
	case TypeRBracket:
		return fmt.Sprintf("%s: RBracket", t.Location.String())
	case TypeDot:
		return fmt.Sprintf("%s: Dot", t.Location.String())
	case TypeComma:
		return fmt.Sprintf("%s: Comma", t.Location.String())
	case TypeArrow:
		return fmt.Sprintf("%s: Arrow", t.Location.String())
	case TypeColon:
		return fmt.Sprintf("%s: Colon", t.Location.String())
	case TypeSemicolon:
		return fmt.Sprintf("%s: Semicolon", t.Location.String())
	case TypeAt:
		return fmt.Sprintf("%s: At", t.Location.String())
	case TypeAssign:
		return fmt.Sprintf("%s: Assign", t.Location.String())
	case TypePlus:
		return fmt.Sprintf("%s: Plus", t.Location.String())
	case TypeMinus:
		return fmt.Sprintf("%s: Minus", t.Location.String())
	case TypeStar:
		return fmt.Sprintf("%s: Star", t.Location.String())
	case TypeSlash:
		return fmt.Sprintf("%s: Slash", t.Location.String())
	case TypeEq:
		return fmt.Sprintf("%s: Eq", t.Location.String())
	case TypeNe:
		return fmt.Sprintf("%s: Ne", t.Location.String())
	case TypeLt:
		return fmt.Sprintf("%s: Lt", t.Location.String())
	case TypeLe:
		return fmt.Sprintf("%s: Le", t.Location.String())
	case TypeGt:
		return fmt.Sprintf("%s: Gt", t.Location.String())
	case TypeGe:
		return fmt.Sprintf("%s: Ge", t.Location.String())
	case TypeDollar:
		return fmt.Sprintf("%s: Dollar", t.Location.String())
	case TypeCaret:
		return fmt.Sprintf("%s: Caret", t.Location.String())
	case TypeShl:
		return fmt.Sprintf("%s: ShiftLeft", t.Location.String())
	case TypeShr:
		return fmt.Sprintf("%s: ShiftRight", t.Location.String())
	case TypeBinAnd:
		return fmt.Sprintf("%s: BinaryAnd", t.Location.String())
	case TypeBinOr:
		return fmt.Sprintf("%s: BinaryOr", t.Location.String())
	case TypeLogAnd:
		return fmt.Sprintf("%s: LogicalAnd", t.Location.String())
	case TypeLogOr:
		return fmt.Sprintf("%s: LogicalOr", t.Location.String())
	default:
		return fmt.Sprintf("%s: Unknown", t.Location.String())
	}
}
