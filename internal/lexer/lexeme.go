package lexer

import (
	"strconv"
)

type TokenType string

const (
	TypeEOF        TokenType = "EOF"
	TypeIdent      TokenType = "Identifier"
	TypeKeyword    TokenType = "Keyword"
	TypeNumber     TokenType = "Number"
	TypeBool       TokenType = "Bool"         // "true" / "false"
	TypeString     TokenType = "String"       // Double-quoted string
	TypeLparen     TokenType = "LeftParen"    // "("
	TypeRparen     TokenType = "RightParen"   // ")"
	TypeLbrace     TokenType = "LeftBrace"    // "{"
	TypeRbrace     TokenType = "RightBrace"   // "}"
	TypeLBracket   TokenType = "LeftBracket"  // "["
	TypeRBracket   TokenType = "RightBracket" // "]"
	TypeDot        TokenType = "Dot"          // "."
	TypeDotDot     TokenType = "DotDot"       // ".." (varargs)
	TypeComma      TokenType = "Comma"        // ","
	TypeArrow      TokenType = "Arrow"        // "->"
	TypeColon      TokenType = "Colon"        // ":"
	TypeSemicolon  TokenType = "Semicolon"    // ";"
	TypeAt         TokenType = "At"           // "@"
	TypeAssign     TokenType = "Assign"       // ":="
	TypePlus       TokenType = "Plus"         // "+"
	TypeMinus      TokenType = "Minus"        // "-"
	TypeStar       TokenType = "Star"         // "*"
	TypeSlash      TokenType = "Slash"        // "/"
	TypePercent    TokenType = "Percent"      // "%"
	TypePlusAssign TokenType = "PlusAssign"   // "+="
	TypeAndAssign  TokenType = "AndAssign"    // "&="
	TypeEq         TokenType = "Eq"           // "=="
	TypeNe         TokenType = "Ne"           // "!="
	TypeLt         TokenType = "Lt"           // "<"
	TypeLe         TokenType = "Le"           // "<="
	TypeGt         TokenType = "Gt"           // ">"
	TypeGe         TokenType = "Ge"           // ">="
	TypeDollar     TokenType = "Dollar"       // "$"
	TypeCaret      TokenType = "Caret"        // "^"
	TypeShl        TokenType = "ShiftLeft"    // "<<"
	TypeShr        TokenType = "ShiftRight"   // ">>"
	TypeBinAnd     TokenType = "BinaryAnd"    // "&"
	TypeBinOr      TokenType = "BinaryOr"     // "|"
	TypeLogAnd     TokenType = "LogicalAnd"   // "&&"
	TypeLogOr      TokenType = "LogicalOr"    // "||"
)

// Symbols is a map of string to TokenType for maximal munch.
var symbols = map[string]TokenType{
	"{":  TypeLbrace,
	"}":  TypeRbrace,
	"..": TypeDotDot, // varargs
	".":  TypeDot,
	",":  TypeComma,
	":":  TypeColon,
	";":  TypeSemicolon,
	"@":  TypeAt,
	"+":  TypePlus,
	"*":  TypeStar,
	"%":  TypePercent,
	"$":  TypeDollar,
	"^":  TypeCaret,
	"+=": TypePlusAssign,
	"&=": TypeAndAssign,
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
