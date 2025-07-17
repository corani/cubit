package lexer

import "slices"

type Keyword string

const (
	KeywordFunc     Keyword = "func"
	KeywordReturn   Keyword = "return"
	KeywordInt      Keyword = "int"
	KeywordString   Keyword = "string"
	KeywordVoid     Keyword = "void"
	KeywordPackage  Keyword = "package"
	KeywordFalse    Keyword = "false"
	KeywordTrue     Keyword = "true"
	KeywordFor      Keyword = "for"
	KeywordIf       Keyword = "if"
	KeywordElse     Keyword = "else"
	KeywordBreak    Keyword = "break"
	KeywordContinue Keyword = "continue"
	KeywordIn       Keyword = "in"
	KeywordStruct   Keyword = "struct"
	KeywordEnum     Keyword = "enum"
	KeywordImport   Keyword = "import"
	KeywordAs       Keyword = "as"
	KeywordSwitch   Keyword = "switch"
	KeywordCase     Keyword = "case"
	KeywordDefault  Keyword = "default"
	KeywordNil      Keyword = "nil"
	KeywordBool     Keyword = "bool"
	KeywordAny      Keyword = "any"
)

var keywords = []Keyword{
	KeywordFunc,
	KeywordReturn,
	KeywordInt,
	KeywordString,
	KeywordVoid,
	KeywordPackage,
	KeywordFalse,
	KeywordTrue,
	KeywordAny,
	KeywordFor,
	KeywordIf,
	KeywordElse,
	KeywordBreak,
	KeywordContinue,
	KeywordIn,
	KeywordStruct,
	KeywordEnum,
	KeywordImport,
	KeywordAs,
	KeywordSwitch,
	KeywordCase,
	KeywordDefault,
	KeywordNil,
	KeywordBool,
}

func checkKeyword(ident string) (Keyword, bool) {
	if slices.Contains(keywords, Keyword(ident)) {
		return Keyword(ident), true
	}

	return "", false
}
