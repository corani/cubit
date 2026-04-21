package parser

import (
	"fmt"
	"io"
	"maps"
	"slices"
	"strings"

	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
)

type Parser struct {
	tok                   []lexer.Token
	index                 int
	unit                  *ast.CompilationUnit
	attributes            ast.Attributes
	localID               int
	currentRetType        *ast.Type
	implicitGenericParams []*ast.GenericParam // non-nil while parsing a function param list
}

func New(tok []lexer.Token) *Parser {
	var location lexer.Location

	// Initial location, we'll update this to the location of 'package' later.
	if len(tok) > 0 {
		location = tok[0].Location
	}

	// TODO(daniel): instead of accepting all tokens, maybe we should accept a
	// lexer and pull in the tokens on demand.
	return &Parser{
		tok:                   tok,
		index:                 0,
		unit:                  ast.NewCompilationUnit(location),
		attributes:            ast.Attributes{},
		localID:               0,
		currentRetType:        nil,
		implicitGenericParams: nil,
	}
}

//nolint:gocognit,cyclop
func (p *Parser) Parse() (*ast.CompilationUnit, error) {
	for {
		start, err := p.expectType(lexer.TypeKeyword, lexer.TypeIdent, lexer.TypeAt)
		if err != nil {
			return p.unit, err // EOF
		}

		//nolint:exhaustive
		switch start.Type {
		case lexer.TypeAt:
			if err := p.parseAttributes(start); err != nil {
				return p.unit, err // EOF
			}
		case lexer.TypeKeyword:
			switch start.Keyword {
			case lexer.KeywordPackage:
				if err := p.parsePackage(start); err != nil {
					return p.unit, err // EOF
				}
			case lexer.KeywordImport:
				if err := p.parseImport(start); err != nil {
					return p.unit, err // EOF
				}
			default:
				start.Location.Errorf("expected keyword 'package', got %s",
					start.StringVal)

				// TODO: error recovery
				return p.unit, fmt.Errorf("expected keyword 'package' at %s, got %s",
					start.Location, start.StringVal)
			}
		case lexer.TypeIdent:
			if p.unit.Ident == "" {
				start.Location.Errorf("package must be defined before any other declarations")

				// error recovery: just continue parsing
			}

			if _, err := p.expectType(lexer.TypeColon); err != nil {
				return p.unit, err // EOF
			}

			// TODO(daniel): parse optional type.

			if _, err := p.expectType(lexer.TypeColon); err != nil {
				return p.unit, err // EOF
			}

			if _, err := p.expectKeyword(lexer.KeywordFunc); err != nil {
				return p.unit, err // EOF
			}

			if err := p.parseFunc(start); err != nil {
				return p.unit, err
			}
		}
	}
}

func (p *Parser) parseImport(start lexer.Token) error {
	_ = start

	if p.unit.Ident == "" {
		start.Location.Errorf("package must be defined before imports")

		// error recovery: just continue parsing
	}

	pkgName, err := p.expectType(lexer.TypeString)
	if err != nil {
		return err // EOF
	}

	alias := pkgName.StringVal

	// parse optional "as as"
	as, err := p.peekKeyword(lexer.KeywordAs)
	if err != nil {
		return err // EOF
	}

	if as.Keyword == lexer.KeywordAs {
		name, err := p.expectType(lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}

		alias = name.StringVal
	}

	if orig, ok := p.unit.Imports[alias]; ok {
		start.Location.Errorf("import %s already defined as %s, cannot redefine",
			alias, orig.Name)
		p.unit.Loc.Infof("previous definition was here")

		// error recovery: just ignore the new import.
	} else {
		p.unit.Imports[alias] = ast.Import{
			Name: pkgName.StringVal,
			Unit: nil, // will be filled by loader
		}
	}

	if _, err := p.expectType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	return nil
}

// parsePackage parses a package declaration.
// It returns io.EOF when there are no more tokens.
func (p *Parser) parsePackage(start lexer.Token) error {
	if p.unit.Ident != "" {
		start.Location.Errorf("package already defined, cannot redefine")
		p.unit.Loc.Infof("previous definition was here")

		// error recovery: just ignore the new package definition.
		_, err := p.expectType(lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}
	} else {
		pkgName, err := p.expectType(lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}

		// Store any attributes collected before the package in the unit's Attributes
		p.unit.Attributes = maps.Clone(p.attributes)
		p.unit.Ident = pkgName.StringVal
		p.unit.Loc = start.Location
	}

	if _, err := p.expectType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	clear(p.attributes)

	return nil
}

// parseAttributes parses attributes in the form `@(...)`.
// It returns io.EOF when there are no more tokens.
//
//nolint:cyclop,funlen
func (p *Parser) parseAttributes(atToken lexer.Token) error {
	_ = atToken

	lparen, err := p.expectType(lexer.TypeLparen)
	if err != nil {
		return err // EOF
	}

	if lparen.Type != lexer.TypeLparen {
		lparen.Location.Errorf("expected ( after @, got %s", lparen.StringVal)

		// TODO: error recovery
	}

	for {
		tok, err := p.expectType(lexer.TypeRparen, lexer.TypeIdent)
		if err != nil {
			return err // EOF
		}

		if tok.Type == lexer.TypeRparen {
			break
		}

		//nolint:varnamelen // ok
		key, ok := ast.ParseAttrKey(tok.StringVal)
		if !ok {
			tok.Location.Errorf("invalid attribute key: %s", tok.StringVal)
		}

		value := ast.AttrValue(ast.AttrBool(true))

		next, err := p.expectType(lexer.TypeAssign, lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err // EOF
		}

		if next.Type == lexer.TypeAssign {
			valTok, err := p.expectType(lexer.TypeString, lexer.TypeNumber)
			if err != nil {
				return err // EOF
			}

			//nolint:exhaustive
			switch valTok.Type {
			case lexer.TypeString:
				value = ast.AttrString(valTok.StringVal)
			case lexer.TypeNumber:
				value = ast.AttrInt(valTok.NumberVal)
			}

			next, err = p.expectType(lexer.TypeComma, lexer.TypeRparen)
			if err != nil {
				return err // EOF
			}
		}

		// ignore invalid attributes
		if ok {
			p.attributes[key] = value
		}

		if next.Type == lexer.TypeRparen {
			break
		}
	}

	// parse optional semicolon
	if _, err := p.expectType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	return nil
}

// declareOrReferenceGenericParam registers a generic parameter in the implicit
// accumulator if not already present (by symbol name), or returns the existing
// one. Returns the param (new or existing).
func (p *Parser) declareOrReferenceGenericParam(gp *ast.GenericParam) {
	if !slices.ContainsFunc(p.implicitGenericParams,
		func(existing *ast.GenericParam) bool {
			return existing.Symbol == gp.Symbol
		}) {
		p.implicitGenericParams = append(p.implicitGenericParams, gp)
	}
}

//nolint:cyclop,funlen
func (p *Parser) parseFunc(name lexer.Token) error {
	if _, err := p.expectType(lexer.TypeLparen); err != nil {
		return err // EOF
	}

	def := ast.NewFuncDef(name.StringVal, p.attributes, name.Location)
	clear(p.attributes)

	// Activate the implicit generic param accumulator for this function.
	// Both explicit ($NAME: kind) and inline ($NAME in type positions) params
	// are collected here and merged into def.GenericParams after the loop.
	p.implicitGenericParams = []*ast.GenericParam{}

	defer func() { p.implicitGenericParams = nil }()

	for {
		// Generic parameters start with '$'
		if tok, err := p.peekType(lexer.TypeDollar); err == nil && tok.Type == lexer.TypeDollar {
			gp, err := p.parseGenericParam()
			if err != nil {
				return err
			}

			p.declareOrReferenceGenericParam(gp)
		} else {
			param, err := p.parseFuncParam()
			if err != nil {
				return err
			}

			if param == nil {
				break
			}

			def.Params = append(def.Params, param)
		}

		tok, err := p.expectType(lexer.TypeComma, lexer.TypeRparen)
		if err != nil {
			return err // EOF
		}

		if tok.Type == lexer.TypeRparen {
			break
		}
	}

	// Merge accumulated generic params (explicit + implicit) into the def.
	def.GenericParams = p.implicitGenericParams

	retType, err := p.parseFuncReturnType()
	if err != nil {
		name.Location.Errorf("error parsing return type: %v", err)

		// error recovery:
		retType = ast.NewType(ast.TypeVoid, name.Location)
	}

	p.currentRetType = retType
	def.ReturnType = retType

	//nolint:nestif
	if def.Attributes.Has(ast.AttrKeyExtern) || def.Attributes.Has(ast.AttrKeyBuiltin) {
		// If the function is extern or builtin, we don't parse the body.
	} else {
		lbrace, err := p.expectType(lexer.TypeLbrace)
		if err != nil {
			return err // EOF
		}

		instructions, err := p.parseBlock(lbrace)
		if err != nil {
			return err
		}

		// Add implicit return if needed
		var addRet bool

		if len(instructions) == 0 {
			addRet = true
		} else {
			_, hasRet := instructions[len(instructions)-1].(*ast.Return)
			addRet = !hasRet
		}

		if addRet {
			//nolint:exhaustive
			switch retType.Kind {
			case ast.TypeVoid:
				// If the return type is void, we can just add an empty return.
				instructions = append(instructions, ast.NewReturn(lbrace.Location, retType))
			default:
				name.Location.Errorf("function %s has return type %s but no return statement",
					def.Ident, retType.String())

				// error recovery:
				instructions = append(instructions, ast.NewReturn(lbrace.Location, retType))
			}
		}

		if _, err := p.expectType(lexer.TypeRbrace); err != nil {
			return err // EOF
		}

		def.Body = ast.NewBody(instructions, lbrace.Location)
	}

	p.unit.Funcs = append(p.unit.Funcs, def)

	if _, err := p.peekType(lexer.TypeSemicolon); err != nil {
		return err // EOF
	}

	return nil
}

//nolint:cyclop
func (p *Parser) parseFuncParam() (*ast.FuncParam, error) {
	// Check for optional attributes before parameter
	var attrs ast.Attributes

	nextTok, err := p.expectType(lexer.TypeRparen, lexer.TypeAt, lexer.TypeIdent)
	if err != nil {
		return nil, err // EOF
	}

	if nextTok.Type == lexer.TypeRparen {
		// Empty parameter list.
		//
		//nolint:nilnil
		return nil, nil
	}

	if nextTok.Type == lexer.TypeAt {
		// Parse parameter attributes
		if err := p.parseAttributes(nextTok); err != nil {
			return nil, err // EOF
		}

		// Copy and clear parser attributes for this param
		attrs = maps.Clone(p.attributes)
		clear(p.attributes)

		// Now expect identifier
		nextTok, err = p.expectType(lexer.TypeIdent)
		if err != nil {
			return nil, err // EOF
		}
	}

	if _, err := p.expectType(lexer.TypeColon); err != nil {
		return nil, err // EOF
	}

	equal, err := p.peekType(lexer.TypeAssign)
	if err != nil {
		return nil, err // EOF
	}

	var paramType *ast.Type

	if equal.Type != lexer.TypeAssign {
		paramType = p.parseParamType()

		equal, err = p.peekType(lexer.TypeAssign)
		if err != nil {
			return nil, err // EOF
		}
	} else {
		paramType = ast.NewType(ast.TypeUnknown, equal.Location)
	}

	var value ast.Expression

	if equal.Type == lexer.TypeAssign {
		// If we have an equals sign, we expect a default value
		value, err = p.parseExpression(false)
		if err != nil {
			return nil, err
		}
	}

	return ast.NewFuncParam(nextTok.StringVal, paramType, value,
		attrs, nextTok.Location), nil
}

// parseGenericParam parses a single generic parameter: $NAME: type or $NAME: int
// The leading '$' has already been consumed by the caller.
func (p *Parser) parseGenericParam() (*ast.GenericParam, error) {
	nameTok, err := p.expectType(lexer.TypeIdent)
	if err != nil {
		_ = nameTok.Location.Errorf("expected identifier after '$'")

		return nil, err
	}

	if _, err := p.expectType(lexer.TypeColon); err != nil {
		_ = nameTok.Location.Errorf("expected ':' after generic parameter name '$%s'", nameTok.StringVal)

		return nil, err
	}

	kindTok, err := p.expectType(lexer.TypeKeyword)
	if err != nil {
		_ = nameTok.Location.Errorf("expected 'type' or a type keyword after ':'")

		return nil, err
	}

	//nolint:exhaustive
	switch kindTok.Keyword {
	case lexer.KeywordType:
		return ast.NewGenericParamType(nameTok.StringVal), nil
	case lexer.KeywordInt:
		return ast.NewGenericParamValue(nameTok.StringVal, ast.NewType(ast.TypeInt, kindTok.Location)), nil
	default:
		kindTok.Location.Errorf("unsupported generic parameter kind '%s': expected 'type' or 'int'", kindTok.Keyword)

		return ast.NewGenericParamType(nameTok.StringVal), nil // error recovery
	}
}

// parseParamType parses a parameter type, supporting varargs (..type).
func (p *Parser) parseParamType() *ast.Type {
	// Check for vararg prefix
	dotdot, _ := p.peekType(lexer.TypeDotDot)

	// Parse the actual type
	//
	//nolint:varnamelen
	ty := p.parseType()

	if dotdot.Type == lexer.TypeDotDot {
		return ast.NewVarargType(ty, ty.Location())
	}

	// If not vararg, just return the type
	return ty
}

// parseFuncReturnType parses the return type of a function.
// If the return type is not specified, it defaults to void.
// It returns io.EOF when there are no more tokens.
//
//nolint:unparam // error is always nil
func (p *Parser) parseFuncReturnType() (*ast.Type, error) {
	arrow, err := p.peekType(lexer.TypeArrow)
	if err != nil {
		//nolint:nilerr,nilnil // EOF is not an error here
		return nil, nil
	}

	if arrow.Type == lexer.TypeArrow {
		return p.parseType(), nil
	}

	// Default to void
	return ast.NewType(ast.TypeVoid, arrow.Location), nil
}

//nolint:gocognit,gocyclo,cyclop,funlen
func (p *Parser) parseBlock(start lexer.Token) ([]ast.Instruction, error) {
	_ = start

	var instructions []ast.Instruction

	for {
		first, err := p.nextToken()
		if err != nil {
			return nil, err // EOF
		}

		//nolint:exhaustive
		switch first.Type {
		case lexer.TypeRbrace:
			// End of block
			p.index--

			return instructions, nil
		case lexer.TypeSemicolon:
			// Empty statement, just continue
			continue
		case lexer.TypeKeyword:
			switch first.Keyword {
			case lexer.KeywordReturn:
				inst, err := p.parseReturn(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			case lexer.KeywordIf:
				inst, err := p.parseIf(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			case lexer.KeywordFor:
				inst, err := p.parseFor(first)
				if err != nil {
					return nil, err
				}

				instructions = append(instructions, inst)
			}
		case lexer.TypeIdent, lexer.TypeLparen:
			// TODO(daniel): fix duplication of code here with parseAssignOrDeclare
			// Try to parse a declaration (ident : ...)
			if first.Type == lexer.TypeIdent {
				next, err := p.peekType(lexer.TypeColon)
				if err != nil {
					return nil, err // EOF
				}

				if next.Type == lexer.TypeColon {
					instr, err := p.parseDeclare(first)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, instr...)

					continue
				}
			}

			// Otherwise, try to parse an lvalue expression followed by '='
			p.index-- // Unconsume first token

			lvalueExpr, err := p.parseLValue()
			if err == nil { //nolint:nestif
				tokenToBinop := map[lexer.TokenType]ast.BinOpKind{
					lexer.TypePlusAssign:    ast.BinOpAdd,
					lexer.TypeMinusAssign:   ast.BinOpSub,
					lexer.TypeStarAssign:    ast.BinOpMul,
					lexer.TypeSlashAssign:   ast.BinOpDiv,
					lexer.TypePercentAssign: ast.BinOpMod,
					lexer.TypeAndAssign:     ast.BinOpAnd,
					lexer.TypeOrAssign:      ast.BinOpOr,
				}

				acceptedTokens := []lexer.TokenType{lexer.TypeAssign}

				for k := range tokenToBinop {
					acceptedTokens = append(acceptedTokens, k)
				}

				next, err := p.peekType(acceptedTokens...)
				if err != nil {
					return nil, err // EOF
				}

				switch next.Type {
				case lexer.TypeAssign:
					instr, err := p.parseAssign(lvalueExpr)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, instr...)

					continue
				default:
					if binop, ok := tokenToBinop[next.Type]; ok {
						// If we have a binop assignment, parse it
						instr, err := p.parseAssignWithOp(lvalueExpr, binop)
						if err != nil {
							return nil, err
						}

						instructions = append(instructions, instr...)

						continue
					}
				}
			}

			// If not assignment, try to parse as a function call (ident(...))
			if first.Type == lexer.TypeIdent { //nolint:nestif
				namespace := ""

				dot, err := p.peekType(lexer.TypeDot)
				if err != nil {
					return nil, err // EOF
				}

				if dot.Type == lexer.TypeDot {
					namespace = first.StringVal

					first, err = p.expectType(lexer.TypeIdent)
					if err != nil {
						return nil, err // EOF
					}
				}

				next, err := p.peekType(lexer.TypeLparen)
				if err != nil {
					return nil, err // EOF
				}

				if next.Type == lexer.TypeLparen {
					inst, err := p.parseCall(namespace, first)
					if err != nil {
						return nil, err
					}

					instructions = append(instructions, inst)

					continue
				}
			}

			first.Location.Errorf("expected statement, got %s", first.StringVal)

			// TODO: error recovery
			return nil, fmt.Errorf("unexpected statement at %s", first.Location)
		}
	}
}

// parseType parses a type, supporting arbitrary nesting of arrays and pointers
// (e.g., [N]^int, ^[N]int, [N][M]^int, etc.)
func (p *Parser) parseType() *ast.Type {
	typeModifier := []func(*ast.Type) *ast.Type{}

	for {
		// Pointer(s)
		if tok, err := p.peekType(lexer.TypeCaret); err == nil && tok.Type == lexer.TypeCaret {
			loc := tok.Location // TODO(daniel): I think this is not needed?

			typeModifier = append(typeModifier, func(inner *ast.Type) *ast.Type {
				return ast.NewPointerType(inner, 1, loc)
			})

			continue
		}

		// Array(s)
		if modifier, ok := p.parseArrayModifier(); ok {
			typeModifier = append(typeModifier, modifier)

			continue
		}

		break
	}

	base := p.parseBaseType()

	// Apply modifiers in reverse order (so the base type is wrapped by the last modifier parsed)
	for i := len(typeModifier) - 1; i >= 0; i-- {
		base = typeModifier[i](base)
	}

	return base
}

// parseArraySize parses the size expression inside "[...]": either a literal
// number or a generic symbol ("$N" or "$N: int"). Called after "[" is consumed.
// When implicitGenericParams is active, registers any new generic param.
func (p *Parser) parseArraySize(lbracketLoc lexer.Location) *ast.Size {
	dollarTok, err := p.peekType(lexer.TypeDollar)
	if err != nil {
		return nil // EOF
	}

	// Literal size: [N]
	if dollarTok.Type != lexer.TypeDollar {
		sizeTok, err := p.expectType(lexer.TypeNumber)
		if err != nil {
			_ = lbracketLoc.Errorf("expected array size after '['")
			sizeTok.NumberVal = 0
		}

		return ast.NewSizeLiteral(sizeTok.NumberVal)
	}

	// Generic size: [$N] or [$N: int] — '$' already consumed by peekType
	symTok, err := p.expectType(lexer.TypeIdent)
	if err != nil {
		_ = lbracketLoc.Errorf("expected identifier after '$' in array size")
		symTok.StringVal = "_"
	}

	if p.implicitGenericParams != nil {
		if colonTok, err := p.peekType(lexer.TypeColon); err == nil && colonTok.Type == lexer.TypeColon {
			// [$N: int] — GenericValue param
			kindTok, err := p.expectType(lexer.TypeKeyword)
			if err != nil || kindTok.Keyword != lexer.KeywordInt {
				_ = colonTok.Location.Errorf("expected 'int' after ':' in generic array size '$%s'", symTok.StringVal)
			}

			p.declareOrReferenceGenericParam(
				ast.NewGenericParamValue(symTok.StringVal, ast.NewType(ast.TypeInt, kindTok.Location)),
			)
		} else {
			// [$N] — GenericType param (type-checker will catch invalid use as size)
			p.declareOrReferenceGenericParam(ast.NewGenericParamType(symTok.StringVal))
		}
	}

	return ast.NewSizeSymbol(symTok.StringVal)
}

// parseArrayModifier attempts to parse an array type prefix ("[N]" or "[$N]" or "[$N: int]").
// If matched, returns the modifier closure and true. If "[" is not next, returns nil, false.
func (p *Parser) parseArrayModifier() (func(*ast.Type) *ast.Type, bool) {
	tok, err := p.peekType(lexer.TypeLBracket)
	if err != nil || tok.Type != lexer.TypeLBracket {
		return nil, false
	}

	size := p.parseArraySize(tok.Location)

	if _, err := p.expectType(lexer.TypeRBracket); err != nil {
		tok.Location.Errorf("expected ']' after array size")
	}

	loc := tok.Location
	sizeCopy := size

	return func(inner *ast.Type) *ast.Type {
		return ast.NewArrayType(inner, sizeCopy, loc)
	}, true
}

// parseBaseType parses the base type (int, bool, string, void, etc.)
//
//nolint:cyclop
func (p *Parser) parseBaseType() *ast.Type {
	// Check for generic type reference: $T ($ already consumed by peekType)
	if dollarTok, err := p.peekType(lexer.TypeDollar); err == nil && dollarTok.Type == lexer.TypeDollar {
		symTok, err := p.expectType(lexer.TypeIdent)
		if err != nil {
			_ = dollarTok.Location.Errorf("expected identifier after '$' in type position")
			symTok.StringVal = "_"
		}

		// Register in the implicit accumulator if active.
		if p.implicitGenericParams != nil {
			p.declareOrReferenceGenericParam(ast.NewGenericParamType(symTok.StringVal))
		}

		return ast.NewGenericType(symTok.StringVal, dollarTok.Location)
	}

	tok, err := p.expectType(lexer.TypeKeyword)
	if err != nil {
		tok.Location.Errorf("expected type keyword, got %s", tok.Type)

		// error recover:
		//
		//nolint:exhaustruct
		tok = lexer.Token{
			Type:     lexer.TypeKeyword,
			Keyword:  lexer.KeywordVoid,
			Location: tok.Location, // FIXME(daniel): pass in the default location?
		}
	}

	//nolint:exhaustive
	switch tok.Keyword {
	case lexer.KeywordInt:
		return ast.NewType(ast.TypeInt, tok.Location)
	case lexer.KeywordString:
		return ast.NewType(ast.TypeString, tok.Location)
	case lexer.KeywordBool:
		return ast.NewType(ast.TypeBool, tok.Location)
	case lexer.KeywordVoid:
		return ast.NewType(ast.TypeVoid, tok.Location)
	case lexer.KeywordAny:
		return ast.NewType(ast.TypeAny, tok.Location)
	default:
		tok.Location.Errorf("unexpected type keyword %s", tok.Keyword)

		// error recovery:
		return ast.NewType(ast.TypeVoid, tok.Location)
	}
}

func (p *Parser) peekKeyword(kws ...lexer.Keyword) (lexer.Token, error) {
	tok, err := p.peekType(lexer.TypeKeyword)
	if err != nil {
		return tok, err
	}

	if tok.Type != lexer.TypeKeyword {
		// Token was not consumed in peekType
		return tok, err
	}

	if slices.Contains(kws, tok.Keyword) {
		return tok, nil
	}

	// If we reach here, the token is not one of the expected keywords. Unread it.
	p.index--

	return tok, nil
}

// expectKeyword checks if the next token is one of the expected keywords.
// If it is, it consumes and returns the token. Otherwise it returns the first
// expected keyword as a fallback and records an error.
// It returns io.EOF when there are no more tokens.
func (p *Parser) expectKeyword(kws ...lexer.Keyword) (lexer.Token, error) {
	token, err := p.nextToken()
	if err != nil {
		return token, err
	}

	if token.Type != lexer.TypeKeyword {
		token.Location.Errorf("expected keyword, got %s", token.Type)

		// error recovery:
		//
		//nolint:exhaustruct
		return lexer.Token{
			Type:       lexer.TypeKeyword,
			Keyword:    kws[0], // Return the first keyword as a fallback
			Identifier: string(kws[0]),
			StringVal:  string(kws[0]),
			Location:   token.Location, // Use the current token's location
		}, nil
	}

	var kwnames []string

	for _, kw := range kws {
		kwnames = append(kwnames, string(kw))

		if token.Keyword == kw {
			return token, nil
		}
	}

	token.Location.Errorf("expected %s, got %s", strings.Join(kwnames, " or "), token.Keyword)

	// error recovery:
	//
	//nolint:exhaustruct
	return lexer.Token{
		Type:       lexer.TypeKeyword,
		Keyword:    kws[0], // Return the first keyword as a fallback
		Identifier: string(kws[0]),
		StringVal:  string(kws[0]),
		Location:   token.Location, // Use the current token's location
	}, nil
}

// peekType checks if the next token is of the expected type(s). If it is, it
// consumes and returns the token. Otherwise it doesn't consume the token.
// It returns io.EOF when there are no more tokens.
func (p *Parser) peekType(tts ...lexer.TokenType) (lexer.Token, error) {
	token, err := p.nextToken()
	if err != nil {
		return token, err
	}

	if slices.Contains(tts, token.Type) {
		return token, nil
	}

	// If we reach here, the token was not of the expected type(s)
	p.index--

	return token, nil
}

// expectType checks if the next token is of the expected type(s). If it is, it
// consumes and returns the token. Otherwise it returns the first expected type
// as a fallback and records an error.
// It returns io.EOF when there are no more tokens.
func (p *Parser) expectType(tts ...lexer.TokenType) (lexer.Token, error) {
	token, err := p.nextToken()
	if err != nil {
		return token, err
	}

	var ttnames []string

	for _, tt := range tts {
		ttnames = append(ttnames, string(tt))
		if token.Type == tt {
			return token, nil
		}
	}

	token.Location.Errorf("expected %s, got %s", strings.Join(ttnames, " or "), token.Type)

	// error recover:
	p.index--

	//nolint:exhaustruct
	return lexer.Token{
		Type:     tts[0],
		Location: token.Location,
	}, nil
}

// nextToken retrieves the next token from the parser's token stream.
// It returns io.EOF when there are no more tokens.
func (p *Parser) nextToken() (lexer.Token, error) {
	if p.index >= len(p.tok) {
		return lexer.Token{}, io.EOF
	}

	token := p.tok[p.index]
	p.index++

	return token, nil
}
