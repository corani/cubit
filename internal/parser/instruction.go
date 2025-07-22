package parser

import (
	"github.com/corani/cubit/internal/ast"
	"github.com/corani/cubit/internal/lexer"
)

func (p *Parser) parseReturn(first lexer.Token) (ast.Instruction, error) {
	var (
		expr ast.Expression
		err  error
	)

	if p.currentRetType.Kind != ast.TypeVoid {
		expr, err = p.parseExpression(false)
		if err != nil {
			return nil, err
		}
	}

	return ast.NewReturn(first.Location, p.currentRetType, expr), nil
}

func (p *Parser) parseDeclare(ident lexer.Token) ([]ast.Instruction, error) {
	// <indent> ':'
	// have been consumed already.
	var instructions []ast.Instruction

	// Could be a declaration or declaration+assignment
	next, err := p.peekType(lexer.TypeAssign, lexer.TypeKeyword, lexer.TypeCaret, lexer.TypeLBracket)
	if err != nil {
		return nil, err // EOF
	}

	declaredType := ast.NewType(ast.TypeUnknown, ident.Location)

	// type
	if next.Type != lexer.TypeAssign {
		p.index--

		declaredType = p.parseType()

		next, err = p.peekType(lexer.TypeAssign)
		if err != nil {
			return nil, err // EOF
		}
	}

	instructions = append(instructions,
		ast.NewDeclare(ident.StringVal, declaredType, ident.Location))

	// optional assignment
	if next.Type == lexer.TypeAssign {
		lvalue := ast.NewVariableRef("", ident.StringVal, declaredType.Kind, ident.Location)

		instr, err := p.parseAssign(lvalue)
		if err != nil {
			return nil, err
		}

		instructions = append(instructions, instr...)
	}

	return instructions, nil
}

func (p *Parser) parseAssignOrDeclare(allowDeclaration bool) ([]ast.Instruction, bool, error) {
	// TODO(daniel): fix duplication of code with parseBlock
	first, err := p.expectType(lexer.TypeIdent)
	if err != nil {
		return nil, false, err // EOF
	}

	// If it's an identifier, it might be a declaration.
	next, err := p.peekType(lexer.TypeColon)
	if err != nil {
		return nil, false, err // EOF
	}

	if next.Type == lexer.TypeColon {
		if !allowDeclaration {
			next.Location.Errorf("declarations not allowed here")

			// error recovery: continue parsing.
		}

		// It's `v : type`, `v : type = expr` or `v := expr`, other assignments
		// like `+=` are not allowed here.
		instr, err := p.parseDeclare(first)
		if err != nil {
			return nil, false, err
		}

		return instr, true, nil
	}

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

	// Peek for assignment operator
	next, err = p.peekType(acceptedTokens...)
	if err != nil {
		return nil, false, nil
	}

	switch next.Type {
	case lexer.TypeAssign:
		lvalue := ast.NewVariableRef("", first.StringVal, ast.TypeUnknown, first.Location)

		instrs, err := p.parseAssign(lvalue)

		return instrs, true, err
	default:
		// Check for assignment with operator, e.g., +=, -=, etc.
		binop, ok := tokenToBinop[next.Type]
		if !ok {
			return nil, false, nil
		}

		lvalue := ast.NewVariableRef("", first.StringVal, ast.TypeUnknown, first.Location)

		instrs, err := p.parseAssignWithOp(lvalue, binop)

		return instrs, true, err
	}
}

// parseAssign now accepts an LValue (e.g., variable ref, deref, etc.)
func (p *Parser) parseAssign(lhs ast.LValue) ([]ast.Instruction, error) {
	// <lvalue> '=' or <lvalue> ':' <type> '=' or <lvalue> ':='
	// have been consumed already.
	var instructions []ast.Instruction

	expr, err := p.parseExpression(false)
	if err != nil {
		return nil, err
	}

	// TODO(daniel): figure out how to get rid of this check, semicolon should be
	// consumed higher up.
	if _, err := p.peekType(lexer.TypeSemicolon); err != nil {
		return nil, err // EOF
	}

	instructions = append(instructions,
		ast.NewAssign(lhs, expr, nil, lhs.Location()))

	return instructions, nil
}

func (p *Parser) parseAssignWithOp(lhs ast.LValue, op ast.BinOpKind) ([]ast.Instruction, error) {
	// <lvalue> op= <expr>
	// have been consumed already.
	var instructions []ast.Instruction

	rhs, err := p.parseExpression(false)
	if err != nil {
		return nil, err
	}

	binop := ast.NewBinop(op, lhs, rhs, lhs.Location())
	assign := ast.NewAssign(lhs, binop, nil, lhs.Location())

	instructions = append(instructions, assign)

	return instructions, nil
}

// parseCall parses the argument list of a function call. It expects `first` to be the identifier
// of the function being called. The left-parenthesis `(` should have already been consumed. It
// parses a comma-separated list of expressions until it encounters a right-parenthesis `)`.
func (p *Parser) parseCall(first lexer.Token) (*ast.Call, error) {
	var (
		args []ast.Arg
		next lexer.Token
	)

	for next.Type != lexer.TypeRparen {
		expr, err := p.parseExpression(true)
		if err != nil {
			return nil, err
		}

		if expr != nil {
			// We successfully parsed an expression, this should be followed by either
			// a comma or a right parenthesis.
			args = append(args, ast.NewArg("", expr, nil, expr.Location()))

			next, err = p.expectType(lexer.TypeRparen, lexer.TypeComma)
			if err != nil {
				return nil, err // EOF
			}
		} else {
			// We didn't parse an expression, so we expect a right parenthesis to form `()`.
			next, err = p.expectType(lexer.TypeRparen)
			if err != nil {
				return nil, err // EOF
			}
		}
	}

	return ast.NewCall(first.Location, "", first.StringVal, args...), nil
}

// parseIf parses an if/else statement.
func (p *Parser) parseIf(first lexer.Token) (ast.Instruction, error) {
	// Expect 'if' keyword already consumed
	var initInstrs []ast.Instruction

	checkpoint := p.index

	// Check for optional initializer: ident : type = expr or ident = expr
	instr, ok, err := p.parseAssignOrDeclare(true)
	if err != nil {
		return nil, err
	} else if ok {
		initInstrs = instr
	} else {
		// Not an initializer, roll back
		p.index = checkpoint
	}

	// Parse condition
	cond, err := p.parseExpression(false)
	if err != nil {
		return nil, err
	}

	// Parse then branch
	lbrace, err := p.expectType(lexer.TypeLbrace)
	if err != nil {
		return nil, err // EOF
	}

	thenInstrs, err := p.parseBlock(lbrace)
	if err != nil {
		return nil, err
	}

	if _, err := p.expectType(lexer.TypeRbrace); err != nil {
		return nil, err // EOF
	}

	thenBody := ast.NewBody(thenInstrs, lbrace.Location)

	// Check for else or else if
	var elseBody *ast.Body

	nextElse, err := p.peekType(lexer.TypeKeyword)
	if err != nil {
		return nil, err // EOF
	}

	if nextElse.Type != lexer.TypeKeyword {
		// Don't rollback, since peek didn't consume the token.
	} else if nextElse.Keyword != lexer.KeywordElse {
		// We expected an 'else' keyword, but got something else.
		p.index--
	} else {
		afterElse, err := p.peekType(lexer.TypeKeyword, lexer.TypeLbrace)
		if err != nil {
			return nil, err // EOF
		}

		if afterElse.Type == lexer.TypeKeyword && afterElse.Keyword == lexer.KeywordIf {
			// else if: recursively parse another if
			elseInstr, err := p.parseIf(afterElse)
			if err != nil {
				return nil, err
			}

			elseBody = ast.NewBody([]ast.Instruction{elseInstr}, elseInstr.Location())
		} else if afterElse.Type == lexer.TypeLbrace {
			// else: parse block
			elseInstrs, err := p.parseBlock(lbrace)
			if err != nil {
				return nil, err
			}

			if _, err := p.expectType(lexer.TypeRbrace); err != nil {
				return nil, err // EOF
			}

			elseBody = ast.NewBody(elseInstrs, lbrace.Location)
		} else {
			afterElse.Location.Errorf("expected 'if' or '{' after 'else', got %s", afterElse.StringVal)

			// error recovery:
			elseBody = nil
		}
	}

	return ast.NewIf(first.Location, initInstrs, cond, thenBody, elseBody), nil
}

// parseFor parses a for loop of the form: for <cond> { ... }
func (p *Parser) parseFor(first lexer.Token) (ast.Instruction, error) {
	// 'for' keyword already consumed
	index := p.index

	var (
		initInstrs []ast.Instruction
		postInstrs []ast.Instruction
		cond       ast.Expression
	)

	// Try to parse an initializer (for now only assignment or set)
	instr, ok, err := p.parseAssignOrDeclare(true)
	if err != nil {
		return nil, err
	} else if ok {
		initInstrs = instr
	} else {
		// Not an initializer, roll back
		p.index = index
	}

	cond, err = p.parseExpression(false)
	if err != nil {
		return nil, err
	}

	semi, err := p.peekType(lexer.TypeSemicolon)
	if err != nil {
		return nil, err // EOF
	}

	if semi.Type == lexer.TypeSemicolon {
		// If we found a semicolon, we expect another assignment.
		instr, ok, err := p.parseAssignOrDeclare(false)
		if err != nil {
			return nil, err
		} else if ok {
			postInstrs = instr
		} else {
			// Not an assignment, roll back
			p.index--
		}
	}

	lbrace, err := p.expectType(lexer.TypeLbrace)
	if err != nil {
		return nil, err // EOF
	}

	bodyInstrs, err := p.parseBlock(lbrace)
	if err != nil {
		return nil, err
	}

	if _, err := p.expectType(lexer.TypeRbrace); err != nil {
		return nil, err // EOF
	}

	return ast.NewFor(first.Location, initInstrs, cond, postInstrs,
		ast.NewBody(bodyInstrs, lbrace.Location)), nil
}
