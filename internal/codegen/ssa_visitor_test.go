package codegen

import (
	"testing"

	"github.com/corani/cubit/internal/ir"
	"github.com/corani/cubit/internal/lexer"
	"github.com/stretchr/testify/require"
)

func TestSSAVisitor_CompilationUnit(t *testing.T) {
	unit := ir.NewCompilationUnit()
	visitor := NewSSAVisitor()
	_ = unit.Accept(visitor) // Add real assertions as needed
}

func TestAST_CompilationUnit(t *testing.T) {
	t.Parallel()

	loc := lexer.Location{
		Line:     1,
		Column:   1,
		Filename: "test.in",
	}

	unit := ir.NewCompilationUnit()
	unit.WithPackage("test", loc)

	unit.WithDataDefs(
		ir.NewDataDefStringZ(loc, ir.Ident("data_hello0"), `Hello from test-%d!\n`))

	unit.WithFuncDefs(
		ir.NewFuncDef(loc, ir.Ident("hello"),
			ir.NewParamRegular(loc, ir.NewAbiTyBase(ir.BaseWord), ir.Ident("arg")),
		).
			WithBlocks(ir.Block{
				Label: "start",
				Instructions: []ir.Instruction{
					ir.NewCall(loc, ir.NewValGlobal(loc, ir.Ident("printf"), ir.NewAbiTyBase(ir.BaseLong)),
						ir.NewArgRegular(loc, ir.NewValGlobal(loc, ir.Ident("data_hello0"), ir.NewAbiTyBase(ir.BaseLong))),
						ir.NewArgRegular(loc, ir.NewValIdent(loc, ir.Ident("arg"), ir.NewAbiTyBase(ir.BaseWord)))),
					ir.NewRet(loc),
				},
			}),
		ir.NewFuncDef(loc, ir.Ident("main")).
			WithLinkage(ir.NewLinkageExport(loc)).
			WithRetTy(ir.NewAbiTyBase(ir.BaseWord)).
			WithBlocks(ir.Block{
				Label: "start",
				Instructions: []ir.Instruction{
					ir.NewCall(loc, ir.NewValGlobal(loc, ir.Ident("hello"), ir.NewAbiTyBase(ir.BaseWord)),
						ir.NewArgRegular(loc, ir.NewValInteger(loc, 33, ir.NewAbiTyBase(ir.BaseWord)))),
					ir.NewRet(loc, ir.NewValInteger(loc, 0, ir.NewAbiTyBase(ir.BaseWord))),
				},
			}),
	)

	expected := `# package test (test.in:1:1)

# test.in:1:1
function $hello(w %arg) {
@start
	call $printf(l $data_hello0, w %arg)
	ret
}

# test.in:1:1
export function w $main() {
@start
	call $hello(w 33)
	ret 0
}
data $data_hello0 = { b "Hello from test-%d!\n", b 0 }
`

	visitor := NewSSAVisitor()
	actual := unit.Accept(visitor)

	require.Equal(t, expected, actual)
}

func TestAST_TypeDef(t *testing.T) {
	t.Parallel()

	loc := lexer.Location{
		Line:     1,
		Column:   1,
		Filename: "test.in",
	}

	tt := []struct {
		name     string
		input    ir.TypeDef
		expected string
	}{
		{
			name: "four floats",
			input: ir.NewTypeDefRegular(loc, ir.Ident("fourfloats"),
				ir.NewSubTyExtSize(ir.ExtSingle, 1),
				ir.NewSubTyExtSize(ir.ExtSingle, 1),
				ir.NewSubTyExtSize(ir.ExtDouble, 1),
				ir.NewSubTyExtSize(ir.ExtDouble, 1),
			),
			expected: "type :fourfloats = { s, s, d, d }",
		},
		{
			name: "onebytemanywords",
			input: ir.NewTypeDefRegular(loc, ir.Ident("onebytemanywords"),
				ir.NewSubTyExtSize(ir.ExtByte, 1),
				ir.NewSubTyExtSize(ir.ExtWord, 100),
			),
			expected: "type :onebytemanywords = { b, w 100 }",
		},
		{
			name: "cryptovector",
			input: ir.NewTypeDefRegular(loc, ir.Ident("cryptovector"),
				ir.NewSubTyExtSize(ir.ExtWord, 4),
			).
				WithAlign(16),
			expected: "type :cryptovector = align 16 { w 4 }",
		},
		{
			name: "union",
			input: ir.NewTypeDefUnion(loc, ir.Ident("union"),
				[]ir.SubTySize{ir.NewSubTyExtSize(ir.ExtByte, 1)},
				[]ir.SubTySize{ir.NewSubTyExtSize(ir.ExtSingle, 1)},
			),
			expected: "type :union = { { b }, { s } }",
		},
		{
			name: "opaque",
			input: ir.NewTypeDefOpaque(loc, ir.Ident("opaque"), 32).
				WithAlign(16),
			expected: "type :opaque = align 16 { 32 }",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			visitor := NewSSAVisitor()
			actual := tc.input.Accept(visitor)

			require.Equal(t, tc.expected, actual)
		})
	}
}

func TestAST_DataDef(t *testing.T) {
	t.Parallel()

	loc := lexer.Location{
		Line:     1,
		Column:   1,
		Filename: "test.in",
	}

	tt := []struct {
		name     string
		input    ir.DataDef
		expected string
	}{
		{
			name: "three words and a byte",
			input: ir.NewDataDef(loc, ir.Ident("a"),
				ir.NewDataInitExt(loc, ir.ExtWord,
					ir.NewDataItemConst(loc, ir.NewConstInteger(loc, 1)),
					ir.NewDataItemConst(loc, ir.NewConstInteger(loc, 2)),
					ir.NewDataItemConst(loc, ir.NewConstInteger(loc, 3))),
				ir.NewDataInitExt(loc, ir.ExtByte,
					ir.NewDataItemConst(loc, ir.NewConstInteger(loc, 0))),
			),
			expected: "data $a = { w 1 2 3, b 0 }",
		},
		{
			name: "a thousand zero initialized bytes",
			input: ir.NewDataDef(loc, ir.Ident("b"),
				ir.NewDataInitZero(loc, 1000),
			),
			expected: "data $b = { z 1000 }",
		},
		{
			name: "an object pointing to itself",
			input: ir.NewDataDef(loc, ir.Ident("c"),
				ir.NewDataInitExt(loc, ir.ExtLong,
					ir.NewDataItemConst(loc, ir.NewConstInteger(loc, -1))),
				ir.NewDataInitExt(loc, ir.ExtLong,
					ir.NewDataItemSymbol(loc, ir.Ident("c"), 0)),
			),
			expected: "data $c = { l -1, l $c }",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			visitor := NewSSAVisitor()
			actual := tc.input.Accept(visitor)

			require.Equal(t, tc.expected, actual)
		})
	}
}

func TestAST_FuncDef(t *testing.T) {
	t.Parallel()

	loc := lexer.Location{
		Line:     1,
		Column:   1,
		Filename: "test.in",
	}

	tt := []struct {
		name     string
		input    ir.FuncDef
		expected string
	}{
		{
			name: "add",
			input: ir.NewFuncDef(loc, ir.Ident("addbyte"),
				ir.NewParamRegular(loc, ir.NewAbiTyBase(ir.BaseWord), ir.Ident("a")),
				ir.NewParamRegular(loc, ir.NewAbiTySubW(ir.SubWSB), ir.Ident("b")),
			).
				WithRetTy(ir.NewAbiTyBase(ir.BaseWord)),
			expected: "\n# test.in:1:1\nfunction w $addbyte(w %a, sb %b) {}",
		},
		{
			name: "addenv",
			input: ir.NewFuncDef(loc, ir.Ident("add"),
				ir.NewParamEnv(loc, ir.Ident("e")),
				ir.NewParamRegular(loc, ir.NewAbiTyBase(ir.BaseWord), ir.Ident("a")),
				ir.NewParamRegular(loc, ir.NewAbiTyBase(ir.BaseWord), ir.Ident("b")),
			).
				WithRetTy(ir.NewAbiTyBase(ir.BaseWord)).
				WithLinkage(ir.NewLinkageExport(loc)),
			expected: "\n# test.in:1:1\nexport function w $add(env %e, w %a, w %b) {}",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			visitor := NewSSAVisitor()
			actual := tc.input.Accept(visitor)

			require.Equal(t, tc.expected, actual)
		})
	}
}
