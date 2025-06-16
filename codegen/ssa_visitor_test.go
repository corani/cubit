package codegen

import (
	"testing"

	"github.com/corani/refactored-giggle/ast"
	"github.com/stretchr/testify/require"
)

func TestSSAVisitor_CompilationUnit(t *testing.T) {
	unit := ast.NewCompilationUnit()
	visitor := NewSSAVisitor()
	_ = visitor.VisitCompilationUnit(&unit) // Add real assertions as needed
}

func TestAST_CompilationUnit(t *testing.T) {
	t.Parallel()

	unit := new(ast.CompilationUnit)

	unit.WithDataDefs(
		ast.NewDataDefStringZ(ast.Ident("data_hello0"), `Hello from test-%d!\n`))

	unit.WithFuncDefs(
		ast.NewFuncDef(ast.Ident("hello"),
			ast.NewParamRegular(ast.NewAbiTyBase(ast.BaseWord), ast.Ident("arg")),
		).
			WithBlocks(ast.Block{
				Label: "start",
				Instructions: []ast.Instruction{
					ast.NewCall(ast.NewValGlobal(ast.Ident("printf")),
						ast.NewArgRegular(ast.NewAbiTyBase(ast.BaseLong), ast.NewValGlobal(ast.Ident("data_hello0"))),
						ast.NewArgRegular(ast.NewAbiTyBase(ast.BaseWord), ast.NewValIdent(ast.Ident("arg")))),
					ast.NewRet(),
				},
			}),
		ast.NewFuncDef(ast.Ident("main")).
			WithLinkage(ast.NewLinkageExport()).
			WithRetTy(ast.NewAbiTyBase(ast.BaseWord)).
			WithBlocks(ast.Block{
				Label: "start",
				Instructions: []ast.Instruction{
					ast.NewCall(ast.NewValGlobal(ast.Ident("hello")),
						ast.NewArgRegular(ast.NewAbiTyBase(ast.BaseWord), ast.NewValInteger(33))),
					ast.NewRet(ast.NewValInteger(0)),
				},
			}),
	)

	expected := `function $hello(w %arg) {
@start
	call $printf(l $data_hello0, w %arg)
	ret
}
export function w $main() {
@start
	call $hello(w 33)
	ret 0
}
data $data_hello0 = { b "Hello from test-%d!\n", b 0 }
`

	visitor := NewSSAVisitor()
	actual := visitor.VisitCompilationUnit(unit)

	require.Equal(t, expected, actual)
}

func TestAST_TypeDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    ast.TypeDef
		expected string
	}{
		{
			name: "four floats",
			input: ast.NewTypeDefRegular(ast.Ident("fourfloats"),
				ast.NewSubTyExtSize(ast.ExtSingle, 1),
				ast.NewSubTyExtSize(ast.ExtSingle, 1),
				ast.NewSubTyExtSize(ast.ExtDouble, 1),
				ast.NewSubTyExtSize(ast.ExtDouble, 1),
			),
			expected: "type :fourfloats = { s, s, d, d }",
		},
		{
			name: "onebytemanywords",
			input: ast.NewTypeDefRegular(ast.Ident("onebytemanywords"),
				ast.NewSubTyExtSize(ast.ExtByte, 1),
				ast.NewSubTyExtSize(ast.ExtWord, 100),
			),
			expected: "type :onebytemanywords = { b, w 100 }",
		},
		{
			name: "cryptovector",
			input: ast.NewTypeDefRegular(ast.Ident("cryptovector"),
				ast.NewSubTyExtSize(ast.ExtWord, 4),
			).
				WithAlign(16),
			expected: "type :cryptovector = align 16 { w 4 }",
		},
		{
			name: "union",
			input: ast.NewTypeDefUnion(ast.Ident("union"),
				[]ast.SubTySize{ast.NewSubTyExtSize(ast.ExtByte, 1)},
				[]ast.SubTySize{ast.NewSubTyExtSize(ast.ExtSingle, 1)},
			),
			expected: "type :union = { { b }, { s } }",
		},
		{
			name: "opaque",
			input: ast.NewTypeDefOpaque(ast.Ident("opaque"), 32).
				WithAlign(16),
			expected: "type :opaque = align 16 { 32 }",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			visitor := NewSSAVisitor()
			actual := visitor.VisitTypeDef(tc.input)

			require.Equal(t, tc.expected, actual)
		})
	}
}

func TestAST_DataDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    ast.DataDef
		expected string
	}{
		{
			name: "three words and a byte",
			input: ast.NewDataDef(ast.Ident("a"),
				ast.NewDataInitExt(ast.ExtWord,
					ast.NewDataItemConst(ast.NewConstInteger(1)),
					ast.NewDataItemConst(ast.NewConstInteger(2)),
					ast.NewDataItemConst(ast.NewConstInteger(3))),
				ast.NewDataInitExt(ast.ExtByte,
					ast.NewDataItemConst(ast.NewConstInteger(0))),
			),
			expected: "data $a = { w 1 2 3, b 0 }",
		},
		{
			name: "a thousand zero initialized bytes",
			input: ast.NewDataDef(ast.Ident("b"),
				ast.NewDataInitZero(1000),
			),
			expected: "data $b = { z 1000 }",
		},
		{
			name: "an object pointing to itself",
			input: ast.NewDataDef(ast.Ident("c"),
				ast.NewDataInitExt(ast.ExtLong,
					ast.NewDataItemConst(ast.NewConstInteger(-1))),
				ast.NewDataInitExt(ast.ExtLong,
					ast.NewDataItemSymbol(ast.Ident("c"), 0)),
			),
			expected: "data $c = { l -1, l $c }",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			visitor := NewSSAVisitor()
			actual := visitor.VisitDataDef(tc.input)

			require.Equal(t, tc.expected, actual)
		})
	}
}

func TestAST_FuncDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    ast.FuncDef
		expected string
	}{
		{
			name: "add",
			input: ast.NewFuncDef(ast.Ident("addbyte"),
				ast.NewParamRegular(ast.NewAbiTyBase(ast.BaseWord), ast.Ident("a")),
				ast.NewParamRegular(ast.NewAbiTySubW(ast.SubWSB), ast.Ident("b")),
			).
				WithRetTy(ast.NewAbiTyBase(ast.BaseWord)),
			expected: "function w $addbyte(w %a, sb %b) {}",
		},
		{
			name: "addenv",
			input: ast.NewFuncDef(ast.Ident("add"),
				ast.NewParamEnv(ast.Ident("e")),
				ast.NewParamRegular(ast.NewAbiTyBase(ast.BaseWord), ast.Ident("a")),
				ast.NewParamRegular(ast.NewAbiTyBase(ast.BaseWord), ast.Ident("b")),
			).
				WithRetTy(ast.NewAbiTyBase(ast.BaseWord)).
				WithLinkage(ast.NewLinkageExport()),
			expected: "export function w $add(env %e, w %a, w %b) {}",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			visitor := NewSSAVisitor()
			actual := visitor.VisitFuncDef(tc.input)

			require.Equal(t, tc.expected, actual)
		})
	}
}
