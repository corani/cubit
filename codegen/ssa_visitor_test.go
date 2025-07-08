package codegen

import (
	"testing"

	"github.com/corani/refactored-giggle/ir"
	"github.com/stretchr/testify/require"
)

func TestSSAVisitor_CompilationUnit(t *testing.T) {
	unit := ir.NewCompilationUnit()
	visitor := NewSSAVisitor()
	_ = unit.Accept(visitor) // Add real assertions as needed
}

func TestAST_CompilationUnit(t *testing.T) {
	t.Parallel()

	unit := ir.NewCompilationUnit()

	unit.WithDataDefs(
		ir.NewDataDefStringZ(ir.Ident("data_hello0"), `Hello from test-%d!\n`))

	unit.WithFuncDefs(
		ir.NewFuncDef(ir.Ident("hello"),
			ir.NewParamRegular(ir.NewAbiTyBase(ir.BaseWord), ir.Ident("arg")),
		).
			WithBlocks(ir.Block{
				Label: "start",
				Instructions: []ir.Instruction{
					ir.NewCall(ir.NewValGlobal(ir.Ident("printf"), ir.NewAbiTyBase(ir.BaseLong)),
						ir.NewArgRegular(ir.NewValGlobal(ir.Ident("data_hello0"), ir.NewAbiTyBase(ir.BaseLong))),
						ir.NewArgRegular(ir.NewValIdent(ir.Ident("arg"), ir.NewAbiTyBase(ir.BaseWord)))),
					ir.NewRet(),
				},
			}),
		ir.NewFuncDef(ir.Ident("main")).
			WithLinkage(ir.NewLinkageExport()).
			WithRetTy(ir.NewAbiTyBase(ir.BaseWord)).
			WithBlocks(ir.Block{
				Label: "start",
				Instructions: []ir.Instruction{
					ir.NewCall(ir.NewValGlobal(ir.Ident("hello"), ir.NewAbiTyBase(ir.BaseWord)),
						ir.NewArgRegular(ir.NewValInteger(33, ir.NewAbiTyBase(ir.BaseWord)))),
					ir.NewRet(ir.NewValInteger(0, ir.NewAbiTyBase(ir.BaseWord))),
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
	actual := unit.Accept(visitor)

	require.Equal(t, expected, actual)
}

func TestAST_TypeDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    ir.TypeDef
		expected string
	}{
		{
			name: "four floats",
			input: ir.NewTypeDefRegular(ir.Ident("fourfloats"),
				ir.NewSubTyExtSize(ir.ExtSingle, 1),
				ir.NewSubTyExtSize(ir.ExtSingle, 1),
				ir.NewSubTyExtSize(ir.ExtDouble, 1),
				ir.NewSubTyExtSize(ir.ExtDouble, 1),
			),
			expected: "type :fourfloats = { s, s, d, d }",
		},
		{
			name: "onebytemanywords",
			input: ir.NewTypeDefRegular(ir.Ident("onebytemanywords"),
				ir.NewSubTyExtSize(ir.ExtByte, 1),
				ir.NewSubTyExtSize(ir.ExtWord, 100),
			),
			expected: "type :onebytemanywords = { b, w 100 }",
		},
		{
			name: "cryptovector",
			input: ir.NewTypeDefRegular(ir.Ident("cryptovector"),
				ir.NewSubTyExtSize(ir.ExtWord, 4),
			).
				WithAlign(16),
			expected: "type :cryptovector = align 16 { w 4 }",
		},
		{
			name: "union",
			input: ir.NewTypeDefUnion(ir.Ident("union"),
				[]ir.SubTySize{ir.NewSubTyExtSize(ir.ExtByte, 1)},
				[]ir.SubTySize{ir.NewSubTyExtSize(ir.ExtSingle, 1)},
			),
			expected: "type :union = { { b }, { s } }",
		},
		{
			name: "opaque",
			input: ir.NewTypeDefOpaque(ir.Ident("opaque"), 32).
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

	tt := []struct {
		name     string
		input    ir.DataDef
		expected string
	}{
		{
			name: "three words and a byte",
			input: ir.NewDataDef(ir.Ident("a"),
				ir.NewDataInitExt(ir.ExtWord,
					ir.NewDataItemConst(ir.NewConstInteger(1)),
					ir.NewDataItemConst(ir.NewConstInteger(2)),
					ir.NewDataItemConst(ir.NewConstInteger(3))),
				ir.NewDataInitExt(ir.ExtByte,
					ir.NewDataItemConst(ir.NewConstInteger(0))),
			),
			expected: "data $a = { w 1 2 3, b 0 }",
		},
		{
			name: "a thousand zero initialized bytes",
			input: ir.NewDataDef(ir.Ident("b"),
				ir.NewDataInitZero(1000),
			),
			expected: "data $b = { z 1000 }",
		},
		{
			name: "an object pointing to itself",
			input: ir.NewDataDef(ir.Ident("c"),
				ir.NewDataInitExt(ir.ExtLong,
					ir.NewDataItemConst(ir.NewConstInteger(-1))),
				ir.NewDataInitExt(ir.ExtLong,
					ir.NewDataItemSymbol(ir.Ident("c"), 0)),
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

	tt := []struct {
		name     string
		input    ir.FuncDef
		expected string
	}{
		{
			name: "add",
			input: ir.NewFuncDef(ir.Ident("addbyte"),
				ir.NewParamRegular(ir.NewAbiTyBase(ir.BaseWord), ir.Ident("a")),
				ir.NewParamRegular(ir.NewAbiTySubW(ir.SubWSB), ir.Ident("b")),
			).
				WithRetTy(ir.NewAbiTyBase(ir.BaseWord)),
			expected: "function w $addbyte(w %a, sb %b) {}",
		},
		{
			name: "addenv",
			input: ir.NewFuncDef(ir.Ident("add"),
				ir.NewParamEnv(ir.Ident("e")),
				ir.NewParamRegular(ir.NewAbiTyBase(ir.BaseWord), ir.Ident("a")),
				ir.NewParamRegular(ir.NewAbiTyBase(ir.BaseWord), ir.Ident("b")),
			).
				WithRetTy(ir.NewAbiTyBase(ir.BaseWord)).
				WithLinkage(ir.NewLinkageExport()),
			expected: "export function w $add(env %e, w %a, w %b) {}",
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
