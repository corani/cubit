package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestAST_CompilationUnit(t *testing.T) {
	t.Parallel()

	unit := new(CompilationUnit)

	unit.WithDataDefs(
		NewDataDefStringZ(Ident("data_hello0"), "Hello from test-%d!\n"))

	unit.WithFuncDefs(
		NewFuncDef(Ident("hello"),
			NewParamRegular(NewAbiTyBase(BaseWord), Ident("arg")),
		).
			WithBlocks(Block{
				Label: "start",
				Instructions: []Instruction{
					NewCall(NewValGlobal(Ident("printf")),
						NewArgRegular(NewAbiTyBase(BaseLong), NewValGlobal(Ident("data_hello0"))),
						NewArgRegular(NewAbiTyBase(BaseWord), NewValIdent(Ident("arg")))),
					NewRet(),
				},
			}),
		NewFuncDef(Ident("main")).
			WithLinkage(NewLinkageExport()).
			WithRetTy(NewAbiTyBase(BaseWord)).
			WithBlocks(Block{
				Label: "start",
				Instructions: []Instruction{
					NewCall(NewValGlobal(Ident("hello")),
						NewArgRegular(NewAbiTyBase(BaseWord), NewValInteger(33))),
					NewRet(NewValInteger(0)),
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

	require.Equal(t, expected, unit.String())
}

func TestAST_TypeDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    TypeDef
		expected string
	}{
		{
			name: "four floats",
			input: NewTypeDefRegular(Ident("fourfloats"),
				NewSubTyExtSize(ExtSingle, 1),
				NewSubTyExtSize(ExtSingle, 1),
				NewSubTyExtSize(ExtDouble, 1),
				NewSubTyExtSize(ExtDouble, 1),
			),
			expected: "type :fourfloats = { s, s, d, d }",
		},
		{
			name: "onebytemanywords",
			input: NewTypeDefRegular(Ident("onebytemanywords"),
				NewSubTyExtSize(ExtByte, 1),
				NewSubTyExtSize(ExtWord, 100),
			),
			expected: "type :onebytemanywords = { b, w 100 }",
		},
		{
			name: "cryptovector",
			input: NewTypeDefRegular(Ident("cryptovector"),
				NewSubTyExtSize(ExtWord, 4),
			).
				WithAlign(16),
			expected: "type :cryptovector = align 16 { w 4 }",
		},
		{
			name: "union",
			input: NewTypeDefUnion(Ident("union"),
				[]SubTySize{NewSubTyExtSize(ExtByte, 1)},
				[]SubTySize{NewSubTyExtSize(ExtSingle, 1)},
			),
			expected: "type :union = { { b }, { s } }",
		},
		{
			name: "opaque",
			input: NewTypeDefOpaque(Ident("opaque"), 32).
				WithAlign(16),
			expected: "type :opaque = align 16 { 32 }",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			require.Equal(t, tc.expected, tc.input.String())
		})
	}
}

func TestAST_DataDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    DataDef
		expected string
	}{
		{
			name: "three words and a byte",
			input: NewDataDef(Ident("a"),
				NewDataInitExt(ExtWord,
					NewDataItemConst(NewConstInteger(1)),
					NewDataItemConst(NewConstInteger(2)),
					NewDataItemConst(NewConstInteger(3))),
				NewDataInitExt(ExtByte,
					NewDataItemConst(NewConstInteger(0))),
			),
			expected: "data $a = { w 1 2 3, b 0 }",
		},
		{
			name: "a thousand zero initialized bytes",
			input: NewDataDef(Ident("b"),
				NewDataInitZero(1000),
			),
			expected: "data $b = { z 1000 }",
		},
		{
			name: "an object pointing to itself",
			input: NewDataDef(Ident("c"),
				NewDataInitExt(ExtLong,
					NewDataItemConst(NewConstInteger(-1))),
				NewDataInitExt(ExtLong,
					NewDataItemSymbol(Ident("c"), 0)),
			),
			expected: "data $c = { l -1, l $c }",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			require.Equal(t, tc.expected, tc.input.String())
		})
	}
}

func TestAST_FuncDef(t *testing.T) {
	t.Parallel()

	tt := []struct {
		name     string
		input    FuncDef
		expected string
	}{
		{
			name: "add",
			input: NewFuncDef(Ident("addbyte"),
				NewParamRegular(NewAbiTyBase(BaseWord), Ident("a")),
				NewParamRegular(NewAbiTySubW(SubWSB), Ident("b")),
			).
				WithRetTy(NewAbiTyBase(BaseWord)),
			expected: "function w $addbyte(w %a, sb %b) {}",
		},
		{
			name: "addenv",
			input: NewFuncDef(Ident("add"),
				NewParamEnv(Ident("e")),
				NewParamRegular(NewAbiTyBase(BaseWord), Ident("a")),
				NewParamRegular(NewAbiTyBase(BaseWord), Ident("b")),
			).
				WithRetTy(NewAbiTyBase(BaseWord)).
				WithLinkage(NewLinkageExport()),
			expected: "export function w $add(env %e, w %a, w %b) {}",
		},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			require.Equal(t, tc.expected, tc.input.String())
		})
	}
}
