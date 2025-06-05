package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"

	"modernc.org/libqbe"
)

func GenerateCode(ops []Op) (string, error) {
	var sb strings.Builder

	data := make(map[string]string)
	funName := ""
	dataIndex := 0

	for _, op := range ops {
		switch op.Type {
		case OpTypeFunc:
			funName = op.Args[0].Identifier

			if funName == "main" {
				fmt.Fprint(&sb, "export function w $main(")
			} else {
				fmt.Fprintf(&sb, "function w $%s(", funName)
			}

			for i, arg := range op.Args[1:] {
				if i > 0 {
					fmt.Fprint(&sb, ", ")
				}

				switch arg.Type {
				case TypeIdent:
					fmt.Fprintf(&sb, "w %%%s", arg.Identifier)
				default:
					return "", fmt.Errorf("unexpected argument type %s in function %s", arg.Type, funName)
				}
			}

			fmt.Fprint(&sb, ") {\n@start\n")
		case OpTypeCall:
			name := op.Args[0].Identifier

			fmt.Fprintf(&sb, "  call $%s(", name)

			for i, arg := range op.Args[1:] {
				if i > 0 {
					fmt.Fprint(&sb, ", ")
				}

				switch arg.Type {
				case TypeString:
					id := fmt.Sprintf("data_%s%d", funName, dataIndex)
					data[id] = arg.StringVal
					dataIndex++

					fmt.Fprintf(&sb, "l $%s", id)
				case TypeNumber:
					fmt.Fprintf(&sb, "w %d", arg.NumberVal)
				case TypeIdent:
					fmt.Fprintf(&sb, "w %%%s", arg.Identifier)
				default:
					return "", fmt.Errorf("unexpected argument type %s in call to %s", arg.Type, name)
				}
			}
			fmt.Fprintln(&sb, ")")
		case OpTypeReturn:
			fmt.Fprintln(&sb, "  ret 0\n}")
		}
	}

	for k, v := range data {
		fmt.Fprintf(&sb, "data $%s = { b \"%s\", b 0 }\n", k, v)
	}

	return sb.String(), nil
}

func GenerateAssembly(srcfile, code, asmfile string) error {
	var w bytes.Buffer

	if err := libqbe.Main(
		libqbe.DefaultTarget(runtime.GOOS, runtime.GOARCH),
		srcfile, strings.NewReader(code), &w, nil,
	); err != nil {
		return err
	}
	return os.WriteFile(asmfile, w.Bytes(), 0644)
}

func Compile(asm, bin string, run bool) error {
	if out, err := exec.Command("cc", "-o", bin, asm).CombinedOutput(); err != nil {
		return fmt.Errorf("cc failed: %s: %w", string(out), err)
	}

	if run {
		if out, err := exec.Command(bin).CombinedOutput(); err != nil {
			return fmt.Errorf("run failed: %s: %w", string(out), err)
		} else if len(out) > 0 {
			fmt.Printf("run output:\n%s\n", string(out))
		}
	}

	return nil
}
