package codegen

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"

	"github.com/corani/refactored-giggle/ast"
	"modernc.org/libqbe"
)

// WriteSSA writes the SSA code for the given CompilationUnit to the specified filename.
func WriteSSA(unit *ast.CompilationUnit, filename string) error {
	return os.WriteFile(filename, []byte(unit.String()), 0644)
}

// GenerateAssembly generates assembly from the given CompilationUnit.
func GenerateAssembly(srcfile string, unit *ast.CompilationUnit, asmfile string) error {
	var w bytes.Buffer

	if err := libqbe.Main(
		libqbe.DefaultTarget(runtime.GOOS, runtime.GOARCH),
		srcfile, strings.NewReader(unit.String()), &w, nil,
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
