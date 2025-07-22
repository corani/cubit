package codegen

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"os/exec"
	"runtime"
	"strings"

	"github.com/corani/cubit/internal/ir"
	"modernc.org/libqbe"
)

// WriteSSA writes the SSA code for the given CompilationUnit to the specified filename.
func WriteSSA(unit *ir.CompilationUnit, filename string) error {
	visitor := NewSSAVisitor()
	ssa := unit.Accept(visitor)

	return os.WriteFile(filename, []byte(ssa), 0644)
}

// GenerateAssembly generates assembly from the given CompilationUnit.
func GenerateAssembly(srcfile string, unit *ir.CompilationUnit, asmfile string) error {
	visitor := NewSSAVisitor()
	ssa := visitor.VisitCompilationUnit(unit)

	var w bytes.Buffer

	// TODO(daniel): make these flags, so you can generate assembly for different targets?
	// That may not work with `Compile` below though.
	goos := runtime.GOOS
	if goos == "android" {
		goos = "linux" // For Termux support on Android
	}

	if err := libqbe.Main(
		libqbe.DefaultTarget(goos, runtime.GOARCH),
		srcfile, strings.NewReader(ssa), &w, nil,
	); err != nil {
		return err
	}

	return os.WriteFile(asmfile, w.Bytes(), 0644)
}

func Compile(asm, bin string, flags ...string) error {
	args := append([]string{"-o", bin, asm}, flags...)

	log.Printf("[CMD] cc %s", strings.Join(args, " "))

	if out, err := exec.Command("cc", args...).CombinedOutput(); err != nil {
		return fmt.Errorf("cc failed: %s: %w", string(out), err)
	}

	return nil
}
