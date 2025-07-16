package lexer

import "fmt"

type Location struct {
	Filename     string
	Line, Column int
}

func (l Location) String() string {
	return fmt.Sprintf("%s:%d:%d", l.Filename, l.Line, l.Column)
}

func (l Location) Errorf(format string, args ...any) error {
	fmt.Printf("%s: [ERRO] "+format+"\n", append([]any{l}, args...)...)

	return fmt.Errorf("%s: "+format, append([]any{l}, args...)...)
}

func (l Location) Warnf(format string, args ...any) {
	fmt.Printf("%s: [WARN] "+format+"\n", append([]any{l}, args...)...)
}

func (l Location) Infof(format string, args ...any) {
	fmt.Printf("%s: [INFO] "+format+"\n", append([]any{l}, args...)...)
}
