package start

import (
	"bufio"
	"fmt"
	"fungsi/internal/help"
	"fungsi/internal/lexer"
	"os"
)

// TODO: subpackage `repl` with `repl` object
// TODO: with IN and OUT and other
func repl() {
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print(help.IN)
		data, err := reader.ReadString('\n')
		if err != nil {
			fmt.Println(help.OUT, err.Error())
		}
		eval(data)
	}
}

func eval(code string) {
	tokens := lexer.Scan(code)
	fmt.Println(tokens)
}

func evalFile(path string) {

}

func Start(args []string) {
	if len(args) == 0 {
		repl()
	} else if len(args) == 1 {
		evalFile(args[0])
	} else {
		fmt.Println(help.HELP)
	}
}
