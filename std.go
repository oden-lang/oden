package main

import (
	"fmt"
)

func main() {
	var result int = 0
	for i := 0; i < 1000000; i++ {
		result += i - i
	}
	fmt.Println(result)
}
