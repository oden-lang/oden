package main

import (
	"fmt"
)

func plus(x, y int) int {
	return x + y
}

//var minus func(int, int) int = func(x int, y int) int {
//return x - y
//}
func minus(x, y int) int {
	return x - y
}

func factorial(n int64) int64 {
	{
		n := n
		var acc int64 = 1
		for {
			if n == 0 {
				return acc
			} else {
				_new_n := n - 1
				_new_acc := acc * n
				n = _new_n
				acc = _new_acc
				continue
			}
		}
	}
}

func main() {
	var result int = 0
	for i := 0; i < 1000000; i++ {
		result = plus(minus(result, i), i)
	}
	fmt.Println(result)

	fmt.Println("factorial(5): ", factorial(5))
	fmt.Println("factorial(10): ", factorial(10))
}
