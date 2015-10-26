package main

import (
	"fmt"
	"time"
)

func timefunc(name string, f func()) {
	iterations := 1000000
	start := time.Now()
	for i := 0; i < iterations; i++ {
		f()
	}
	elapsed := time.Since(start)
	fmt.Printf("%s:\n%d iterations took %s\n\n", name, iterations, elapsed)
}

func main() {
	var result int = 0
	timefunc("plus and minus", func() {
		result = plus(minus(result, 1), 1)
	})

	fmt.Println("factorial(5): ", factorial(5))
	fmt.Println("factorial(10): ", factorial(10))

	fmt.Println("Ping Pong: ", pingPong())
	fmt.Println("Ping Pong 2: ", pingPong2())

	timefunc("TreeDynamic", func() {
		exampleTreeDynamic.Size()
	})
	timefunc("TreeStatic", func() {
		SizeTreeStatic(exampleTreeStatic)
	})
}
