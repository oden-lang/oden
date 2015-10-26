package main

import (
	"fmt"
)

func pingPong() string {
	a := make(chan int)
	b := make(chan int)
	res := make(chan string)
	f := func(name string, in, out chan int) {
		var n int = <-in
		for {
			if n > 0 {
				fmt.Println(name, n)
				out <- (n - 1)
				_new_n := <-in
				n = _new_n
				continue
			} else {
				res <- "Done"
			}
		}
	}
	go f("ping", a, b)
	go f("pong", b, a)
	a <- 10
	return <-res
}
