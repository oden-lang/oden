package main

import (
	"fmt"
)

func pingPonger(name string, in, out chan int, res chan string) {
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

func pingPong2() string {
	a := make(chan int)
	b := make(chan int)
	res := make(chan string)
	go pingPonger("ping", a, b, res)
	go pingPonger("pong", b, a, res)
	a <- 10
	return <-res
}
