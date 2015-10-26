package main

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
