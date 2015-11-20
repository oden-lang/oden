(pkg fibonacci/main)

(import fmt)
(import strconv)

(define (fib [n : int])
  (if (== n 1)
    0
    (if (== n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(define (main)
  (fmt.Println (strconv.Itoa (fib 10))))
