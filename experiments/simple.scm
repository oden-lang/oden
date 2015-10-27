(define (plus (x int) (y int) int)
 (+ x y))

(define minus (-> int int int)
 (lambda ((x int) (y int)) int
   (- x y)))
