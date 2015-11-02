#lang racket

(require rackunit "compiler.rkt")
(require rackunit/text-ui)

(define (run prg)
  (with-input-from-string (go-run (kashmir-compile-prg prg))
    (lambda () (read))))

(define compiler-tests
  (test-suite
   "compiler"

   (test-case
    "int literal"
    (check-equal?
     (run 123)
     123))

   (test-case
    "bool literal"
    (check-equal?
     (run 'true)
     'true))

   (test-case
    "lambda -> func"
    (check-equal?
     (kashmir-compile-expr
      '(lambda ([q : int]) q))
     "(func (q int) int {\nreturn q\n})"))

   (test-case
    "partial application"
    (check-equal?
     (run '(((lambda (x) (lambda (y) x)) 1) 2))
     1))

   (test-case
    "let"
    (check-equal?
     (run '(let (x 1) ((+ x) 2)))
     3))

   ))

(run-tests compiler-tests)
