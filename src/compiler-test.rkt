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
    "lambda with unbound type variables"
    (check-exn
     exn:fail?
     (lambda ()
       (kashmir-compile-expr
	'(lambda (q) q)))))
      
   (test-case
    "lambda with no arguments -> func()"
    (check-equal?
     (kashmir-compile-expr
      '(lambda () 1))
     "(func () int {\nreturn 1\n})"))

   (test-case
    "apply no-argument function"
    (check-equal?
     (kashmir-compile-expr
      '((lambda () 1)))
     "(func () int {\nreturn 1\n})()"))

   (test-case
    "let no-argument function"
    (check-equal?
     (kashmir-compile-expr
      '(let ([x (lambda () 1)]) 2))
     "(func () int {\nvar x func () (int) = (func () int {\nreturn 1\n})\nreturn 2\n}())"))

      (test-case
    "let no-argument function"
    (check-equal?
     (kashmir-compile-expr
      '(let ([name-with-dashes 1]) name-with-dashes))
     "(func () int {\nvar nameWithDashes int = 1\nreturn nameWithDashes\n}())"))

   (test-case
    "partial application"
    (check-equal?
     (run '(((lambda (x y) x) 1) 2))
     1))

   (test-case
    "let"
    (check-equal?
     (run '(let ([x 1]) (+ x 2)))
     3))

   (test-case
   "let type annotated"
    (check-equal?
     (run '(let ([[x : int] 1]) (+ x 2)))
     3))
      
   (test-case
    "higher-order functions"
    (check-equal?
     (run '(((lambda (x y) (x y)) (lambda (x) x)) 1))
     1))

   (test-case
    "call by name"
    (check-equal?
     (run '(let ([make-num (lambda () 3)]) (* (make-num) (make-num))))
     9))))
  
(run-tests compiler-tests)

