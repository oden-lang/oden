#lang racket

(require "explode.rkt")
(require "inferencer.rkt")
(require "compiled-pkg.rkt")

(require rackunit "go-backend.rkt")
(require rackunit/text-ui)

(define (codegen-single-expression expr)
  (codegen-expr (infer (explode expr))))

(define go-backend-tests
  (test-suite
   "go-backend"

   (test-case
    "empty pkg"
    (check-equal?
     (codegen-pkg (compiled-pkg 'foo '() '() '()))
     "package foo\n\n// imports\n\n// definitions\n"))

   (test-case
    "int literal"
    (check-equal?
     (codegen-single-expression 123)
     "123"))

   (test-case
    "bool literal"
    (check-equal?
     (codegen-single-expression 'true)
     "true"))

   (test-case
    "lambda -> func"
    (check-equal?
     (codegen-single-expression
      '(lambda ([q : int]) q))
     "(func (q int) int {\nreturn q\n})"))

   (test-case
    "lambda with unbound type variables"
    (check-exn
     exn:fail?
     (lambda ()
       (codegen-single-expression
	'(lambda (q) q)))))
      
   (test-case
    "lambda with no arguments -> func()"
    (check-equal?
     (codegen-single-expression
      '(lambda () 1))
     "(func () int {\nreturn 1\n})"))

   (test-case
    "apply no-argument function"
    (check-equal?
     (codegen-single-expression
      '((lambda () 1)))
     "(func () int {\nreturn 1\n})()"))

   (test-case
    "let no-argument function"
    (check-equal?
     (codegen-single-expression
      '(let ([x (lambda () 1)]) 2))
     "(func () int {\nvar x func () (int) = (func () int {\nreturn 1\n})\nreturn 2\n}())"))

      (test-case
    "let no-argument function"
    (check-equal?
     (codegen-single-expression '(let ([name-with-dashes 1]) name-with-dashes))
     "(func () int {\nvar nameWithDashes int = 1\nreturn nameWithDashes\n}())"))

   (test-case
    "partial application"
    (check-equal?
     (codegen-single-expression '(((lambda (x y) x) 1) 2))
     "(func (x int) func (int) (int) {\nreturn (func (y int) int {\nreturn x\n})\n})(1)(2)"))

   (test-case
    "let"
    (check-equal?
     (codegen-single-expression '(let ([x 1]) (+ x 2)))
     "(func () int {\nvar x int = 1\nreturn (x + 2)\n}())"))

   (test-case
   "let type annotated"
    (check-equal?
     (codegen-single-expression '(let ([[x : int] 1]) (+ x 2)))
     "(func () int {\nvar x int = 1\nreturn (x + 2)\n}())"))
      
   (test-case
    "higher-order functions"
    (check-equal?
     (codegen-single-expression '(((lambda (x y) (x y)) (lambda (x) x)) 1))
     "(func (x func (int) (int)) func (int) (int) {\nreturn (func (y int) int {\nreturn x(y)\n})\n})((func (x int) int {\nreturn x\n}))(1)"))

   (test-case
    "call by name"
    (check-equal?
     (codegen-single-expression '(let ([make-num (lambda () 3)]) (* (make-num) (make-num))))
     "(func () int {\nvar makeNum func () (int) = (func () int {\nreturn 3\n})\nreturn (makeNum() * makeNum())\n}())"))))
  
(run-tests go-backend-tests)
