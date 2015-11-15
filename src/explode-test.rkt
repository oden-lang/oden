#lang racket

(require rackunit "explode.rkt")
(require rackunit/text-ui)

(define explode-test
  (test-suite
   "explode"

   (test-case
    "literal"
    (check-equal?
     (explode 'foo)
     'foo))

   (test-case
    "type-annotated literal"
    (check-equal?
     (explode '(123 : int))
     '(123 : int)))

   (test-case
    "type-annotated complex expression"
    (check-equal?
     (explode '((+ 1 2) : int))
     '(((+ 1) 2) : int)))

   (test-case
    "function application"
    (check-equal?
     (explode '(f a b c d e))
     '(((((f a) b) c) d) e)))

   (test-case
    "function application without arguments"
    (check-equal?
     (explode '(f))
     '(f)))

   (test-case
    "fn with single argument"
    (check-equal?
     (explode '(fn (x) x))
     '(fn (x) x)))

   (test-case
    "fn with multiple arguments"
    (check-equal?
     (explode '(fn (x y z) x))
     '(fn (x) (fn (y) (fn (z) x)))))

   (test-case
    "fn with single type-annotated argument"
    (check-equal?
     (explode '(fn ([x : int]) x))
     '(fn ([x : int]) x)))

   (test-case
    "fn with multiple type-annotated arguments"
    (check-equal?
     (explode '(fn (x [y : int] [z : bool]) x))
     '(fn (x) (fn ([y : int]) (fn ([z : bool]) x)))))

   (test-case
    "if is not exploded like a function application"
    (check-equal?
     (explode '(if c a b))
     '(if c a b)))

   (test-case
    "simple define is not transformed"
    (check-equal?
     (explode-definition '(define foo bar))
     '(define foo bar)))

   (test-case
    "define function shorthand explodes to define fn"
    (check-equal?
     (explode-definition '(define (foo bar baz) (bar baz)))
     '(define foo
	(fn (bar)
	  (fn (baz)
	    (bar baz))))))))

(run-tests explode-test)
