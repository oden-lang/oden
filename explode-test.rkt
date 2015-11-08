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
    (explode '(f))
    '(f))

   (test-case
    "lambda with single argument"
    (explode '(lambda (x) x))
    '(lambda (x) x))

   (test-case
    "lambda with multiple arguments"
    (explode '(lambda (x y z) x))
    '(lambda (x) (lambda (y) (lambda (z) x))))

   (test-case
    "lambda with single type-annotated argument"
    (explode '(lambda ([x : int]) x))
    '(lambda ([x : int]) x))

   (test-case
    "lambda with multiple type-annotated arguments"
    (explode '(lambda (x [y : int] [z : bool]) x))
    '(lambda (x) (lambda ([y : int]) (lambda ([z : bool]) x))))

   ))

(run-tests explode-test)
