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
    "function without arguments"
    (explode '(f))
    '(f))))

(run-tests explode-test)
