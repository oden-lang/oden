#lang racket

(require rackunit "types.rkt")
(require rackunit/text-ui)

(define types-test
  (test-suite
   "Testing the type system"

   (check-equal?
    (get-type (e/app identity (e/var "true")))
    bool
    "identity")

   (check-equal?
    (get-type (e/app (e/app (e/var "pair") (e/var "1")) (e/var "2")))
    (pair number number)
    "product type")))

(run-tests types-test)
