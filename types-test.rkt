#lang racket

(require rackunit "types.rkt")
(require rackunit/text-ui)

(define identity (e/lam (e/var "x") (e/var "x")))

(define types-test
  (test-suite
   "Testing the type system"

   (check-equal?
    ;; (identity true) : bool
    (get-type (e/app identity (e/var "true")))
    predef/bool
    "identity")

   (check-equal?
    ;; (list/singleton 1) : (list number)
    (get-type (e/app (e/var "list/singleton") (e/var "1")))
    (predef/list predef/number)
    "list/singleton")))

(run-tests types-test)
