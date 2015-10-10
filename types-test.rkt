#lang racket

(require rackunit "types.rkt")
(require rackunit/text-ui)

(define identity (e/lam (e/var "x") (e/var "x")))

(define types-test
  (test-suite
   "Type System"

   (test-case
    "identity"
    (check-equal?
     ;; (identity true) : bool
     (get-type (e/app identity (e/var "true")))
     predef/bool))

   (test-case
    "list/singleton"
    (check-equal?
     ;; (list/singleton 1) : (list number)
     (get-type (e/app (e/var "list/singleton") (e/var "1")))
     (predef/list predef/number)))

   (test-case
    "if"
    (check-equal?
     ;; (if true 1 0) : bool
     (get-type (e/app
                (e/app
                 (e/app (e/var "if") (e/var "true"))
                 (e/var "1"))
                (e/var "0")))
     predef/number))

   (test-case
    "if branches have same type"
    (check-exn
     exn:fail?
     ;; (if true 1 true)
     (lambda ()
       (get-type (e/app
                  (e/app
                   (e/app (e/var "if") (e/var "true"))
                   (e/var "1"))
                  (e/var "true"))))))))

(run-tests types-test)
