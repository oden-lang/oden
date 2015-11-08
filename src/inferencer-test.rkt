#lang racket

(require rackunit "inferencer.rkt")
(require rackunit/text-ui)

(define (only-type te) (car (cddr te)))

(define inferencer-test
  (test-suite
   "Type System"

   (test-case
    "identity"
    (check-equal?
     (:? '((lambda (x) x) true))
     '((((lambda ([x : bool]) (x : bool)) : (bool -> bool)) (true : bool)) : bool)))

   (test-case
    "lambda without arguments"
    (:? '((lambda () true)))
    '(((lambda () (true : bool)) : (-> bool)) : bool))

   (test-case
    "if"
    (check-equal?
     (:? '(if true 1 0))
     '((if (true : bool) (1 : int) (0 : int)) : int)))


   (test-case
    "if branches have same type"
    (check-exn
     exn:fail?
     (lambda ()
       (:? '(if true 1 true)))))

   (test-case
    "type-annotated complex expression"
    (check-equal?
     (only-type (:? '(((+ 1) 2) : int)))
     'int))))

(run-tests inferencer-test)
