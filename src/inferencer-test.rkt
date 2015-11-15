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
     (infer '((fn (x) x) true))
     '((((fn ([x : bool]) (x : bool)) : (bool -> bool)) (true : bool)) : bool)))

   (test-case
    "fn without arguments"
    (infer '((fn () true)))
    '(((fn () (true : bool)) : (-> bool)) : bool))

   (test-case
    "if"
    (check-equal?
     (infer '(if true 1 0))
     '((if (true : bool) (1 : int) (0 : int)) : int)))


   (test-case
    "if branches have same type"
    (check-exn
     exn:fail?
     (thunk
       (infer '(if true 1 true)))))

   (test-case
    "complex expressions in if"
    (check-equal?
     (only-type (infer '(if ((== ((+ 10) 10)) 20) ((+ 1) 1) ((+ 2) 2))))
     'int))

   (test-case
    "type-annotated complex expression"
    (check-equal?
     (only-type (infer '(((+ 1) 2) : int)))
     'int))

   (test-case
     "recursive function definition"
     (check-equal?
      (only-type (infer-def '(define inf (fn  ([x : int]) ((+ 1) (inf x))))))
      '(int -> int)))

   (test-case
     "recursive function definition with no type type constraints (will fail in codegen stage) "
     (check-equal?
      (only-type (infer-def '(define inf (fn (x) (inf x)))))
      '(_.0 -> _.1)))))

(run-tests inferencer-test)
