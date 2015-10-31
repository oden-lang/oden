#lang racket

(require rackunit "inferencer.rkt")
(require rackunit/text-ui)

(define types-test
  (test-suite
   "Type System"

   (test-case
    "identity"
    (check-equal?
     ;; (identity true) : bool
     (:? '((lambda (x) x) true))
     'bool
     #;
     (typed-expr
      predef/bool
      (e/app
       (typed-expr
        (t/fn predef/bool predef/bool)
        (e/lam
         (typed-expr predef/bool (e/var "x"))
         (typed-expr predef/bool (e/var "x"))))
       (typed-expr predef/bool (e/var "true"))))))


   (test-case
    "cons"
    (check-equal?
     (:? '(cons 1 ()))
     '(list int)
     #;
     (typed-expr
      (predef/list predef/number)
      (e/app
       (typed-expr
        (t/fn predef/number (predef/list predef/number))
        (e/var "list/singleton"))
       (typed-expr
        predef/number
        (e/var "1"))))))


   (test-case
    "if"
    (check-equal?
     (:? '(if true 1 0))
     'int
     #;
     (typed-expr
      predef/number
      (e/app
       (typed-expr
        (t/fn predef/number predef/number)
        (e/app
         (typed-expr
          (t/fn predef/number (t/fn predef/number predef/number))
          (e/app
           (typed-expr
            (t/fn predef/bool (t/fn predef/number (t/fn predef/number predef/number)))
            (e/var "if"))
           (typed-expr
            predef/bool
            (e/var "true"))))
         (typed-expr
          predef/number
          (e/var "1"))))
       (typed-expr
        predef/number
        (e/var "0"))))
     ))


   (test-case
    "if branches have same type"
    (check-exn
     exn:fail?
     (lambda ()
       (:? '(if true 1 true)))))))

(run-tests types-test)
