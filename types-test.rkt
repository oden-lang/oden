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
     (get-typed-expr (e/app identity (e/var "true")))
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
    "list/singleton"
    (check-equal?
     ;; (list/singleton 1) : (list number)
     (get-typed-expr (e/app (e/var "list/singleton") (e/var "1")))
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
     ;; (if true 1 0) : bool            ;
     (get-typed-expr (e/app
                      (e/app
                       (e/app (e/var "if") (e/var "true"))
                       (e/var "1"))
                      (e/var "0")))
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
     predef/number))


   (test-case
    "if branches have same type"
    (check-exn
     exn:fail?
     ;; (if true 1 true)                ;
     (lambda ()
       (get-typed-expr (e/app
                        (e/app
                         (e/app (e/var "if") (e/var "true"))
                         (e/var "1"))
                        (e/var "true"))))))))

(run-tests types-test)
