#lang racket

(provide get-non-local-references)

(define/contract (get-non-local-references expr [local '()])
  (->* (any/c) ((listof symbol?)) (listof symbol?))
  (match expr
    [(? symbol? s)
     (if (member s local)
         '()
         (list s))]
    [(? number?) '()]
    [(? string?) '()]
    [`(,e : ,t)
     (get-non-local-references e local)]
    [`(let ([,name ,expr]) ,body)
     (append (get-non-local-references expr local)
             (if (member name local)
                 (get-non-local-references body local)
                 (get-non-local-references body (cons name local))))]
    [`(fn (,a) ,b)
     (get-non-local-references b (cons a local))]
    [`(fn () ,b)
     (get-non-local-references b local)]
    [`(,f ,a)
     (append (get-non-local-references f local)
             (get-non-local-references a local))]
    [`(,f)
     (get-non-local-references f local)]
    [`(if ,c ,a ,b)
     (append (get-non-local-references c local)
             (get-non-local-references a local)
             (get-non-local-references b local))]
    [e (error (format "Cannot get non-local references of expr: ~v" e))]))

(module+ test
  (require rackunit)
  (require "explode.rkt")

  (test-case "fn"
    (check-equal?
     (get-non-local-references
      (explode '(fn (x y) (f x y))))
     '(f)))

  (test-case "fn application"
    (check-equal?
     (get-non-local-references
      (explode '((fn (x y) (f x y)) 1 2)))
     '(f)))

    (test-case "no-arg fn application"
    (check-equal?
     (get-non-local-references
      (explode '((fn () (f)))))
     '(f)))

  (test-case "let"
    (check-equal?
     (get-non-local-references
      (explode '(let ([x y]) x)))
     '(y))))
