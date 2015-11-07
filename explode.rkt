#lang racket

(provide explode)

(define (explode expr)
  (match expr
    [`(lambda () ,body)
     `(lambda ,(list (gensym "_explode_")) ,body)]
    [`(lambda (,arg) ,body)
     `(lambda (,arg) ,(explode body))]
    [`(lambda ,args ,body)
     (explode `(lambda ,(list (car args)) ,(explode `(lambda ,(cdr args) ,body))))]
    [`(let ((,s ,v)) ,b)
     `(let ((,s ,(explode v))) ,(explode b))]
    [`(let ,ps ,b)
     `(let ,(list (car ps)) ,(explode `(let ,(cdr ps) ,b)))]
    [`(,f ,a) `(,(explode f) ,(explode a))]
    ;; (f x y z) -> (((f x) y) z)
    [`(,f . ,args)
     (explode `((,f ,(car args)) . ,(cdr args)))]
    [e e]))

