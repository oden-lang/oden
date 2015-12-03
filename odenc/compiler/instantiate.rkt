#lang racket

(provide
 instantiate-p-def
 instantiate-typed-expr)

(require "get-substitutions.rkt")

(define (substitute-type type subs)
  (match type
    [`(var ,p) (hash-ref subs `(var ,p))]
    [`(-> ,t) `(-> ,(substitute-type t subs))]
    [`(,t1 -> ,t2) `(,(substitute-type t1 subs)
                     ->
                     ,(substitute-type t2 subs))]
    [`(,t . ,ts) (cons (substitute-type t subs)
                       (substitute-type ts subs))]
    ['() '()]
    [(? symbol? s) s]
    [_ (error
        (format "Cannot substitute types of ~a" type))]))

(define (substitute-typed-expr te subs)
  (match te
    [`(define ,name ,e)
     `(define ,name ,(substitute-typed-expr e subs))]
    [`(,e : ,t) `(,(substitute-typed-expr e subs)
                  :
                  ,(substitute-type t subs))]
    [`(let ([,name ,value]) ,body)
     `(let ([,(substitute-type name) ,(substitute-typed-expr value subs)])
        ,(substitute-typed-expr body subs))]
    [`(fn (,a) ,b)
     `(fn (,(substitute-type a subs))
        ,(substitute-typed-expr b subs))]
    [`(if ,c ,a ,b)
     `(if (substitute-typed-expr c subs)
          (substitute-typed-expr a subs)
          (substitute-typed-expr b subs))]
    (`(,f ,a)
     `(,(substitute-typed-expr f subs)
       ,(substitute-typed-expr a subs)))
    [(? number?) te]
    [(? symbol?) te]
    [(? string?) te]
    ))

(define (instantiate-p-def p-def type instance-name)
  (match (substitute-typed-expr
          p-def
          (get-substitutions (caddr p-def) type))
    [`((define ,name ,e) : ,t)
     `((define ,instance-name ,e) : ,t)]))

(define (instantiate-typed-expr typed-expr type)
  (substitute-typed-expr
   typed-expr
   (get-substitutions (caddr typed-expr) type)))

(module+ test
  (require rackunit)
  (require "../inferencer.rkt")

  (test-case "instantiates identity for (int -> int)"
    (check-match
     (instantiate-p-def
      (infer-def '(define identity (fn (x) x)))
      '(int -> int)
      'some-name)
     `((define some-name ,e) : (int -> int))))

  (test-case "instantiates identity for ((int -> int) (int -> int))"
    (check-match
     (instantiate-p-def
      (infer-def '(define identity (fn (x) x)))
      '((int -> int) -> (int -> int))
      'some-name)
     `((define some-name ,e) : ((int -> int) -> (int -> int)))))

  (test-case "instantiates identity expr for (int -> int)"
    (check-match
     (instantiate-typed-expr
      '((fn ([x : int]) (x : int)) : ((var a) -> (var a)))
      '(int -> int))
     `((fn ([x : int]) (x : int)) : (int -> int))))
  )

