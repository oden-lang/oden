#lang racket

(provide (all-defined-out))

(require unstable/hash)
(require "make-monomorph-name.rkt")
(require "instantiate-p-def.rkt")

(struct
  monomorphed-def
  (name instance-name def)
  #:transparent)

(struct
  monomorphed-binding
  (name instance-name)
  #:transparent)

(define (hash-replace v1 v2) v2)

(define/contract (monomorph p-defs
                            p-bindings
                            expr
                            [m-defs (hash)]
                            [m-bindings (hash)]
                            [local '()])
  (->* (hash? hash? any/c)
       ((hash/c (list/c symbol? any/c) monomorphed-def?)
        (hash/c (list/c symbol? any/c) monomorphed-binding?)
        (listof symbol?))
       (list/c any/c
               (hash/c (list/c symbol? any/c) monomorphed-def?)
               (hash/c (list/c symbol? any/c) monomorphed-binding?)))
  (match expr
    [`(,(? symbol? s) : ,t)
     (if (and (not (member s local))
              (hash-has-key? p-defs s))
         
         ;; this was a reference to a p-def
         (let* ([key (list s t)]
                [mname (make-monomorph-name s t)])
           (if (hash-has-key? m-defs key)
               ;; already monomorphed
               `((,mname : ,t) ,m-defs ,m-bindings)
               
               ;; monomorph the p-def recursively
               (let* ([p-def (hash-ref p-defs s)]
                      [instance (instantiate-p-def p-def t mname)])
                 ;; TODO: if the p-def is calling itself recursively this should never exit?
                 (match (monomorph p-defs p-bindings instance m-defs m-bindings '())
                   [`(,def ,m-defs ,m-bindings)
                    `((,mname : ,t)
                      ,(hash-set m-defs key (monomorphed-def s mname def))
                      ,m-bindings)]))))

         ;; not a p-def reference
         `(,expr ,m-defs ,m-bindings))]
    [`(define ,name ,e)
     (match (monomorph p-defs p-bindings e m-defs m-bindings local)
       [`(,m-expr ,m-defs ,m-bindings) `((define ,name ,m-expr) ,m-defs ,m-bindings)])]
    [(? number?) `(,expr ,m-defs ,m-bindings)]
    [(? string?) `(,expr ,m-defs ,m-bindings)]
    [`(,e : ,t)
     (let ([sub (monomorph p-defs p-bindings e m-defs m-bindings local)])
       `((,(car sub) : ,t) ,(cadr sub) ,(caddr sub)))]
    [`(let ([,name ,value]) ,body)
     (let* ([value-sub (monomorph p-defs p-bindings value m-defs m-bindings local)]
            [body-sub
             (monomorph p-defs
                        ;; TODO: Add polymorphic let binding
                        p-bindings
                        body
                        (hash-union (cadr value-sub) m-defs #:combine hash-replace)
                        (hash-union (caddr value-sub) m-bindings #:combine hash-replace)
                        (cons name local))])
       ;; TODO: Instantiate all let monomorphizations
       `((let ([,name ,(car value-sub)])
           ,(car body-sub))
         ,(cadr body-sub)
         ,(caddr body-sub)))]
    [`(fn ((,a : ,_)) ,b)
     (let ([b-sub (monomorph p-defs p-bindings b m-defs m-bindings (cons a local))])
       `((fn (,a) ,(car b-sub))
         ,(cadr b-sub)
         ,(caddr b-sub)))]
    [`(fn () ,b)
     (let ([b-sub (monomorph p-defs p-bindings b m-defs m-bindings local)])
       `((fn () ,(car b-sub))
         ,(cadr b-sub)
         ,(caddr b-sub)))]
    [`(,f ,a)
     (let* ([f-sub (monomorph p-defs p-bindings f m-defs m-bindings local)]
            [a-sub (monomorph p-defs p-bindings a m-defs m-bindings local)]
            [all-m-defs (hash-union (cadr f-sub) (cadr a-sub) #:combine hash-replace)]
            [all-m-bindings (hash-union (caddr f-sub) (caddr a-sub) #:combine hash-replace)])
       `((,(car f-sub) ,(car a-sub)) ,all-m-defs ,all-m-bindings))]
    [`(,f)
     (let* ([f-sub (monomorph p-defs p-bindings f m-defs m-bindings local)])
       `((,(car f-sub)) ,(cadr f-sub) ,(caddr f-sub)))]
    [`(if ,c ,a ,b)
     (let* ([c-sub (monomorph p-defs p-bindings c m-defs m-bindings local)]
            [a-sub (monomorph p-defs p-bindings a m-defs m-bindings local)]
            [b-sub (monomorph p-defs p-bindings b m-defs m-bindings local)]
            [all-m-defs (hash-union (cadr c-sub) (cadr a-sub) (cadr b-sub) #:combine hash-replace)]
            [all-m-bindings (hash-union (caddr c-sub) (caddr a-sub) (caddr b-sub) #:combine hash-replace)])
       `((if ,(car c-sub) ,(car a-sub) ,(car b-sub))
         ,all-m-defs
         ,all-m-bindings))]
    [e (error (format "Cannot monomorph expr: ~v" e))]))

(module+ test
  (require rackunit)
  (require "../inferencer.rkt")

  (define env '((identity : ((var a) -> (var a)))
                (identity-again : ((var a) -> (var a)))))

  (define p-defs (hash 'identity
                      (infer-def '(define identity (fn (x) x)))
                      'identity-again
                      (infer-def '(define identity-again (fn (x) (identity x))) env)))

  (test-case "one instance of a fn"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '(fn ([y : int]) (identity y)) env))
     `((,e : (int -> int))
       ,(hash-table
         ('(identity (int -> int))
          (monomorphed-def 'identity
                           'identity_inst_int_to_int
                           `((define identity_inst_int_to_int ,expr) : (int -> int)))))
       ,_)))

  (test-case "let without polymorphic function"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '(let ([name "foo"]) name) env))
     `((,e : string)
       ,(hash-table)
       ,_)))

  (test-case "no-arg fn application"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '((fn () 123)) env))
     `((,e : int)
       ,(hash-table)
       ,_)))

  (test-case "two different instances of one fn"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '(fn ([y : int]) ((identity identity) y)) env))
     `((,e : (int -> int))
       ,(hash-table
         ('(identity (int -> int))
          (monomorphed-def 'identity
                           'identity_inst_int_to_int
                           `((define identity_inst_int_to_int ,name) : (int -> int))))
         ('(identity ((int -> int) -> (int -> int)))
          (monomorphed-def 'identity
                           'identity_inst_int__to__int_to_int__to__int
                           `((define identity_inst_int__to__int_to_int__to__int ,expr)
                             :
                             ((int -> int) -> (int -> int))))))
       ,_)))


  (test-case "two calls with one instance of a fn"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '(fn ([y : int]) (identity (identity y))) env))
     `((,e : (int -> int))
       ,(hash-table
         ('(identity (int -> int))
          (monomorphed-def 'identity
                           'identity_inst_int_to_int
                           `((define identity_inst_int_to_int ,expr) : (int -> int)))))
       ,_)))

  (test-case "multiple levels of p-def calls"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '(fn ([y : int]) (identity-again y)) env))
     `((,e : (int -> int))
       ,(hash-table
         ('(identity (int -> int))
          (monomorphed-def 'identity
                           'identity_inst_int_to_int
                           _))           
         ('(identity-again (int -> int))
          (monomorphed-def 'identity-again
                           'identity-again_inst_int_to_int
                           _)))
       ,_)))
  
  (test-case "calls to let-aliased polymorphic function"
    (check-match
     (monomorph p-defs
                (hash)
                (infer '(let ([my-id identity]) (my-id 1)) env))
     `((,e : (int -> int))
       ,(hash-table
         ('(identity (int -> int))
          (monomorphed-def 'identity
                           'identity_inst_int_to_int
                           `((define identity_inst_int_to_int ,expr) : (int -> int)))))
       ,_)))
  
  )
