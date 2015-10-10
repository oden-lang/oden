#lang racket

(require "minikanren/mk.rkt")

(struct e/var (name) #:transparent)
(struct e/app (fn param) #:transparent)
(struct e/lam (arg body) #:transparent)

(define (t/value type)
  (list ':value type))
(define (t/fn domain range)
  (list ':fn domain range))
(define (t/oper name args)
  (list ':oper name args))
(define (t/failure msg)
  (list ':failure msg))

(define/match (t/failure? t)
  [((list ':failure _)) #t]
  [(_) #f])

(define/contract (t/failure-message t)
  (-> t/failure? string?)
  (first (rest t)))

(define succeed (== #t #t))

(define/match (show-e expr)
  [((e/var name)) name]
  [((e/app fn param)) (format "(~a ~a)" (show-e fn) (show-e param))]
  [((e/lam arg body)) (format "(lambda (~a) ~a)" (show-e arg) (show-e body))])

; predefined types
(define predef/bool (t/oper "bool" '()))
(define predef/number (t/oper "number" '()))
(define (predef/list t) (t/oper "list" (list t)))

; unifies t with the type of the expression
(define (unify-type t expr scope)
  (match expr

    ;; var
    [(e/var name) (if (hash-has-key? scope name)
                      (== t (hash-ref scope name))
                      (let ([n (string->number name)])
                        (if n
                            (== t predef/number)
                            (== t (t/failure (format "Undefined: ~a" name))))))]

    ;; application
    [(e/app fn param)
     (fresh (fn-type param-type)
            (== fn-type (t/fn param-type t))
            (unify-type fn-type fn scope)
            (unify-type param-type param scope))]

    ;; lambda abstraction
    [(e/lam (e/var arg-name) body)
     (fresh (arg-type body-type)
            (== t (t/fn arg-type body-type))
            (unify-type
             body-type
             body
             (hash-set scope arg-name arg-type)))]
    [e
     (== t (t/failure (format "Invalid expression: ~a" e)))]))

;; TODO: introduce variadic functions and get rid of this
(define (predef/unify-singleton t)
  (fresh (a)
         (== t (t/fn a (predef/list a)))))

(define (get-type expr)
  (let ([t (run 1 (q)
                (fresh (singleton)
                       (predef/unify-singleton singleton)
                       (let ([predefined-symbols
                              (hash "true" predef/bool
                                    "false" predef/bool
                                    "list/singleton" singleton)])
                   (unify-type q expr predefined-symbols))))])
    (cond
     [(empty? t) (error "Type check failed:" t)]
     [(t/failure? (first t)) (error (t/failure-message (first t)))]
     [else (first t)])))

(provide
 e/var
 e/lam
 e/app
 t/value
 t/fn
 t/oper
 t/failure
 get-type
 predef/bool
 predef/number
 predef/list)
