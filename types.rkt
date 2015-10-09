#lang racket

(require "minikanren/mk.rkt")

(struct e-var (name) #:transparent)
(struct e-app (fn param) #:transparent)
(struct e-lam (arg body) #:transparent)

(define (t-value type)
  (list ':value type))
(define (t-fn domain range)
  (list ':fn domain range))
(define (t-oper name args)
  (list ':oper name args))
(define (t-failure msg)
  (list ':failure msg))

(define/match (t-failure? t)
  [((list ':failure _)) #t]
  [(_) #f])

(define/contract (t-failure-message t)
  (-> t-failure? string?)
  (first (rest t)))

(define succeed (== #t #t))

(define/match (show-e expr)
  [((e-var name)) name]
  [((e-app fn param)) (format "(~a ~a)" (show-e fn) (show-e param))]
  [((e-lam arg body)) (format "(lambda (~a) ~a)" (show-e arg) (show-e body))])

(define bool (t-oper "bool" '()))
(define number (t-oper "number" '()))

; unifies t with the type of the expression
(define (unify-type t expr scope)
  (match expr

    ;; var
    [(e-var name) (if (hash-has-key? scope name)
                      (== t (hash-ref scope name))
                      (let ([n (string->number name)])
                        (if n
                            (== t number)
                            (== t (t-failure (format "Undefined: ~a" name))))))]

    ;; application
    [(e-app fn param)
     (fresh (fn-type param-type)
            (== fn-type (t-fn param-type t))
            (unify-type fn-type fn scope)
            (unify-type param-type param scope))]

    ;; lambda abstraction
    [(e-lam (e-var arg-name) body)
     (fresh (arg-type body-type)
            (== t (t-fn arg-type body-type))
            (unify-type
             body-type
             body
             (hash-set scope arg-name arg-type)))]))

(define predefined-types (hash "true" bool
                               "false" bool))

(define (get-type expr)
  (let ([t (run 1 (q) (unify-type q expr predefined-types))])
    (cond
     [(empty? t) (error "Type check failed!")]
     [(t-failure? (first t)) (error (t-failure-message (first t)))]
     [else (first t)])))

(define identity (e-lam (e-var "x") (e-var "x")))
