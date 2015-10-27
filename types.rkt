#lang racket

(require "minikanren/mk.rkt")

(define (e/var name)
  (list ':var name))
(define (e/app fn param)
  (list ':app fn param))
(define (e/lam arg body)
  (list ':lam arg body))

(define (t/fn domain range)
  (list ':fn domain range))
(define (t/constructor name args)
  (list ':constructor name args))
(define (t/failure msg)
  (list ':failure msg))

(define (typed-expr type expr)
  (list type expr))

(define/match (t/failure? t)
  [((list ':failure _)) #t]
  [(_) #f])

(define/contract (t/failure-message t)
  (-> t/failure? string?)
  (first (rest t)))

(define/match (show-t type)
  [((list ':constructor name '())) name]
  [((list ':constructor name args)) (format "(~a ~a)" name (map show-t args))]
  [((list ':fn domain range)) (format "(-> ~a ~a)" (show-t domain) (show-t range))]
  [((list ':failure msg)) (format "Failure: ~a" msg)]
  [((? symbol? s)) (~a s)])

(define/match (show-typed-expr type)
  [((list type (list ':var name))) (format "(: ~a ~a)"
                                           (show-t type)
                                           name)]
  [((list type (list ':app fn param))) (format "(: ~a (~a ~a))"
                                               (show-t type)
                                               (show-typed-expr fn)
                                               (show-typed-expr param))]
  [((list type (list ':lam arg body))) (format "(: ~a (lambda (~a) ~a))"
                                               (show-t type)
                                               (show-typed-expr arg)
                                               (show-typed-expr body))]
  [((? symbol? s)) (~a s)])

(define succeed (== #t #t))

(define/match (show-e expr)
  [((list ':var name)) name]
  [((list ':app fn param)) (format "(~a ~a)" (show-e fn) (show-e param))]
  [((list ':lam arg body)) (format "(lambda (~a) ~a)" (show-e arg) (show-e body))])

; predefined types
(define predef/bool (t/constructor "bool" '()))
(define predef/number (t/constructor "number" '()))
(define (predef/list t) (t/constructor "list" (list t)))

; unifies t with a typed expression
(define (type-expr t expr scope)
  (match expr

    ;; var
    [(list ':var name) (if (hash-has-key? scope name)
                      (== t (typed-expr
                             (hash-ref scope name)
                             (e/var name)))
                      (let ([n (string->number name)])
                        (if n
                            (== t (typed-expr predef/number (e/var name)))
                            (== t (t/failure (format "Undefined: ~a" name))))))]

    ;; application
    [(list ':app fn param)
     (fresh (expr-type fn-te fn-type fn-expr-te param-te param-type param-expr-te)
            ;; the application typed expression itself
            (== t (typed-expr
                   expr-type
                   (e/app fn-te param-te)))

            ;; the type of the application should be the range of the
            ;; function
            (== fn-te (typed-expr
                       (t/fn param-type expr-type)
                       fn-expr-te))
            ;; the parameter type should match the domain of the
            ;; applied function
            (== param-te (typed-expr
                          param-type
                          param-expr-te))

            ;; recursively unify fn and param typed expressions
            (type-expr fn-te fn scope)
            (type-expr param-te param scope))]

    ;; lambda abstraction
    [(list ':lam (list ':var arg-name) body)
     (fresh (arg-te arg-type body-te body-type body-expr-te)
            (== t (typed-expr
                   (t/fn arg-type body-type)
                   (e/lam arg-te body-te)))


            (== arg-te (typed-expr
                        arg-type
                        (e/var arg-name)))
            (== body-te (typed-expr
                         body-type
                         body-expr-te))

            (type-expr
             body-te
             body
             (hash-set scope arg-name arg-type)))]
    [e
     (== t (t/failure (format "Invalid expression: ~a" e)))]))

;; TODO: introduce variadic functions and get rid of this
(define (predef/unify-singleton t)
  (fresh (a)
         (== t (t/fn a (predef/list a)))))

(define (predef/unify-if t)
  (fresh (a)
         (== t (t/fn predef/bool (t/fn a (t/fn a a))))))

(define (get-typed-expr expr)
  (let ([t (run 1 (q)
                (fresh (singleton-t if-t)
                       (predef/unify-singleton singleton-t)
                       (predef/unify-if if-t)
                       (let ([predefined-symbols
                              (hash "true" predef/bool
                                    "false" predef/bool
                                    "list/singleton" singleton-t
                                    "if" if-t)])
                   (type-expr q expr predefined-symbols))))])
    (cond
     [(empty? t) (error "Type check failed:" t)]
     [(t/failure? (first t)) (error (t/failure-message (first t)))]
     [else (first t)])))

(provide
 e/var
 e/lam
 e/app
 t/fn
 t/constructor
 t/failure
 typed-expr
 get-typed-expr
 show-typed-expr
 predef/bool
 predef/number
 predef/list)
