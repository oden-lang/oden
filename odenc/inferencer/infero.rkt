#lang racket

(provide infero)

(require Racket-miniKanren/miniKanren/mk)
(require "lookupo.rkt")
(require "arith-operatoro.rkt")
(require "generalizeo.rkt")
(require "erroro.rkt")

(define stringo (make-flat-tag 'string string?))
(define floato (make-flat-tag 'float flonum?))
(define fixedo (make-flat-tag 'fixed fixnum?))

(define (infero expr env t)
  (conde
   [(fresh (x)
           (symbolo expr)
           (lookupo expr env x)
           (conde
            [(fresh (_)
                    (erroro x)
                    (== t x))]
            [(fresh (_)
                    (not-erroro x)
                    (== `(,expr : ,x) t))]))]
   [(floato expr)
    (== `(,expr : float) t)]
   [(fixedo expr)
    (== `(,expr : int) t)]
   [(stringo expr)
    (== `(,expr : string) t)]
   [(== 'false expr)
    (== '(false : bool) t)]
   [(== 'true expr)
    (== '(true : bool) t)]

   ;; (e : t)
   [(fresh (s st te)
           (== `(,s : ,st) expr)
           (infero s env te)
           (conde
            [(fresh (ie)
                    ;; the inferred type should match the annotated type
                    (== `(,ie : ,st) te)
                    (== te t))]
            [(fresh (ie it)
                    (== `(,ie : ,it) te)
                    (== t `(error type-mismatch ,expr ,it ,st)))]))]

   [(fresh (x b bt b-ignore r d wd ft)
           (symbolo x)
           (conde
            ;; if no type is specified wrap with 'unbound'
            [(== `(fn (,x) ,b) expr)
             (== `(unbound ,d) wd)]
            ;; if type is specified just use that
            [(== `(fn ([,x : ,d]) ,b) expr)
             (== d wd)])

           (infero b `((,x : ,wd) . ,env) bt)
           (== `(,b-ignore : ,r) bt)
           (== `((fn ([,x : ,d]) ,bt) : (,d -> ,r)) t))]

   [(fresh (b bt b-ignore d)
           (== `(fn () ,b) expr)
           (infero b env bt)
           (== bt `(,b-ignore : ,d))
           (== `((fn () ,bt) : (-> ,d)) t))]
   [(fresh (x xt e e-ignore et b bt b-ignore lt)
           (conde
            [(== `(let ([,x ,e]) ,b) expr)]
            [(== `(let ([[,x : ,xt] ,e]) ,b) expr)])
           (symbolo x)
           (infero e env et)
           (== `(,e-ignore : ,xt) et)
           (generalizeo xt)
           (infero b `((,x : ,xt) . ,env) bt)
           (== `(,b-ignore : ,lt) bt)
           (== `((let [[(,x : ,xt) ,et]] ,bt) : ,lt) t))]
   [(fresh (f ft f-ignore a at a-ignore x et)
           (== `(,f ,a) expr)
           (infero f env ft)
           (infero a env at)
           (== `(,f-ignore : (,x -> ,et)) ft)
           (== `(,a-ignore : ,x) at)
           (== `((,ft ,at) : ,et) t))]
   [(fresh (f ft f-ignore et)
           (== `(,f) expr)
           (infero f env ft)
           (== `(,f-ignore : (-> ,et)) ft)
           (== `((,ft) : ,et) t))]
   [(fresh (c ct c-ignore a at a-ignore b bt b-ignore it)
           (== `(if ,c ,a ,b) expr)
           (infero c env ct)
           (== `(,c-ignore : bool) ct)
           (infero a env at)
           (infero b env bt)
           (== `(,a-ignore : ,it) at)
           (== `(,b-ignore : ,it) bt)
           (== `((if ,ct ,at ,bt) : ,it) t))]))

(module+ test
  (require rackunit)

  (test-case "identity fn"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(fn (x) x) '() `(,_ : ,q))
                  (== q `(a -> a))))
     '((a -> a))))

  (test-case "identity fn with type-annotated arg"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(fn ([x : int]) x) '() `(,_ : ,q))))
     '((int -> int))))

  (test-case "let identity fn"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(let ([f (fn (x) x)]) f) '() `(,_ : ,q))))
     '((_.0 -> _.0))))

  (test-case "instantiation of generalized let'd fn"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(let ([g f]) g)
                          '([f : ((var foo) -> (var foo))]) `(,_ : ,q))))
     '((_.0 -> _.0))))

  (test-case "instantiation of generalized fn"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero 'f '([f : ((var foo) -> (var foo))]) `(,_ : ,q))))
     '((_.0 -> _.0))))

  (test-case "multiple instantiations of generalized fn"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(f f) '([f : ((var foo) -> (var foo))]) `(,_ : ,q))))
     '((_.0 -> _.0))))

  (test-case "nested fn expressions"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(fn (x) (fn (y) x)) '() `(,_ : ,q))
                  (== q `(a -> (b -> a)))))
     '((a -> (b -> a)))))

  (test-case "predefined fn in fn expression"
    (check-equal?
     (run* (q)
           (fresh (_)
                  (infero '(fn (x) y)
                          '([y : (int -> int)]) `(,_ : ,q))))
     '((_.0 -> (int -> int)))))

  (test-case "fn application"
    (check-match
     (run* (q)
           (infero '((fn (x) x) 1) '() q))
     `((,e : int))))

  (test-case "partial application with predefined fn"
    (check-match
     (run* (q)
           (infero '(f 1)
                   '([f : ((var a) -> ((var b) -> (var a)))]) q))
     `((,e : (,t -> int)))))

  (test-case "partial application with fn expressions"
    (check-match
     (run* (q)
           (infero '((fn (x) (fn (y) x)) 1)
                   '() q))
     `((,e : (,t -> int)))))

  (test-case "undefined identifier error"
    (check-equal?
     (run* (q)
           (infero 'x '() q))
     '((error undefined-identifier x))))

  (test-case "type annotation mismatch"
    (check-equal?
     (run* (q)
           (infero '(1 : string) '() q))
     '((error type-mismatch (1 : string) int string))))

  )
