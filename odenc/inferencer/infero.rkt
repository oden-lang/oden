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
           (lookupo expr env t))]
   [(floato expr)
    (== `(,expr : float) t)]
   [(fixedo expr)
    (== `(,expr : int) t)]
   [(stringo expr)
    (== `(,expr : string) t)]

   ;; (e : t)
   [(fresh (s st te)
           (== `(,s : ,st) expr)
           (infero s env te)
           (conda
            [(fresh (ie)
                    ;; the inferred type should match the annotated type
                    (== `(,ie : ,st) te)
                    (== te t))]
            [(fresh (ie it)
                    (== `(,ie : ,it) te)
                    (=/= it st)
                    (== t `(error type-mismatch ,expr ,it ,st)))]
            [(erroro te)
             (== t te)]))]

   [(fresh (x b bt b-ignore cd d wd ft)
           (symbolo x)
           (conde
            ;; if no type is specified wrap with 'unbound'
            [(== `(fn (,x) ,b) expr)
             (== `(unbound ,d) wd)]
            ;; if type is specified just use that
            [(== `(fn ([,x : ,d]) ,b) expr)
             (== d wd)])

           (infero b `((,x : ,wd) . ,env) bt)
           (conde
            [(== `(,b-ignore : ,cd) bt)
             (== `((fn ([,x : ,d]) ,bt) : (,d -> ,cd)) t)]
            [(erroro bt)
             (== t bt)]))]

   [(fresh (b bt b-ignore d)
           (== `(fn () ,b) expr)
           (infero b env bt)
           (conde
            [(== bt `(,b-ignore : ,d))
             (== `((fn () ,bt) : (-> ,d)) t)]
            [(erroro bt)
             (== t bt)]))]
   
   [(fresh (x xt e e-ignore et b bt b-ignore lt)
           (conde
            [(== `(let ([,x ,e]) ,b) expr)]
            [(== `(let ([[,x : ,xt] ,e]) ,b) expr)])
           (symbolo x)
           (infero e env et)
           (conde
            [(== `(,e-ignore : ,xt) et)
             (generalizeo xt)
             (infero b `((,x : ,xt) . ,env) bt)
             (conde
              [(== `(,b-ignore : ,lt) bt)
               (== `((let [[(,x : ,xt) ,et]] ,bt) : ,lt) t)]
              [(erroro bt)
               (== t bt)])]
            [(erroro et)
             (== t et)]))]
   
   [(fresh (f ft f-ignore a at a-ignore fx ax et)
           (== `(,f ,a) expr)
           ;(=/= f 'fn)
           ;(=/= f 'let)
           (infero f env ft)
           (conde
            [(== `(,f-ignore : (,fx -> ,et)) ft)
             (infero a env at)
             (conde
              [(== `(,a-ignore : ,ax) at)
               (conda
                [(== fx ax)
                 (== `((,ft ,at) : ,et) t)]
                [(== `(error type-mismatch ,expr ,ax ,fx) t)]
                )]
              [(erroro at)
               (== t at)])]
            [(erroro ft)
             (== t ft)]))]
   
   [(fresh (f ft f-ignore et)
           (== `(,f) expr)
           (infero f env ft)
           (conde
            [(== `(,f-ignore : (-> ,et)) ft)
             (== `((,ft) : ,et) t)]
            [(erroro ft)
             (== t ft)]))]
   
   [(fresh (c ct c-ignore a at a-ignore b bt b-ignore it)
           (== `(if ,c ,a ,b) expr)
           (infero c env ct)
           (conde
            [(erroro ct)
             (== t ct)]
            [(fresh (_ c-type)
                    (== `(,_ : ,c-type) ct)
                    (=/= c-type 'bool)
                    (== `(error type-mismatch ,c ,c-type bool) t))]
            [(== `(,c-ignore : bool) ct)
             (infero a env at)
             (conde
              [(erroro at)
               (== t at)]
              [(== `(,a-ignore : ,it) at)
               (infero b env bt)
               (conde
                [(erroro bt)
                 (== t bt)]
                [(fresh (b-it)
                        (== `(,b-ignore : ,b-it) bt)
                        (conde
                         [(== it b-it)
                          (== `((if ,ct ,at ,bt) : ,it) t)]
                         [(=/= it b-it)
                          (== `(error if-branch-type-mismatch ,expr ,it ,b-it) t)]))])])]))]))

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

  (test-case "undefined-identifier error in fn body"
    (check-equal?
     (run* (q)
           (infero '(fn (x) y) '() q))
     '((error undefined-identifier y))))

  (test-case "undefined-identifier error in no-arg fn body"
    (check-equal?
     (run* (q)
           (infero '(fn () y) '() q))
     '((error undefined-identifier y))))

  (test-case "undefined-identifier error in let binding"
    (check-equal?
     (run* (q)
           (infero '(let ([x y]) x) '() q))
     '((error undefined-identifier y))))

  (test-case "undefined-identifier error in let body"
    (check-equal?
     (run* (q)
           (infero '(let ([x 10]) y) '() q))
     '((error undefined-identifier y))))

  (test-case "fn application argument type mismatch"
    (check-equal?
     (run* (q)
           (infero '((fn ([x : string]) x) 1) '() q))
     '((error type-mismatch ((fn ((x : string)) x) 1)  int string))))

  (test-case "no-arg fn application argument error propagation"
    (check-equal?
     (run* (q)
           (infero '((fn () foo)) '() q))
     '((error undefined-identifier foo))))

  (test-case "if condition error propagation"
    (check-equal?
     (run* (q)
           (infero '(if foo true false) '() q))
     '((error undefined-identifier foo))))

  (test-case "if then error propagation"
    (check-equal?
     (run* (q)
           (infero '(if true foo false) '() q))
     '((error undefined-identifier foo))))

  (test-case "if else error propagation"
    (check-equal?
     (run* (q)
           (infero '(if false true foo) '() q))
     '((error undefined-identifier foo))))

  (test-case "if condition type mismatch"
    (check-equal?
     (run* (q)
           (infero '(if "hello" true false) '() q))
     '((error type-mismatch "hello" string bool))))

  (test-case "if branches type mismatch"
    (check-equal?
     (run* (q)
           (infero '(if true 1 "hello") '() q))
     '((error if-branch-type-mismatch (if true 1 "hello") int string))))
  
  )
