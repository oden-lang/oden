#lang racket

(require "miniKanren/mk.rkt")

(define (infero expr env t)
  (conde
   [(fresh (x)
           (symbolo expr)
           (lookupo expr env x)
           (== `(,expr : ,x) t))]
   [(numbero expr)
    (== `(,expr : int) t)]
   [(== 'false expr)
    (== '(false : bool) t)]
   [(== 'true expr)
    (== '(true : bool) t)]
   [(fresh (s  st)
           (== `(,s : ,st) expr)
           (infero s env expr)
           (== expr t))]
   [(fresh (s st)
           (== `(,s : ,st) expr)
           (infero s env `(,s : ,st))
           (== expr t))]
   [(fresh (x b bt b-ignore r d)
           (symbolo x)
           (conde
            [(== `(lambda (,x) ,b) expr)]
            [(== `(lambda ([,x : ,r]) ,b) expr)])
           (== bt `(,b-ignore : ,d))
           (infero b `((,x : ,r) . ,env) bt)
           (== `((lambda ([,x : ,r]) ,bt) : (,r -> ,d)) t))]
   [(fresh (b bt b-ignore d)
	   (== `(lambda () ,b) expr)
	   (infero b env bt)
	   (== bt `(,b-ignore : ,d))
	   (== `((lambda () ,bt) : (-> ,d)) t))]
   [(fresh (x xt e e-ignore et b bt b-ignore lt)
           (conde
            [(== `(let ([,x ,e]) ,b) expr)]
            [(== `(let ([[,x : ,xt] ,e]) ,b) expr)])
           (symbolo x)
           (== et `(,e-ignore : ,xt))
           (infero e env et)
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
           (== `((if ,ct ,at ,bt) : ,it) t))]
   #;
   [(fresh (x h ht h-ignore r rt r-ignore)
           (== `(cons ,h ,r) expr)
           (infero h env ht)
           (infero r env rt)
           (== `(,h-ignore : ,x) ht)
           (== `(,r-ignore : (list ,x)) rt)
   (== t `((cons ,ht ,rt) : (list ,x))))]
   #;
   [(fresh (x)
           (== '() expr)
           (== t `(() : (list ,x))))]))

(define (lookupo x env t)
  (fresh ()
         (symbolo x)
         (conde
          ;; todo: uncomment when I grok this :)
          #;
          ((fresh (e env^ _)
                  (== `((,x poly ,e ,env^) . ,_) env)
                  (infero e env^ t)))
          ((fresh (_)
                  (== `((,x : ,t) . ,_) env)))
          ((fresh (y _ env^)
                  (== `((,y . ,_) . ,env^) env)
                  (=/= x y)
                  (symbolo y)
                  (lookupo x env^ t))))))

(define (default-envo t)
  (fresh (+t)
         (conde
          [(== +t 'int)]
          [(== +t 'string)]
          [(== +t 'long)])
         (== t `[(+ : (,+t -> (,+t -> ,+t)))
                 (- : (,+t -> (,+t -> ,+t)))
                 (* : (,+t -> (,+t -> ,+t)))
                 (/ : (,+t -> (,+t -> ,+t)))
                 (== : (,+t -> (,+t -> ,+t)))
                 (!= : (,+t -> (,+t -> ,+t)))])))

(define/contract (:? expr)
  (-> (or/c symbol? list? number?) (or/c symbol? list?))
  (let ([t (run 1 (q)
                (fresh (env)
                       (default-envo env)
                       (infero expr env q)))])
    (cond
     [(empty? t) (raise-user-error "Type check failed!")]
     [else (first t)])))

(provide
 infero
 :?)
