#lang racket

(require "miniKanren/mk.rkt")

(define (infero expr env t)
  (conde
   [(symbolo expr)
    (lookupo expr env t)]
   [(numbero expr)
    (== 'int t)]
   [(== 'false expr)
    (== 'bool t)]
   [(== 'true expr)
    (== 'bool t)]
   [(fresh (s)
           (== `(,s : ,t) expr)
           (infero s env t))]
   [(fresh (x b r d)
           (conde
            [(== `(lambda (,x) ,b) expr)]
            [(== `(lambda ([,x : ,r]) ,b) expr)])
           (== `(,r -> ,d) t)
           (symbolo x)
           (infero b `((,x : ,r) . ,env) d))]
   [(fresh (x e et b)
           (== `(let (,x ,e) ,b) expr)
           (symbolo x)
           (infero e env et)
           (infero b `((,x : ,et) . ,env) t))]
   [(fresh (f a ft at)
           (== `(,f ,a) expr)
           (infero f env ft)
           (infero a env at)
           (== `(,at -> ,t) ft))]
   [(fresh (c a b)
           (== `(if ,c ,a ,b) expr)
           (infero c env 'bool)
           (infero a env t)
           (infero b env t))]
   [(fresh (x h r)
           (== `(cons ,h ,r) expr)
           (infero h env x)
           (infero r env t)
           (== t `(list ,x)))]
   [(fresh (x)
           (== '() expr)
           (== t `(list ,x)))]))

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

(define/contract (:? expr)
  (-> (or/c symbol? list?) (or/c symbol? list?))
  (let ([t (run 1 (q) (infero expr '() q))])
    (cond
     [(empty? t) (error "Type check failed!")]
     [else (first t)])))

(provide
 infero
 :?)
