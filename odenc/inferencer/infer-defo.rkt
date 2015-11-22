#lang racket

(provide infer-defo)

(require Racket-miniKanren/miniKanren/mk)
(require "infero.rkt")
(require "generalizeo.rkt")

(define (infer-defo def env t)
  (fresh (rec-env name expr expr-ignore expr-t expr-te)
         (== `(define ,name ,expr) def)
         (== rec-env (cons `(,name : (unbound ,expr-t)) env))
         (infero expr rec-env expr-te)
         (== `(,expr-ignore : ,expr-t) expr-te)
         (generalizeo expr-t)
         (== `((define ,name ,expr-te) : ,expr-t) t)))

(module+ test
  (require rackunit)
  
  (test-case "define monomorphic identity fn with type-annotated arg"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo '(define identity (fn ([x : int]) x)) '() q)))
     `(((define identity ,f) : (int -> int)))))
  
  (test-case "define monomorphic identity fn wrapped in type-annotation"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo '(define identity ((fn (x) x) : (int -> int))) '() q)))
     `(((define identity ,f) : (int -> int)))))
  
  (test-case "define polymorphic identity fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo
                   '(define identity (fn (x) x))
                   '()
                   `(,_ : ,q))))
     `(((var ,v) -> (var ,v)))))

  (test-case "define recursive monomorphic fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo
                   '(define inf ((fn (x) (inf x)) : (int -> int)))
                   '()
                   `(,_ : ,q))))
     `((int -> int))))

  (test-case "define recursive polymorphic fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo
                   '(define inf (fn (x) (inf x)))
                   '()
                   `(,_ : ,q))))
     `(((var ,v1) -> (var ,v2)))))
  )
