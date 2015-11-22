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

  (test-case "define monomorphic identity fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo '(define identity (fn ([x : int]) x)) '() q)))
     `(((define identity ,f) : (int -> int)))))
  
  (test-case "define polymorphic identity fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo '(define identity (fn (x) x)) '() q)))
     `(((define identity ,f) : ((var ,v) -> (var ,v))))))

  (test-case "define recursive monomorphic fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo '(define inf (fn ([x : int]) (inf x))) '() q)))
     `(((define inf ,f) : (int -> int)))))

  (test-case "define recursive polymorphic fn"
    (check-match
     (run* (q)
           (fresh (_)
                  (infer-defo '(define inf (fn (x) (inf x))) '() q)))
     `(((define inf ,f) : ((var ,v) -> (var ,v))))))
  )
