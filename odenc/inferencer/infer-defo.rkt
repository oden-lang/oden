#lang racket

(provide infer-defo)

(require Racket-miniKanren/miniKanren/mk)
(require "infero.rkt")

(define (infer-defo def env t)
  (fresh (rec-env name expr expr-ignore expr-t expr-te)
         (== `(define ,name ,expr) def)
         (== rec-env (cons `(,name : ,expr-t) env))
         (infero expr rec-env expr-te)
         (== `(,expr-ignore : ,expr-t) expr-te)
         (== `((define ,name ,expr-te) : ,expr-t) t)))

(module+ test
  ;; todo
  )
