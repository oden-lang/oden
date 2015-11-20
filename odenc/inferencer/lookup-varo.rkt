#lang racket

(provide
 varo
 not-varo
 lookup-varo)

(require Racket-miniKanren/miniKanren/mk)

(define (varo v)
  (fresh (_)
         (== `(var ,_) v)))

(define (not-varo v)
  (conda
   [(varo v) fail]
   [succeed]))

(define (lookup-varo s env t)
  ;(varo s)
  (conda
   [(fresh (_)
           (== `((,s ,t) . ,_) env))]
   [(fresh (_ s^ env^)
           (== `((,s^ ,_) . ,env^) env)
           (=/= s s^)
           (lookup-varo s env^ t))]
   [(== '() env)]))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  
  ;; TODO!
  )
