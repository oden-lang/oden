#lang racket

(provide polymorphic?)

(define (polymorphic? t)
  (match t
    [`(var ,n) #t]
    ['() #f]
    [`(,x . ,xs) (or (polymorphic? x) (polymorphic? xs))]
    [(? symbol?) #f]
    [`(,t1 -> ,t2) (or (polymorphic? t1) (polymorphic? t2))]
    [`(-> ,t) (polymorphic? t)]))
