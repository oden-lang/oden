#lang racket

(struct pkg-source (pkg-decl imports definitions))

(define (pkg-decl-expr? expr)
  (match expr
    [`(pkg ,_) #t]
    [_ #f]))

(define (import-expr? expr)
  (match expr
    [`(import ,_) #t]
    [_ #f]))

(define (definition-expr? expr)
  (match expr
    [`(define ,_ ,_) #t]
    [_ #f]))

(provide (all-defined-out))
