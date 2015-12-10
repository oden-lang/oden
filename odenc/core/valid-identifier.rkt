#lang racket

(provide
 valid-identifier?)

(define (valid-identifier? s)
  (and (symbol? s) (regexp-match-exact? #rx"[^:].*" (symbol->string s))))
