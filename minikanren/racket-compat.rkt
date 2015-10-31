#lang racket

(provide remp my-sort datum->string)

(define (remp p ls) (filter (compose not p) ls))

(define (my-sort comp ls)
  (sort ls comp))

(define datum->string ~a)
