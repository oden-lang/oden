#lang racket

(require rackunit "types.rkt")

(check-equal?
 (get-type (e-app (e-app (e-var "pair") (e-var "1")) (e-var "2")))
 (pair number number)
 "product type")
