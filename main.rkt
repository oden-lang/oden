#lang racket

(require "compiler.rkt")

(go-run (kashmir-compile-prg '(((lambda (x)
                                  (lambda (y) x))
                                1)
                               2)))
