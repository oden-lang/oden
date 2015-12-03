#lang racket

(provide (all-defined-out))

(struct
  monomorphed-binding
  (name instance-name expr)
  #:transparent)
