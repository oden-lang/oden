#lang racket

(struct compiled-pkg (name imports defs monomorphed-defs exports) #:transparent)

(provide (all-defined-out))
