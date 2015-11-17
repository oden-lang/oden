#lang racket

(struct compiled-pkg (name imports definitions exports) #:transparent)

(provide (all-defined-out))
