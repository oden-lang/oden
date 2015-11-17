#lang info

(define collection "oden")
(define deps '("base"
               "rackunit-lib"
               "git://github.com:miniKanren/Racket-miniKanren.git"
               "git://github.com:oden-lang/graph.git"))
(define build-deps '("racket-doc"))
(define pkg-desc "The Oden Language")
(define version "0.1.7")
