#lang racket

(require rackunit "compiler.rkt")
(require rackunit/text-ui)

(require "source-pkg.rkt")
(require "compiled-pkg.rkt")
(require "explode.rkt")

(define compiler-tests
  (test-suite
   "compiler"

   (test-case
    "transforms hello world"
    (check-equal?
     (compile-pkg
      (source-pkg
       '(pkg main)
       '((import fmt))
       '((define main
	    (lambda ()
	      (fmt.Println "Hello, world!"))))))
     (compiled-pkg
      'main
      '((import fmt))
      '((define main
	   ((lambda ()
	      (((fmt.Println : (string -> unit))
		("Hello, world!" : string)) : unit))
	    :
	    (-> unit))))
      '((main : (-> unit))))))

   (test-case
    "get-non-local-references function"
    (check-equal?
     (get-non-local-references
      (explode '(lambda (x y) (f x y))))
     '(f)))

   (test-case
    "get-non-local-references function"
    (check-equal?
     (get-non-local-references
      (explode '(let ([x y]) x)))
     '(y)))

   (test-case
    "sort-definitions"
    (check-equal?
     (explode-and-sort-definitions
      (source-pkg
       '(pkg main)
       '((import something))
       '((define foo undefined) (define bar (+ foo 1)))))
     '((define foo undefined)
       (define bar ((+ foo) 1)))))))

  
(run-tests compiler-tests)
