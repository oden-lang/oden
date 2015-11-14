#lang racket

(require rackunit "compiler.rkt")
(require rackunit/text-ui)

(require "source-pkg.rkt")
(require "compiled-pkg.rkt")

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
      '((main : (-> unit))))))))
  
(run-tests compiler-tests)
