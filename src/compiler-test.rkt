#lang racket

(require rackunit "compiler.rkt")
(require rackunit/text-ui)



(define compiler-tests
  (test-suite
   "compiler"

   (test-case
    "transforms hello world"
    (check-equal?
     (transform-top-level-forms
      '((pkg main)
	(import fmt)
	(define main
	  (lambda ()
	    (fmt.Println "Hello, world!")))))
     ;; only type-annotations added in this case
     '((pkg main)
       (import fmt)
       (define main
	 ((lambda ()
	    (((fmt.Println : (string -> unit))
	      ("Hello, world!" : string)) : unit))
	  :
	  (-> unit))))))

   (test-case
    "file must not be empty"
    (check-exn
     exn:fail?
     (lambda ()
       (transform-top-level-forms '()))))
   
   (test-case
    "pkg must be declared"
    (check-exn
     exn:fail?
     (lambda ()
       (transform-top-level-forms
	'((import fmt))))))

   (test-case
    "pkg must be first top-level form"
    (check-exn
     exn:fail?
     (lambda ()
       (transform-top-level-forms
	'((import fmt)
	  (pkg main))))))

   (test-case
    "imports must appear before definitions"
    (check-exn
     exn:fail?
     (lambda ()
       (transform-top-level-forms
	'((pkg lib)
	  (define test "hello")
	  (import fmt))))))

   (test-case
    "only pkg declaration is required"
    (check-equal?
     (transform-top-level-forms '((pkg main)))
     '((pkg main))))))
  
(run-tests compiler-tests)
