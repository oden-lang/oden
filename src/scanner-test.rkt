#lang racket

(require rackunit "scanner.rkt")
(require rackunit/text-ui)

(require "source-pkg.rkt")

(define scanner-tests
  (test-suite
   "scanner"
   (test-case
    "file must not be empty"
    (check-exn
     exn:fail?
     (lambda ()
       (read-kashmir-pkg '()))))
   
   (test-case
    "only pkg declaration is required"
    (check-equal?
     (with-input-from-string
	 "(pkg foo)"
       read-kashmir-pkg)
     (source-pkg
      '(pkg foo)
      '()
      '())))

   (test-case
    "pkg must be first top-level form"
    (check-exn
     exn:fail?
     (lambda ()
       (with-input-from-string
	 "(import fmt) (pkg main)"
	 read-kashmir-pkg))))

   (test-case
    "imports must appear before definitions"
    (check-exn
     exn:fail?
     (lambda ()
       (with-input-from-string
	 "(pkg lib) (define test 123) (import fmt)"
	 read-kashmir-pkg))))))

(run-tests scanner-tests)
