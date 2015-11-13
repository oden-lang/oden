#lang racket

(require "inferencer.rkt")
(require "explode.rkt")
(require "go-backend.rkt")
(require "pkg-source.rkt")

(define (validate-definition name te)
  (match (list name te)
    [`(main (,_ : (-> unit))) void]
    [`(main (,_ : ,t))
     (error
      (format "Bad main function type ~a, must be (-> unit)."
	      t))]
    [_ void]))

(define (compile-pkg source)  
  (let loop ([definitions (pkg-source-definitions source)]
	     [pkg-env '()]
	     [forms (append 
		     (pkg-source-imports source)
		     (list (pkg-source-pkg-decl source)))])
    (match definitions
      ['() `(,(reverse forms) ,pkg-env)]
      [`((define ,(? symbol? name) ,expr) . ,es)
       (let* ([te (:? (explode expr) pkg-env)]
	      [t (car (cddr te))])
	 (validate-definition name te)
	 (loop es (cons `(,name : ,t) pkg-env) (cons `(define ,name ,te) forms)))]
      [f (error (format "Invalid top level form: ~a" f))])))

(define (compile-pkg-forms-to-string forms)
  (string-join (map compile-top-level-form forms)))

(provide
 compile-pkg
 compile-pkg-forms-to-string)
