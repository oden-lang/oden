#lang racket

(require "inferencer.rkt")
(require "explode.rkt")
(require "source-pkg.rkt")
(require "compiled-pkg.rkt")

(define (validate-definition name te)
  (match (list name te)
    [`(main (,_ : (-> unit))) void]
    [`(main (,_ : ,t))
     (error
      (format "Bad main function type ~a, must be (-> unit)."
	      t))]
    [_ void]))

(define/contract (compile-pkg pkg)
  (-> source-pkg? compiled-pkg?)
  (let loop ([definitions (source-pkg-definitions pkg)]
	     [pkg-env '()]
	     [forms '()])
    (match definitions
      ['() (compiled-pkg (car (cdr (source-pkg-decl pkg)))
			 (source-pkg-imports pkg)
			 (reverse forms)
			 pkg-env)]
      [`((define ,(? symbol? name) ,expr) . ,es)
       (let* ([te (:? (explode expr) pkg-env)]
	      [t (car (cddr te))])
	 (validate-definition name te)
	 (loop es (cons `(,name : ,t) pkg-env) (cons `(define ,name ,te) forms)))]
      [f (error (format "Invalid top level form: ~a" f))])))

(provide
 compile-pkg)
