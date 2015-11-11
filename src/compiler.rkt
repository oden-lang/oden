#lang racket

(require racket/match)
(require racket/system)
(require "inferencer.rkt")
(require "explode.rkt")
(require "go-backend.rkt")

(define (validate-top-level-forms all)
  (let loop ([forms all] [seen-pkg #f] [seen-define #f])
    (match (list forms seen-pkg seen-define)
      [`(() #f ,_)
       (error "Package declaration missing!")]
      [`(() #t ,_) all]
      [`(((pkg ,_) . ,fs) #f #f)
       (loop fs #t #f)]
      [`(((pkg ,_) . ,_) ,_ ,_)
       (error
	(format "Invalid pkg declaration: ~a'nPackage declaration must be the first top-level form."
		(car forms)))]
      [`(((import ,_) . ,_) #t #t)
       (error
	(format "Invalid import: ~a\nImports must be declared before all definitions."
		(car forms)))]
      [`(((import ,_) . ,fs) #t #f)
       (loop fs #t #f)]
      [`(((import ,_) . ,_) #f ,_)
       (error
	(format "Package declaration missing before import: ~a"
		(car forms)))]
      [`(((define ,name ,expr) . ,_) #f ,_)
       (error
	(format "Invalid definition: ~a\nDefinitions must appear after pkg declaration and imports."
		(car forms)))]
      [`(((define ,name ,expr) . ,fs) #t ,_)
       (loop fs #t #t)])))

(define (validate-definition name te)
  (match (list name te)
    [`(main (,_ : (-> unit))) void]
    [`(main (,_ : ,t))
     (error
      (format "Bad main function type ~a, must be (-> unit)."
	      t))]
    [_ void]))

(define (transform-top-level-forms exprs)  
  (let* ([forms (let loop ([exprs exprs]
			   [pkg-env '()]
			   [forms '()])
		  (match exprs
		    ['() forms]
		    [`((pkg ,(? symbol? name)) . ,fs)
		     (loop fs pkg-env (cons (car exprs) forms))]
		    [`((import ,(? symbol? path)) . ,fs)
		     (loop fs pkg-env (cons (car exprs) forms))]

		    [`((define ,(? symbol? name) ,expr) . ,es)
		     (let* ([te (:? (explode expr) pkg-env)]
			    [t (car (cddr te))])
		       (validate-definition name te)
		       (loop es (cons `(,name : ,t) pkg-env) (cons `(define ,name ,te) forms)))]
		    [f (error (format "Invalid top level form: ~a" f))]))]
	 [r (reverse forms)])
    (validate-top-level-forms r)))

(define (compile-top-level-forms-to-string exprs)
  (string-join (map compile-top-level-form (transform-top-level-forms exprs))))

(provide
 transform-top-level-forms
 compile-top-level-forms-to-string)
