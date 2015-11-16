#lang racket

(require "compiled-pkg.rkt")

(define (translate-identifier id)
  (let* ([parts (string-split (symbol->string id) "-" #:trim? #t)]
	 [titles (cons (car parts) (map string-titlecase (cdr parts)))])
    (string-join titles "")))

(define (codegen-type type)
  (match type
    ['unit ""]
    [`(,r -> ,d)
     (format "func (~a) (~a)"
	     (codegen-type r)
	     (codegen-type d))]
    [`(-> ,d)
     (format "func () (~a)"
	     (codegen-type d))]
    [(? list? l) (string-join (map ~a l) "_")]
    [(? symbol? t)
     (match (~a t)
       [(regexp #rx"_\\.[0-9]+")
	(error (string-trim "
Cannot currently codegen values with type variables (e.g. polymorphic 
functions). You might have to annotate some parts of your code to resolve
this. In future versions of Oden this should be possible."))]
       [s s])]))

(define (infix-operator? op)
  (member op '(+ - * / == != < > <= >=)))

(define (codegen-return typed-expr)
  (match typed-expr
    [`((,f) : unit)
     (format "~a\nreturn\n" (codegen-expr typed-expr))]
    [`((,f ,a) : unit)
     (format "~a\nreturn\n" (codegen-expr typed-expr))]
    [`(,e : unit) "return\n"]
    [te (format "return ~a\n"
		(codegen-expr te))]))

(define (codegen-expr typed-expr)
  (match typed-expr
    [(? number? x) (~a x)]
    [`(,e : ,t) (codegen-expr e)]
    ['false "false"]
    ['true "true"]
    ['unit ""]
    [(? symbol? s) (translate-identifier s)]
    [(? string? s) (~v s)]
    [(list
      (list
       (list
        (list (? infix-operator? op) ': _)
        (list a ': _))
       ':
       (list _ '-> _))
      (list b ': _))
     (format "(~a ~a ~a)"
             (codegen-expr a)
             op
             (codegen-expr b))]
    [`(fn () (,e : ,et))
     (format "(func () ~a {\n~a})"
	     (codegen-type et)
	     (codegen-return `(,e : ,et)))]
    [`(fn ((,x : ,xt)) (,e : ,et))
     (format "(func (~a ~a) ~a {\n~a})"
             (codegen-expr x)
             (codegen-type xt)
             (codegen-type et)
             (codegen-return `(,e : ,et)))]
    [`(let (([,x : ,xt] (,e : ,et))) (,b : ,bt))
     (format "(func () ~a {\nvar ~a ~a = ~a\n~a}())"
             (codegen-type bt)
             (codegen-expr x)
             (codegen-type xt)
             (codegen-expr e)
	     (codegen-return `(,b : ,bt)))]
    [`(if ,c (,a : ,t) (,b : ,t))
     (format "(func() ~a {\nif ~a {\n~a} else {\n~a}\n})()\n"
             (codegen-type t)
             (codegen-expr c)
             (codegen-return `(,a : ,t))
             (codegen-return `(,b : ,t)))]
    [`((,f : ,_))
     (format "~a()"
	     (codegen-expr f))]
    [`((,f : ,ft) (,a : ,at))
     (format "~a(~a)"
             (codegen-expr f)
             (codegen-expr a))]
    [e (error
        (format "Go codegen failed for: ~v" e))]))

(define/contract (codegen-imports pkg)
  (-> compiled-pkg? string?)
  (string-join
   (for/list ([import (compiled-pkg-imports pkg)])
     (format "import \"~a\"\n" (car (cdr import))))))

(define/contract (codegen-definitions pkg)
  (-> compiled-pkg? string?)
  (string-join
   (for/list ([definition (compiled-pkg-definitions pkg)])
     (match definition       
       [`(define ,name ((fn ,arg (,e : ,et)) : ,_))
	(let ([codegend-param (match arg
				[`() "()"]
				[`((,x : ,xt)) (format "(~a ~a)"
						       (codegen-expr x)
						       (codegen-type xt))]
				[_ (error ("Invalid argument list: ~a" arg))])])
	  (format "func ~a ~a ~a {\n~a}\n"
		  (translate-identifier name)		 
		  codegend-param
		  (codegen-type et)
		  (codegen-return `(,e : ,et))))]      
       [`(define ,name (,e : ,t))
	(format "var ~a ~a = ~a\n"
		(translate-identifier name)
		(codegen-type t)
		(codegen-expr e))]))))

(define (codegen-pkg-name pkg)
  (let* ([name (compiled-pkg-name pkg)]
	 [split (string-split (symbol->string name) "/")])
    (last split)))

(define (codegen-pkg pkg)
  (format "package ~a\n\n// imports\n~a\n// definitions\n~a"
	  (codegen-pkg-name pkg)
	  (codegen-imports pkg)
	  (codegen-definitions pkg)))

(provide
 codegen-expr
 codegen-imports
 codegen-definitions
 codegen-pkg)
