#lang racket

(define (translate-identifier id)
  (let* ([parts (string-split (symbol->string id) "-" #:trim? #t)]
	 [titles (cons (car parts) (map string-titlecase (cdr parts)))])
    (string-join titles "")))

(define (compile-type type)
  (match type
    ['unit ""]
    [`(,r -> ,d)
     (format "func (~a) (~a)"
	     (compile-type r)
	     (compile-type d))]
    [`(-> ,d)
     (format "func () (~a)"
	     (compile-type d))]
    [(? list? l) (string-join (map ~a l) "_")]
    [(? symbol? t)
     (match (~a t)
       [(regexp #rx"_\\.[0-9]+")
	(error (string-trim "
Cannot currently compile values with type variables (e.g. polymorphic 
functions). You might have to annotate some parts of your code to resolve
this. In future versions of Kashmir this should be possible."))]
       [s s])]))

(define (infix-operator? op)
  (member op '(+ - * / == !=)))

(define (compile-return typed-expr)
  (match typed-expr
    [`((,f) : unit)
     (format "~a\nreturn\n" (compile-expr typed-expr))]
    [`((,f ,a) : unit)
     (format "~a\nreturn\n" (compile-expr typed-expr))]
    [`(,e : unit) "return\n"]
    [te (format "return ~a\n"
		(compile-expr te))]))

(define (compile-expr typed-expr)
  (match typed-expr
    [(? number? x) (~a x)]
    [`(,e : ,t) (compile-expr e)]
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
             (compile-expr a)
             op
             (compile-expr b))]
    [`(lambda () (,e : ,et))
     (format "(func () ~a {\n~a})"
	     (compile-type et)
	     (compile-return `(,e : ,et)))]
    [`(lambda ((,x : ,xt)) (,e : ,et))
     (format "(func (~a ~a) ~a {\n~a})"
             (compile-expr x)
             (compile-type xt)
             (compile-type et)
             (compile-return `(,e : ,et)))]
    [`(let (([,x : ,xt] (,e : ,et))) (,b : ,bt))
     (format "(func () ~a {\nvar ~a ~a = ~a\n~a}())"
             (compile-type bt)
             (compile-expr x)
             (compile-type xt)
             (compile-expr e)
	     (compile-return `(,b : ,bt)))]
    [`((,f : ,_))
     (format "~a()"
	     (compile-expr f))]
    [`((,f : ,ft) (,a : ,at))
     (format "~a(~a)"
             (compile-expr f)
             (compile-expr a))]
    [`(if ,c `(,a : ,t) `(,b : ,t))
     (format "(func() ~a {\nif ~a {\n~a} else {\n~a}\n})()\n"
             (compile-type t)
             (compile-expr c)
             (compile-return `(,a : ,t))
             (compile-return `(,b : ,t)))]))

(define (compile-top-level-form top-level-form)
  (match top-level-form
    [`(pkg ,name)
     (format "package ~a\n\n" (~a name))]
    [`(import ,path)
     (format "import \"~a\"\n" path)]
    [`(define ,name ((lambda ,arg (,e : ,et)) : ,_))
     (let ([compiled-param (match arg
			     [`() "()"]
			     [`((,x : ,xt)) (format "(~a ~a)"
						    (compile-expr x)
						    (compile-type xt))]
			     [_ (error ("Invalid argument list: ~a" arg))])])
       (format "func ~a ~a ~a {\n~a}\n"
	       (translate-identifier name)		 
	       compiled-param
	       (compile-type et)
	       (compile-return `(,e : ,et))))]      
    [`(define ,name (,e : ,t))
     (format "var ~a ~a = ~a\n"
	     (translate-identifier name)
	     (compile-type t)
	     (compile-expr e))]))

(provide
 compile-expr
 compile-top-level-form)
