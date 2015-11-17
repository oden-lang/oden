#lang racket

(require graph)
(require "inferencer.rkt")
(require "explode.rkt")
(require "source-pkg.rkt")
(require "compiled-pkg.rkt")

(define/contract (get-non-local-references expr [local '()])
  (->* (any/c) ((listof symbol?)) (listof symbol?))
  (match expr
    [(? symbol? s)
     (if (member s local)
         '()
         (list s))]
    [(? number?) '()]
    [(? string?) '()]
    [`(,e : ,t)
     (get-non-local-references e local)]
    [`(let ([,name ,expr]) ,body)
     (append (get-non-local-references expr local)
             (if (member name local)
                 (get-non-local-references body local)
                 (get-non-local-references body (cons name local))))]
    [`(fn (,a) ,b)
     (get-non-local-references b (cons a local))]
    [`(fn () ,b)
     (get-non-local-references b local)]
    [`(,f ,a)
     (append (get-non-local-references f local)
             (get-non-local-references a local))]
    [`(if ,c ,a ,b)
     (append (get-non-local-references c local)
             (get-non-local-references a local)
             (get-non-local-references b local))]
    [e (error (format "Cannot get non-local references of expr: ~v" e))]))

(define (get-sorted-names defs)
  (let loop ([defs defs]
             [deps '()])
    (match defs
      ['() (graph-topological-sort (make-graph deps))]
      [`((define ,name ,expr) . ,ds)
       (let* ([non-local (get-non-local-references expr (list name))]
              [with-name (map (lambda (i) (list i name)) non-local)])
         (loop ds (append with-name deps)))])))

(define (explode-and-sort-definitions pkg)
  (let* ([defs (map explode-definition (source-pkg-definitions pkg))]
         [h (make-hash (for/list ([def defs])
                         (match def
                           [`(define ,name ,_) (list name def)])))])
    (apply
     append
     (for/list ([k (get-sorted-names defs)])
       (hash-ref h k '())))))

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
  (let loop ([definitions (explode-and-sort-definitions pkg)]
	     [pkg-env '()]
	     [forms '()])
    (match definitions
      ['() (compiled-pkg (car (cdr (source-pkg-decl pkg)))
			 (source-pkg-imports pkg)
			 (reverse forms)
			 pkg-env)]
      [`((define ,(? symbol? name) ,_) . ,ds)
       (match (infer-def (car definitions) pkg-env)
         [`((define ,name ,expr) : ,t)
          (validate-definition name expr)
          (loop ds (cons `(,name : ,t) pkg-env) (cons `(define ,name ,expr) forms))])]
      [f (error (format "Invalid top level form: ~a" f))])))

(provide
 get-non-local-references
 explode-and-sort-definitions
 compile-pkg)
