#lang racket

(provide compile-pkg)

(require graph)

(require "inferencer.rkt")
(require "source-pkg.rkt")
(require "compiler/compiled-pkg.rkt")
(require "compiler/explode.rkt")
(require "compiler/get-non-local-references.rkt")
(require "compiler/monomorph.rkt")

(define (get-sorted-names defs)
  (let loop ([defs defs]
             [deps '()])
    (match defs
      ['() (graph-topological-sort (make-graph deps))]
      [`((define ,name ,expr) . ,ds)
       (let* ([non-local (get-non-local-references expr (list name))]
              [with-name (map (lambda (i) (list i name)) non-local)])
         (loop ds (append with-name deps)))])))

(define (explode-defs pkg)
  (map explode-definition (source-pkg-defs pkg)))

(define (defs-to-hash exploded-defs)
  (make-hash (for/list ([def exploded-defs])
                         (match def
                           [`(define ,name ,_) (list name def)]))))

(define (sort-defs exploded-defs def-h)
  (apply
     append
     (for/list ([k (get-sorted-names exploded-defs)])
       (hash-ref def-h k '()))))

(define (validate-definition name te)
  (match (list name te)
    [`(main (,_ : (-> unit))) void]
    [`(main (,_ : ,t))
     (error
      (format "Bad main function type ~a, must be (-> unit)."
	      t))]
    [_ void]))

(define (polymorphic? t)
  (match t
    [`(var ,n) #t]
    ['() #f]
    [`(,x . ,xs) (or (polymorphic? x) (polymorphic? xs))]
    [(? symbol?) #f]
    [`(,t1 -> ,t2) (or (polymorphic? t1) (polymorphic? t2))]
    [`(-> ,t) (polymorphic? t)]))

(define (collect-monomorphed-defs monomorphed)
  (map monomorphed-def-def (hash-values monomorphed)))

(define/contract (compile-pkg pkg)
  (-> source-pkg? compiled-pkg?)
  (let* ([defs (explode-defs pkg)]
         [h (defs-to-hash defs)])
    (let loop ([defs (sort-defs defs h)]
               [pkg-env '()]
               [p-defs (hash)] ;; polymorphic function defs
               [monomorphed (hash)]
               [forms '()])
      (match defs
        ['() (compiled-pkg (car (cdr (source-pkg-decl pkg)))
                           (source-pkg-imports pkg)
                           (reverse forms)
                           (collect-monomorphed-defs monomorphed)
                           pkg-env)]

        [`((define ,(? symbol? name) ,_) . ,ds)
         (let* ([def (car defs)]
                [inferred (infer-def def pkg-env)])
           (match inferred
             [`((define ,name ,expr) : ,t)
              (validate-definition name expr)
              (if (polymorphic? t)
                  ;; polymorphic definition
                  (loop ds
                        (cons `(,name : ,t) pkg-env)
                        (hash-set p-defs name inferred)
                        monomorphed
                        forms)
                  ;; monomorphic definition
                  (match (monomorph p-defs inferred monomorphed)
                    [`(,def ,m)
                     (loop ds
                           (cons `(,name : ,t) pkg-env)
                           p-defs
                           m
                           (cons def forms))]))]))]
        
        [f (error (format "Invalid top level form: ~a" f))]))))

(module+ test
  (require rackunit)
  
  (test-case "transforms hello world"
    (check-equal?
     (compile-pkg
      (source-pkg
       '(pkg main)
       '((import fmt))
       '((define main
           (fn ()
             (fmt.Println "Hello, world!"))))))
     (compiled-pkg
      'main
      '((import fmt))
      '(((define main
            ((fn ()
               (((fmt.Println : (string -> unit))
                 ("Hello, world!" : string)) : unit)) : (-> unit)))
         : (-> unit)))
      `()
      '((main : (-> unit))))))

  (test-case "monomorphs"
    (check-equal?
     (compiled-pkg-defs (compile-pkg
                                (source-pkg
                                 '(pkg monomorphization)
                                 '()
                                 '((define identity (fn (x) x))
                                   (define foo (identity 1))))))
     '(((define foo
           (((identity_inst_int_to_int : (int -> int)) (1 : int)) : int))
        : int))))

  (test-case "monomorphs nested"
    (check-equal?
     (compiled-pkg-defs (compile-pkg
                                (source-pkg
                                 '(pkg monomorphization)
                                 '()
                                 '((define identity (fn (x) x))
                                   (define identity-twice (fn (x) (identity (identity x))))
                                   (define foo (identity-twice 1))))))
     '(((define foo
          (((identity-twice_inst_int_to_int : (int -> int)) (1 : int)) : int))
        : int))))

  (test-case "sort-defs"
    (check-equal?
     (let* ([pkg (source-pkg
                  '(pkg main)
                  '((import something))
                  '((define foo undefined) (define bar (+ foo 1))))]
            [defs (explode-defs pkg)]
            [h (defs-to-hash defs)])
       (sort-defs defs h))
     '((define foo undefined)
       (define bar ((+ foo) 1)))))

  #|
  (test-case "inline polymorphic"
    (check-equal?
     (compile-pkg (source-pkg
                   '(pkg main)
                   '()
                   '((define (identity x) x)
                     (define bar (identity 1)))))
     '((define bar ((fn (x) x) 1)))))
  |#)
