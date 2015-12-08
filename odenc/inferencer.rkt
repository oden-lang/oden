#lang racket

(provide
 infer
 infer-def
 instantiate-type)

(require Racket-miniKanren/miniKanren/mk)
(require "inferencer/infero.rkt") 
(require "inferencer/infer-defo.rkt")
(require "inferencer/instantiateo.rkt")
(require "inferencer/erroro.rkt") 
 
(define (default-envo t)
  (== t
      `[(unit : unit)
        ;; todo: remove when proper FFI is in place
        (strconv.Itoa : (int -> string))
        (fmt.Println : (string -> unit))
        ]))

(define (format-error e)
  (match e
    [`(error undefined-identifier ,id)
     (format "Undefined identifier: ~a" id)]
    [`(error type-mismatch ,expr ,actual ,expected)
     (format "Type mismatch:\n  Actual: ~a\n  Expected: ~a\n  Expression: ~a\n"
             actual
             expected
             expr)]
    [`(error if-branch-type-mismatch ,expr ,a ,b)
     (format "If branch type mismatch:\n  Then-branch: ~a\n  Else-branch: ~a\n  Expression: ~a\n"
             a
             b
             expr)]
    [`(error ,name . ,args)
     (format "~a error: ~a" name args)]))

(define (extract-typed-expr mk-results)
  (match mk-results
    [`()
     (raise-user-error "Unexpected type inference error!")]
    [`(,(? error? e))
     (raise-user-error (format-error e))]
    [`(,te) te]))

(define (infer expr [custom-env '()])
  (extract-typed-expr
   (run 1 (q)
        (fresh (env d)
               (default-envo d)
               (== env (append custom-env d))
               (infero expr env q)))))

(define (infer-def def [custom-env '()])
  (extract-typed-expr
   (run 1 (q)
        (fresh (env default-env)
               (default-envo default-env)
               (== env (append custom-env default-env))
               (infer-defo def env q)))))

(define (instantiate-type t)
  (match (run 1 (q) (instantiateo t '() q))    
    [`() (raise-user-error (format "Failed to instantiate type: ~a" t))]
    [`(,i . ,is) i]))

(module+ test
  (require rackunit)
  
  (define (only-type te) (car (cddr te)))
  
  (test-case "identity"
    (check-equal?
     (infer '((fn (x) x) true))
     '((((fn ([x : bool]) (x : bool)) : (bool -> bool)) (true : bool)) : bool)))

  (test-case "fn without arguments"
    (check-equal?
     (infer '((fn () true)))
     '((((fn () (true : bool)) : (-> bool))) : bool)))

  (test-case "if"
    (check-equal?
     (infer '(if true 1 0))
     '((if (true : bool) (1 : int) (0 : int)) : int)))


  (test-case "if branches have same type"
    (check-exn
     exn:fail:user?
     (thunk
      (infer '(if true 1 true)))))

  (test-case "complex expressions in if"
    (check-equal?
     (only-type (infer '(if ((== ((+ 10) 10)) 20) ((+ 1) 1) ((+ 2) 2))))
     'int))

  (test-case "type-annotated complex expression"
    (check-equal?
     (only-type (infer '(((+ 1) 2) : int)))
     'int))

  (test-case "polymorphic fn"
    (check-equal?
     (only-type (infer '(fn (x) x)))
     '(_.0 -> _.0)))

  (test-case "partial application of polymorphic fn"
    (check-equal?
     (only-type (infer '((fn (x) (fn (y) x)) 1)))
     '(_.0 -> int)))

  (test-case "polymorphic let"
    (check-equal?
     (caddar
      (run* (q) (infero '(let ([f g]) ((f f) 1))
                        '((g : ((var foo) -> (var foo)))) q)))
     'int))

  (test-case "polymorphic let with fn"
    (check-equal?
     (caddar
      (run* (q) (infero '(let ([f (fn (x) x)]) ((f f) 1)) '() q)))
     'int))

  (test-case "recursive function definition"
    (check-equal?
     (only-type (infer-def '(define inf (fn  ([x : int]) ((+ 1) (inf x))))))
     '(int -> int)))
  
  (test-case "recursive function definition with no type type constraints (will fail in codegen stage) "
    (check-match
     (only-type (infer-def '(define inf (fn (x) (inf x)))))
     `((var ,v1) -> (var ,v2)))))
