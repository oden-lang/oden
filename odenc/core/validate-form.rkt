#lang racket

(require "valid-identifier.rkt")

(provide
 validate-form)

(define (validate-identifier i)
  (when (not (valid-identifier? i))
    (raise-user-error (format "Invalid identifier: ~a" i))))

;; Takes exploded syntax and validates it.
(define (validate-form expr)
  (match expr
    [`(define ,name ,value)
     (validate-identifier name)
     (validate-form value)]
    [(? symbol? s)
     (validate-identifier s)]
    [(? number?)
     (void)]
    [(? string?)
     (void)]
    [`(fn () ,body)
     (validate-form body)]
    [`(fn (,arg) ,body)
     (validate-identifier arg)
     (validate-form body)]
    [`(let ((,s ,v)) ,b)
     (validate-identifier s)
     (validate-form v)
     (validate-form b)]
    [`(if ,c ,a ,b)
     (validate-form c)
     (validate-form a)
     (validate-form b)]
    [`(,e : ,t)
     (validate-form e)]
    [`(,f)
     (validate-form f)]
    [`(,f ,a)
     (validate-form f)
     (validate-form a)]
    [e (void)]))

(module+ test
  (require rackunit)

  (test-case "invalid identifier"
    (check-exn
     exn:fail:user?
     (thunk
      (validate-form ':foo))))

  (test-case "invalid identifier in fn arg"
    (check-exn
     exn:fail:user?
     (thunk
      (validate-form '(fn (:foo) bar)))))

  (test-case "invalid identifier in let binding"
    (check-exn
     exn:fail:user?
     (thunk
      (validate-form '(let ([:foo 123]) bar)))))
  )
