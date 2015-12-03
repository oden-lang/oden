#lang racket

(provide expand-monomorphed-let)

(require "monomorphed-binding.rkt")

(define (expand-monomorphed-let m-bindings name body)
  (define (key-matches-name key)
    (equal? (car key) name))
  
  (let loop ([matching-keys (filter key-matches-name (hash-keys m-bindings))]
             [expr body])
    (match matching-keys
      [`(,k . ,ks)
       (match (hash-ref m-bindings k)
         [(monomorphed-binding name m-name expr)
          `((let ([(,m-name : ,(caddr expr)) ,expr]) ,(loop ks body)) : ,(caddr body))])]
      ['() expr])))

(module+ test
  (require rackunit)

  (test-case "expands to only body if no monomorphed matches"
    (check-equal?
     (expand-monomorphed-let (hash) 'name '(body : t))
     '(body : t)))

  (test-case "expands to one let if there's one monomorphization"
    (check-equal?
     (expand-monomorphed-let
      (hash '(name whatever-type) (monomorphed-binding 'name 'm-name '(value : t)))
      'name
      '(body : t))
     '((let ([(m-name : t) (value : t)]) (body : t)) : t)))

  (test-case "expands to multiple lets for multiple monomorphizations"
    (check-match
     (expand-monomorphed-let
      (hash '(name whatever-1) (monomorphed-binding 'name 'm-name-1 '(value : t))
            '(name whatever-2) (monomorphed-binding 'name 'm-name-2 '(value : t)))
      'name
      '(body : t))
     `((let (,_)
         ((let (,_)
            (body : t))
          : t))
       : t)))
  
  )
