#lang racket

(provide get-substitutions)

(require unstable/hash)

(define (hash-combine a b) b)

(define (get-substitutions p-type type)
  (match `(,p-type ,type)
    [(list (? symbol? s) (? symbol? s)) (hash)]
    [`((var ,p) ,t) (hash `(var ,p) t)]
    [`((-> ,p) (-> ,t)) (get-substitutions p t)]
    [`((,p1 -> ,p2) (,t1 -> ,t2)) (hash-union (get-substitutions p1 t1)
                                              (get-substitutions p2 t2)
                                              #:combine hash-combine)]
    [`((,constructor . ,ps) (,constructor . ,ts))
     (let ([subs (map
                  (lambda (p) (get-substitutions (car p) (cdr p)))
                  (map cons ps ts))])
       (apply hash-union subs #:combine hash-combine))]
    [_ (error
        (format "Cannot get substitutions, ~a not matching ~a"
                p-type
                type))]))

(module+ test
  (require rackunit)

  (test-case "get substitutions for single var"
    (check-equal?
     (get-substitutions '(var a) 'x)
     (hash '(var a) 'x)))

  (test-case "get substitutions for no-arg fn"
    (check-equal?
     (get-substitutions '(-> (var a)) '(-> x))
     (hash '(var a) 'x)))

  (test-case "get substitutions for fn with single tvar"
    (check-equal?
     (get-substitutions '((var a) -> (var a)) '(x -> x))
     (hash '(var a) 'x)))

  (test-case "get substitutions for fn with multiple tvars"
    (check-equal?
     (get-substitutions '((var a) -> (var b)) '(x -> y))
     (hash '(var a) 'x
           '(var b) 'y)))

  (test-case "get substitutions for higher-order fns"
    (check-equal?
     (get-substitutions '((var a) -> (var b)) '((int -> int) -> float))
     (hash '(var a) '(int -> int)
           '(var b) 'float)))

  (test-case "get substitutions for type constructor with multiple tvars"
    (check-equal?
     (get-substitutions '(Pair (var a) (var b)) '(Pair x y))
     (hash '(var a) 'x
           '(var b) 'y)))

  (test-case "get substitutions for nested type constructor with tvar"
    (check-equal?
     (get-substitutions '(Promise (Either (var a) (var b)))
                        '(Promise (Either error int)))
     (hash '(var a) 'error
           '(var b) 'int))))
