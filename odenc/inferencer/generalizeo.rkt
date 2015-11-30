#lang racket

(provide generalizeo)

(require Racket-miniKanren/miniKanren/mk)

(define (generalizeo v)
  (conda
   [(== v `(var ,(gensym)))]
   [(fresh (d r)
           (== v `(,d -> ,r))
           (generalizeo d)
           (generalizeo r))]
   [succeed]))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (test-case "substitutes unbound logic variable for type vars"
    (check-equal?
     (caar (run 1 (q) (generalizeo q)))
     'var))

  (test-case "retains bound logic variables (might be type vars) "
    (check-equal?
     (car (run* (q)
                (== q 'foo)
                (generalizeo q)))
     'foo))
  
  (test-case "substitues fn domain logic varable for type var"
    (check-equal?
     (caar (run 1 (q)
                (generalizeo `(,q -> ,q)))
           )
     'var))
  
  (test-case "generalizeo after unification"
    (check-match
     (run 1 (q)
          (fresh (t)
                 (== q `(,t -> ,t))
                 (generalizeo t)))
     `(((var ,_) -> (var ,_)))))

  (test-case "generalizeo before unification fails"
    (check-equal?
     (run 1 (q)
          (fresh (t)
                 (generalizeo q)
                 (== q `(,t -> ,t))))
     '()))

  (test-case ""
    (check-match
     (run 1 (q)
          (fresh (a b c)
                 (== `(,a -> (,b -> ((,a -> (,b -> ,c)) -> ,c))) q)
                 (generalizeo q)))
     `(((var ,a)
        -> ((var ,b)
            -> (((var ,a) -> ((var ,b) -> (var ,c)))
                -> (var ,c)))))))
  )


