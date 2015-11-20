#lang racket

(provide lookupo)

(require Racket-miniKanren/miniKanren/mk)
(require "arith-operatoro.rkt")
(require "instantiateo.rkt")

(define (lookupo x env t)
  (symbolo x)
  (conde
   [(arith-operatoro x t)]
   ((fresh (s _)
           (== `((,x : ,s) . ,_) env)
           (instantiateo s '() t)
           ))
   ((fresh (y _ env^)
           (== `((,y . ,_) . ,env^) env)
           (=/= x y)
           (symbolo y)
           (lookupo x env^ t)))))

(module+ test
  (require rackunit)
  
  (test-case "instantiates type vars"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : (var foo))) q))
     '(_.0)))

  (test-case "instantiates type vars in fn"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : ((var foo) -> (var bar)))) q))
     '((_.0 -> _.1))))

  (test-case "instantiates matching type vars to same logic variable"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : ((var foo) -> (var foo)))) q))
     '((_.0 -> _.0))))
  
  (test-case "instantiates type vars in nested fns"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : (int -> ((var foo) -> (var bar))))) q))
     '((int -> (_.0 -> _.1)))))

  (test-case "retains non-type vars"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : int)) q))
     '(int)))
  
  (test-case "extracts wrapped unbound logic variables"
    (check-equal?
     (run* (q)
           (lookupo 'x `((x : (unbound ,q))) q))
     '(_.0)))
  )
