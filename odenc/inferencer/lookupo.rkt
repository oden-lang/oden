#lang racket

(provide lookupo)

(require Racket-miniKanren/miniKanren/mk)
(require "arith-operatoro.rkt")
(require "instantiateo.rkt")

(define (lookupo x env t)
  (symbolo x)
  (conda

   ;; try to find x in env, as a bool literal or as an arithmetic operator
   [(conde
     [(== 'false x)
      (== '(false : bool) t)]
     [(== 'true x)
      (== '(true : bool) t)]
     [(arith-operatoro x t)]
     [(fresh (s _ i)
             (== `((,x : ,s) . ,_) env)
             (instantiateo s '() i)
             (== `(,x : ,i) t))]
     [(fresh (y _ env^)
             (== `((,y . ,_) . ,env^) env)
             (=/= x y)
             (symbolo y)
             (lookupo x env^ t))])]
   
   ;; not found in env
   [(== `(error undefined-identifier ,x) t)]))

(module+ test
  (require rackunit)
  
  (test-case "instantiates type vars"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : (var foo))) q))
     '((x : _.0))))

  (test-case "instantiates type vars in fn"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : ((var foo) -> (var bar)))) q))
     '((x : (_.0 -> _.1)))))

  (test-case "instantiates matching type vars to same logic variable"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : ((var foo) -> (var foo)))) q))
     '((x : (_.0 -> _.0)))))
  
  (test-case "instantiates type vars in nested fns"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : (int -> ((var foo) -> (var bar))))) q))
     '((x : (int -> (_.0 -> _.1))))))

  (test-case "retains non-type vars"
    (check-equal?
     (run* (q)
           (lookupo 'x '((x : int)) q))
     '((x : int))))
  
  (test-case "extracts wrapped unbound logic variables"
    (check-equal?
     (run* (q)
           (fresh (t)
                  (lookupo 'x `((x : (unbound ,t))) q)))
     '((x : _.0))))

  (test-case "unifies with undefined-identifier error when not found in env"
    (check-equal?
     (run* (q)
           (lookupo 'x '((y : foo)) q))
     '((error undefined-identifier x))))

  (test-case "unifies with undefined-identifier error when env is empty"
    (check-equal?
     (run* (q)
           (lookupo 'x '() q))
     '((error undefined-identifier x))))
  )
