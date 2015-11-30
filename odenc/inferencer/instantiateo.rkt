#lang racket

(provide instantiateo)

(require Racket-miniKanren/miniKanren/mk)
(require "lookup-varo.rkt")

(define (instantiateo s env t)
  (conda
   [(== '(error instantiate-unbound-variable) s) (== s t)]
   [(== `(unbound ,t) s)]
   [(fresh (d dt r rt vt)
           (== `(,d -> ,r) s)

           (instantiateo d `((,r ,rt) . ,env) dt)           
           (instantiateo r `((,d ,dt) . ,env) rt)
           
           (== `(,dt -> ,rt) t))]
   [(fresh (r rt)
           (== `(-> ,r) s)
           (instantiateo r env rt)
           (== `(-> ,rt) t))]

   [(fresh (x xi xs xsi)
           (== `(,x . ,xs) s)
           (=/= x 'var)
           (instantiateo x env xi)
           ;; todo: extend env with ti
           (instantiateo xs env xsi)
           (== `(,xi . ,xsi) t))]
   [(== '() s) (== '() t)]
   
   [(not-varo s) (== s t)]
   [(lookup-varo s env t)]))

(module+ test
  (require rackunit)

  (test-case "substitutes unseen type var for logic variable"
    (check-equal?
     (run 1 (q)
          (instantiateo '(var foo) '() q))
     '(_.0)))
  
  (test-case "substitutes unseen type var for logic variable"
    (check-equal?
     (run 1 (q)
          (instantiateo '(var foo) '() q))
     '(_.0)))

  (test-case "substitutes fn domain type var"
    (check-equal?
     (run* (q)
           (instantiateo '((var foo) -> bar) '() q))
     '((_.0 -> bar))))
  
  (test-case "substitutes fn range type var"
    (check-equal?
     (run* (q)
           (instantiateo '(foo -> (var bar)) '() q))
     '((foo -> _.0))))

  (test-case "substitutes both types in fn"
    (check-equal?
     (run* (q)
           (fresh (f b)
                  (instantiateo '((var foo) -> (var bar)) '() q)))
     '((_.0 -> _.1))))
  
  (test-case "substitutes matching type vars for same logic variable in fn"
    (check-equal?
     (run* (q)
           (instantiateo '((var foo) -> (var foo)) '() q))
     '((_.0 -> _.0))))
  
  (test-case "substitutes type var in no-arg fn"
    (check-equal?
     (run* (q)
           (instantiateo '(-> (var foo)) '() q))
     '((-> _.0))))

  (test-case "retains monotypes"
    (check-equal?
     (run* (q)
           (instantiateo 'foo '() q))
     '(foo)))

  (test-case "retains monotype constructors"
    (check-equal?
     (run* (q)
           (instantiateo '(Foo bar) '() q))
     '((Foo bar))))

  (test-case "substitues type vars in type constructors"
    (check-equal?
     (run* (q)
           (instantiateo '(Foo (var foo) bar) '() q))
     '((Foo _.0 bar))))

  (test-case "extracts wrapped logic variable"
    (check-equal?
     (run* (q)
           (fresh (a b)
                  (instantiateo `(unbound ,a) '() b)
                  (== q `(,a ,b))))
     '((_.0 _.0))))
  
  (test-case "unifies with error for unbound logic var"
    (check-equal?
     (run* (q)
           (fresh (r)
                  (instantiateo q '() r)))
     '((error instantiate-unbound-variable))))

  (test-case "pair fn"
    (check-equal?
     (run* (q)
           (instantiateo `((var a) -> ((var b) -> (((var a) -> ((var b) -> (var c)))
                                                   -> (var c))))
                         '()
                         q))
     '((_.0 -> (_.1 -> ((_.0 -> (_.1 -> _.2)) -> _.2))))))
  )
