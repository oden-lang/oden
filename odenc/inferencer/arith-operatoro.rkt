#lang racket

(provide arith-operatoro)

(require Racket-miniKanren/miniKanren/mk)

(define (appendo l s out)
  (conde
   ((== '() l) (== s out))
   ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res)))))

(define (membero x y)
  (fresh [a b]
         (appendo a `(,x . ,b) y)))

;; TODO: need some better way to do polymorphic operators
(define (arith-operatoro o t)
  (fresh (ot)
         (conde
          [(== '+ o)
           (membero ot '(int float string))
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(membero o '(- * /))
           (membero ot '(int float))
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(== '% o )
           (== ot 'int)
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(membero o '(== !=))
           (membero ot '(int float string bool))
           (== `(,ot -> (,ot -> bool)) t)]
          [(== 'not o)
           (== '(bool -> bool) t)]
          [(membero o '(and or))
           (== ot 'bool)
           (== '(bool -> (bool -> bool)) t)]
          [(membero o '(> < >= <=))
           (membero ot '(int float))
           (== `(,ot -> (,ot -> bool)) t)])))

(module+ test
  (require rackunit)

  (test-case "+"
    (check-equal? (list->set (run* (q) (arith-operatoro '+ q)))
                  (list->set '((int -> (int -> int))
                                (float -> (float -> float))
                                (string -> (string -> string))))))

  (test-case "-"
    (check-equal? (list->set (run* (q) (arith-operatoro '- q)))
                  (list->set '((int -> (int -> int))
                                (float -> (float -> float))))))

  (test-case "%"
    (check-equal? (run* (q) (arith-operatoro '% q))
                  '((int -> (int -> int)))))

  (test-case "=="
    (check-equal? (list->set (run* (q) (arith-operatoro '== q)))
                  (list->set '((int -> (int -> bool))
                                (float -> (float -> bool))
                                (string -> (string -> bool))
                                (bool -> (bool -> bool))))))

  (test-case "<"
    (check-equal? (list->set (run* (q) (arith-operatoro '< q)))
                  (list->set '((int -> (int -> bool))
                               (float -> (float -> bool))))))

  (test-case "and"
    (check-equal? (run* (q) (arith-operatoro 'and q))
                  '((bool -> (bool -> bool)))))

  (test-case "not"
    (check-equal? (run* (q) (arith-operatoro 'not q))
                  '((bool -> bool)))))
