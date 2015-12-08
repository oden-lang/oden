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
  (fresh (t^)
         (== `(,o : ,t^) t)
         (conde
          [(== '++ o)
           (== `(string -> (string -> string)) t^)]
          [(== '+ o)
           (== `(int -> (int -> int)) t^)]
          [(membero o '(- * /))
           (== `(int -> (int -> int)) t^)]
          [(== '% o )
           (== `(int -> (int -> int)) t^)]
          [(fresh (ot)
                  (membero o '(== !=))
                  (== `(,ot -> (,ot -> bool)) t^))]
          [(== 'not o)
           (== '(bool -> bool) t^)]
          [(membero o '(and or))
           (== '(bool -> (bool -> bool)) t^)]
          [(membero o '(> < >= <=))
           (== `(int -> (int -> bool)) t^)])))

(module+ test
  (require rackunit)

  (test-case "++"
    (check-equal? (run* (q) (arith-operatoro '++ q))
                  '((++ : (string -> (string -> string))))))

  (test-case "+"
    (check-equal? (run* (q) (arith-operatoro '+ q))
                  '((+ : (int -> (int -> int))))))

  (test-case "-"
    (check-equal? (run* (q) (arith-operatoro '- q))
                  '((- : (int -> (int -> int))))))

  (test-case "%"
    (check-equal? (run* (q) (arith-operatoro '% q))
                  '((% : (int -> (int -> int))))))

  (test-case "=="
    (check-equal? (run* (q) (arith-operatoro '== q))
                  '((== : (_.0 -> (_.0 -> bool))))))

  (test-case "<"
    (check-equal? (run* (q) (arith-operatoro '< q))
                  '((< : (int -> (int -> bool))))))

  (test-case "and"
    (check-equal? (run* (q) (arith-operatoro 'and q))
                  '((and : (bool -> (bool -> bool))))))

  (test-case "not"
    (check-equal? (run* (q) (arith-operatoro 'not q))
                  '((not : (bool -> bool))))))
