#lang racket

(require "racket-compat.rkt")

;; all defined for now, but should avoid exporting
;; private functions later
(provide (all-defined-out))

(define a->s (lambda (a) (car a)))
(define a->c* (lambda (a) (cadr a)))
(define a->t (lambda (a) (caddr a)))

(define-syntax lambdag@
  (syntax-rules (:)
    ((_ (a) e) (lambda (a) e))
    ((_ (a : s c* t) e)
     (lambda (a)
       (let ((s (a->s a)) (c* (a->c* a)) (t (a->t a)))
         e)))))

(define mzero (lambda () #f))
(define unit (lambdag@ (a) a))
(define choice (lambda (a f) (cons a f)))
(define-syntax lambdaf@ 
  (syntax-rules () ((_ () e) (lambda () e))))

(define-syntax inc
  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define empty-f (lambdaf@ () (mzero)))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((not a-inf) e0)
         ((procedure? a-inf)  (let ((f^ a-inf)) e1))
         ((not (and (pair? a-inf)
                 (procedure? (cdr a-inf))))
          (let ((a^ a-inf)) e2))
         (else (let ((a (car a-inf)) (f (cdr a-inf))) 
                 e3)))))))
(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (take n f))
         ((a) (cons a '()))
         ((a f) (cons a (take (and n (- n 1)) f))))))))

(define empty-a '(() () ()))
  
(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ...
            (lambdag@ (final-a)
              (choice ((reify x) final-a) empty-f)))
          empty-a))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (a)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 a) g ...)))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((a) (g a))
      ((a f) (mplus (g a) (lambdaf@ () (bind (f) g)))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplus (f) f^)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (a) 
       (inc 
         (mplus* 
           (bind* (g0 a) g ...)
           (bind* (g1 a) g^ ...) ...))))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0 
                    (lambdaf@ () (mplus* e ...))))))

(define pr-t->tag
  (lambda (pr-t)
    (car (rhs pr-t))))

(define pr-t->pred
  (lambda (pr-t)
    (cdr (rhs pr-t))))

(define noo
  (lambda (tag u)
    (let ((pred (lambda (x) (not (eq? x tag)))))
      (lambdag@ (a : s c* t)
        (noo-aux tag u pred a s c* t)))))

(define noo-aux
  (lambda (tag u pred a s c* t)
    (let ((u (if (var? u) (walk u s) u)))
      (cond
        ((pair? u)
         (cond
           ((pred u)
            (let ((a (noo-aux tag (car u) pred a s c* t)))
              (and a
                ((lambdag@ (a : s c* t)
                   (noo-aux tag (cdr u) pred a s c* t))
                 a))))
           (else (mzero))))
        ((not (var? u))
         (cond
           ((pred u) (unit a))
           (else (mzero))))
        ((ext-t u tag pred s t) =>
         (lambda (t0)
           (cond
             ((not (eq? t0 t))
              (let ((t^ (list (car t0))))
                (let ((c* (subsume t^ c*)))
                  (unit (subsume-t s c* t0)))))
             (else (unit a)))))
        (else (mzero))))))

(define make-flat-tag
  (lambda (tag pred)
    (lambda (u)
      (lambdag@ (a : s c* t)
        (let ((u (if (var? u) (walk u s) u)))
          (cond
            ((not (var? u))
             (cond
               ((pred u) (unit a))
               (else (mzero))))
            ((ext-t u tag pred s t) =>
             (lambda (t0)
               (cond
                 ((not (eq? t0 t))
                  (let ((t^ (list (car t0))))
                    (let ((c* (subsume t^ c*)))
                      (unit (subsume-t s c* t0)))))
                 (else (unit a)))))
            (else (mzero))))))))

(define deep-tag?
  (lambda (tag)
    (not (or (eq? tag 'sym) (eq? tag 'num)))))

;;; We can extend t with a deep tag provided
;;; It is not in a singleton c of c* with the same
;;; tag.  That would mean lifting an innocent
;;; constraint to an overarching constraint,
;;; would be wrong.  So, no change to c* or t.
;;; in that case.

(define ext-t
  (lambda (x tag pred s t^)
    (let ((x (walk x s)))
      (let loop ((t t^))
        (cond
          ((null? t) (cons `(,x . (,tag . ,pred)) t^))
          ((not (eq? (walk (lhs (car t)) s) x)) (loop (cdr t)))
          ((eq? (pr-t->tag  (car t)) tag) t^)
          ((works-together? (pr-t->tag (car t)) tag)
           (loop (cdr t)))
          (else #f))))))

(define works-together?
  (lambda (t1 t2)
    (or (deep-tag? t1) (deep-tag? t2))))

(define subsume-t
  (lambda (s c* t)
    (let loop
      ((x* (rem-dups (map lhs t)))
       (c* c*)
       (t t))
      (cond
        ((null? x*) `(,s ,c* ,t))
        (else
         (let ((c*/t (subsume-c*/t (car x*) s c* t)))
           (loop (cdr x*) (car c*/t) (cdr c*/t))))))))

(define rem-dups
  (lambda (vars)
    (cond
      ((null? vars) '())
      ((memq (car vars) (cdr vars))
       (rem-dups (cdr vars)))
      (else (cons (car vars) (rem-dups (cdr vars)))))))

(define have-flat-tag?
  (lambda (pred x)
    (lambda (pr-t)
      (let ((tag (pr-t->tag pr-t)))
        (and
         (not (deep-tag? tag))
         (eq? (lhs pr-t) x)
         (pred tag))))))

(define subsume-c*/t
  (lambda (x s c* t)
    (cond
      ((findf (have-flat-tag? (lambda (u) (eq? u 'sym)) x) t)
       (subsumed-from-t-to-c* x s c* t '()))
      ((findf (have-flat-tag? (lambda (u) (not (eq? u 'sym))) x) t)
       `(,c* . ,(drop-from-t x t)))
      (else `(,c* . ,t)))))

(define drop-from-t
  (lambda (x t)
    (remp (lambda (pr-t)
            (and
              (eq? (lhs pr-t) x)
              (deep-tag? (pr-t->tag pr-t))))
      t)))

(define subsumed-from-t-to-c*
  (lambda (x s c* t t^)
    (cond
      ((null? t) `(,c* . ,t^))
      (else
       (let ((pr-t (car t)))
         (let ((tag (pr-t->tag pr-t))
               (y (lhs pr-t)))
           (cond
             ((and (eq? y x) (deep-tag? tag))
              (subsumed-from-t-to-c* x s
                (new-c* x tag c* s)
                (cdr t)
                t^))
             (else
              (subsumed-from-t-to-c* x s
                c*
                (cdr t)
                (cons (car t) t^))))))))))

(define new-c*
  (lambda (x tag c* s)
    (cond
      ((findf
         (lambda (c)
           (and (null? (cdr c))
             (eq? (walk (lhs (car c)) s) x)
             (eq? (rhs (car c)) tag)))
         c*)
       c*)
      (else (cons `((,x . ,tag)) c*)))))

;;; End reading here.

(define subsume
  (lambda (t c*)
    (remp (lambda (c)
            (findf (subsumed-pr? t) c))
      c*)))
 
(define subsumed-pr?
  (lambda (t)
    (lambda (pr-c)
      (let ((u (rhs pr-c)))
        (and (not (var? u))
          (let ((x (lhs pr-c)))
            (let ((pr-t (assq x t)))
              (and pr-t
                (let ((tag (pr-t->tag pr-t)))
                  (cond
                    ((and (deep-tag? tag) (eq? tag u)))
                    ((not ((pr-t->pred pr-t) u)))
                    (else #f)))))))))))

(define booleano
  (lambda (x)
    (conde
      ((== #f x))
      ((== #t x)))))

(define symbolo (make-flat-tag 'sym symbol?))

(define numbero (make-flat-tag 'num number?))

(define =/=
  (lambda (u v)
    (lambdag@ (a : s c* t)
      (cond
        ((unify u v s) =>
         (lambda (s0)
           (cond
             ((eq? s0 s) (mzero))
             (else
              (let ((p* (list (prefix-s s0 s))))
                (let ((p* (subsume t p*)))
                  (let ((c* (append p* c*)))
                    (unit `(,s ,c* ,t)))))))))
        (else (unit a))))))

(define prefix-s
  (lambda (s0 s)
    (cond
      ((eq? s0 s) '())
      (else (cons (car s0)
              (prefix-s (cdr s0) s))))))

(define ==
  (lambda (u v)
    (lambdag@ (a : s c* t)
      (cond
        ((unify u v s) =>
         (lambda (s0)
           (cond
             ((eq? s0 s) (unit a))
             ((verify-c* c* s0) =>
              (lambda (c*)
                (cond
                  ((verify-t t s0) =>
                   (lambda (t)
                     (let ((c* (subsume t c*)))
                       (unit (subsume-t s0 c* t)))))
                  (else (mzero)))))
             (else (mzero)))))
        (else (mzero))))))
 
(define verify-c*
  (lambda (c* s)
    (cond
      ((null? c*) '())
      ((verify-c* (cdr c*) s) =>
       (lambda (c0*)
         (let ((c (car c*)))
           (cond
             ((verify-c*-aux c c0* s))
             (else (mzero))))))
      (else #f))))

(define verify-c*-aux
  (lambda (c c0* s)
    (cond
      ((unify* c s) =>
       (lambda (s0)
         (and (not (eq? s0 s))
           (cons (prefix-s s0 s) c0*))))
      (else c0*))))

(define verify-t
  (lambda (t s)
    (cond
      ((null? t) '())
      ((verify-t (cdr t) s) =>
       (letrec ((rec
         (lambda (u)
           (let ((u (if (var? u) (walk u s) u)))
             (let ((tag (pr-t->tag (car t)))
                   (pred (pr-t->pred (car t))))
               (lambda (t0)
                 (cond
                   ((var? u)
                    (ext-t u tag pred s t0))
                   ((pair? u)
                    (if (deep-tag? tag)
                      (cond
                        (((rec (car u)) t0) =>
                         (rec (cdr u)))
                        (else #f))
                      (and (pred u) t0)))
                   (else (and (pred u) t0)))))))))
         (rec (lhs (car t)))))
      (else #f))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define var (lambda (dummy) (vector dummy)))
(define var? (lambda (x) (vector? x)))
(define lhs (lambda (pr) (car pr)))
(define rhs (lambda (pr) (cdr pr)))

(define walk
  (lambda (x s)
    (let ((a (assq x s)))
      (cond
        (a (let ((u (rhs a)))
             (if (var? u) (walk u s) u)))
        (else x)))))

(define walk*
  (lambda (v s)
    (let ((v (if (var? v) (walk v s) v)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons (walk* (car v) s) (walk* (cdr v) s)))
        (else v)))))

(define unify
  (lambda (u v s)
    (let ((u (if (var? u) (walk u s) u))
          (v (if (var? v) (walk v s) v)))
      (cond
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s
             (unify (cdr u) (cdr v) s))))
        (else (unify-nonpair u v s))))))

(define unify-nonpair
  (lambda (u v s)
    (cond
      ((eq? u v) s)      
      ((var? u)
       (and (or (not (pair? v)) (valid? u v s))
         (cons `(,u . ,v) s)))
      ((var? v)
       (and (or (not (pair? u)) (valid? v u s))
         (cons `(,v . ,u) s)))
      ((equal? u v) s)
      (else #f))))

(define valid?
  (lambda (x v s)
    (not (occurs-check x v s))))

(define occurs-check
  (lambda (x v s)
    (let ((v (if (var? v) (walk v s) v)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) 
         (or (occurs-check x (car v) s)
             (occurs-check x (cdr v) s)))
        (else #f)))))  

(define reify-s
  (lambda (v)
    (let reify-s ((v v) (r '()))
      (let ((v (if (var? v) (walk v r) v)))
        (cond
          ((var? v)
           (let ((n (length r)))
             (let ((name (reify-name n)))
               (cons `(,v . ,name) r))))
          ((pair? v)
           (let ((r (reify-s (car v) r)))
             (reify-s (cdr v) r)))
          (else r))))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (x)
    (lambdag@ (a : s c* t)
      (let ((v (walk* x s)))
        (let ((r (reify-s v)))
          (reify-aux r v
            (let ((c* (remp
                        (lambda (c)
                          (anyvar? c r))
                        c*)))
              (rem-subsumed c*))        
            (remp
              (lambda (pr)
                (var? (walk (lhs pr) r)))
              t)))))))

(define reify-aux
  (lambda (r v c* t)
    (let ((v (walk* v r))
          (c* (walk* c* r))
          (t (walk* t r)))
      (let ((c* (sorter (map sorter c*)))
            (p* (sorter
                  (map sort-t-vars
                    (partition* t)))))
        (cond
          ((and (null? c*) (null? p*)) v)
          ((null? c*) `(,v . ,p*))
          (else `(,v (=/= . ,c*) . ,p*)))))))

(define sorter
  (lambda (ls)
    (my-sort lex<=? ls)))
 
(define sort-t-vars
  (lambda (pr-t)
    (let ((tag (car pr-t))
          (x* (sorter (cdr pr-t))))
      (let ((reified-tag (tag->reified-tag tag)))
        `(,reified-tag . ,x*)))))

(define tag->reified-tag
  (lambda (tag)
    (if (deep-tag? tag)
      (string->symbol
        (string-append "no-"
          (symbol->string tag)))
      tag)))

(define lex<=?
  (lambda (x y)
    (string<=? (datum->string x) (datum->string y))))
  
(define anyvar?
  (lambda (c r)
    (cond
      ((pair? c)
       (or (anyvar? (car c) r)
           (anyvar? (cdr c) r)))
      (else (and (var? c) (var? (walk c r)))))))

(define rem-subsumed
  (lambda (c*)
    (let rem-subsumed ((c* c*) (c^* '()))
      (cond
        ((null? c*) c^*)
        ((or (subsumed? (car c*) (cdr c*))
             (subsumed? (car c*) c^*))
         (rem-subsumed (cdr c*) c^*))
        (else (rem-subsumed (cdr c*)
                (cons (car c*) c^*)))))))

(define unify*
  (lambda (c s)
    (unify (map lhs c) (map rhs c) s)))
 
(define subsumed?
  (lambda (c c*)
    (cond
      ((null? c*) #f)
      (else
        (let ((c^ (unify* (car c*) c)))
          (or
            (and c^ (eq? c^ c))
            (subsumed? c (cdr c*))))))))

(define part
  (lambda (tag t x* y*)
    (cond
     ((null? t)
      (cons `(,tag . ,x*) (partition* y*)))
     ((eq? (pr-t->tag (car t)) tag)
      (let ((x (lhs (car t))))
        (let ((x* (cond
                    ((memq x x*) x*)
                    (else (cons x x*)))))
          (part tag (cdr t) x* y*))))
     (else
      (let ((y* (cons (car t) y*)))
        (part tag (cdr t) x* y*))))))

(define partition*
  (lambda (t)
    (cond
      ((null? t) '())
      (else
       (part (pr-t->tag (car t)) t '() '())))))

(define-syntax project 
  (syntax-rules ()
    ((_ (x ...) g g* ...)  
     (lambdag@ (a : s c* t)
       (let ((x (walk* x s)) ...)
         ((fresh () g g* ...) a))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (a)
       (inc
         (ifa ((g0 a) g ...)
              ((g1 a) g^ ...) ...))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (a)
       (inc
         (ifu ((g0 a) g ...)
              ((g1 a) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* (unit a) g ...)))))))

(define onceo (lambda (g) (condu (g))))
