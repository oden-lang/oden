#lang racket

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

(define stringo (make-flat-tag 'string string?))
(define floato (make-flat-tag 'float flonum?))
(define fixedo (make-flat-tag 'fixed fixnum?))

(define (infero expr env t)
  (conde
   [(fresh (x)
           (symbolo expr)
           (lookupo expr env x)
           (== `(,expr : ,x) t))]
   [(floato expr)
    (== `(,expr : float) t)]
   [(fixedo expr)
    (== `(,expr : int) t)]
   [(stringo expr)
    (== `(,expr : string) t)]
   [(== 'false expr)
    (== '(false : bool) t)]
   [(== 'true expr)
    (== '(true : bool) t)]
   [(fresh (s st te-ignore te)
           (== `(,s : ,st) expr)
           (== `(,te-ignore : ,st) te)
           (infero s env te)
           (== te t))]
   [(fresh (x b bt b-ignore r d)
           (symbolo x)
           (conde
            [(== `(fn (,x) ,b) expr)]
            [(== `(fn ([,x : ,r]) ,b) expr)])
           (== bt `(,b-ignore : ,d))
           (infero b `((,x : ,r) . ,env) bt)
           (== `((fn ([,x : ,r]) ,bt) : (,r -> ,d)) t))]
   [(fresh (b bt b-ignore d)
	   (== `(fn () ,b) expr)
	   (infero b env bt)
	   (== bt `(,b-ignore : ,d))
	   (== `((fn () ,bt) : (-> ,d)) t))]
   [(fresh (x xt e e-ignore et b bt b-ignore lt)
           (conde
            [(== `(let ([,x ,e]) ,b) expr)]
            [(== `(let ([[,x : ,xt] ,e]) ,b) expr)])
           (symbolo x)
           (== et `(,e-ignore : ,xt))
           (infero e env et)
           (infero b `((,x : ,xt) . ,env) bt)
           (== `(,b-ignore : ,lt) bt)
           (== `((let [[(,x : ,xt) ,et]] ,bt) : ,lt) t))]
   [(fresh (f ft f-ignore a at a-ignore x et)
           (== `(,f ,a) expr)
           (infero f env ft)
           (infero a env at)
           (== `(,f-ignore : (,x -> ,et)) ft)
           (== `(,a-ignore : ,x) at)
           (== `((,ft ,at) : ,et) t))]
   [(fresh (f ft f-ignore et)
	   (== `(,f) expr)
	   (infero f env ft)
	   (== `(,f-ignore : (-> ,et)) ft)
	   (== `((,ft) : ,et) t))]
   [(fresh (c ct c-ignore a at a-ignore b bt b-ignore it)
           (== `(if ,c ,a ,b) expr)
           (infero c env ct)
           (== `(,c-ignore : bool) ct)
           (infero a env at)
           (infero b env bt)
           (== `(,a-ignore : ,it) at)
           (== `(,b-ignore : ,it) bt)
           (== `((if ,ct ,at ,bt) : ,it) t))]))

;; need some better way to do polymorphic operators
(define (arith-operatoro o t)
  (fresh (ot)
         (conde
          [(== '+ o)
           (membero ot '(int float string))
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(membero o '(- * /))
           (membero ot '(int float))
           (== `(,ot -> (,ot -> ,ot)) t)]
          [(membero o '(== !=))
           (membero ot '(int float string))
           (== `(,ot -> (,ot -> bool)) t)]
          [(membero o '(> < >= <=))
           (membero ot '(int float))
           (== `(,ot -> (,ot -> bool)) t)])))

(define (lookupo x env t)
  (fresh ()
         (symbolo x)
         (conde
          ((fresh (_)
                  (== `((,x : ,t) . ,_) env)))
          ((fresh (y _ env^)
                  (== `((,y . ,_) . ,env^) env)
                  (=/= x y)
                  (symbolo y)
                  (conde
                   [(arith-operatoro x t)]
                   [(lookupo x env^ t)]))))))

(define (default-envo t)
  (== t
      `[(unit : unit)
        ;; todo: remove when proper FFI is in place
        (strconv.Itoa : (int -> string))
        (fmt.Println : (string -> unit))
        ]))

(define (infer expr [custom-env '()])
    (let ([t (run 1 (q)
                (fresh (env d)
                       (default-envo d)
		       (== env (append custom-env d))
                       (infero expr env q)))])
    (cond
     [(empty? t) (raise-user-error "Type check failed!")]
     [else (first t)])))

(define (infer-defo def env t)
  (fresh (rec-env name expr expr-ignore expr-t expr-te)
         (== `(define ,name ,expr) def)
         (== rec-env (cons `(,name : ,expr-t) env))
         (infero expr rec-env expr-te)
         (== `(,expr-ignore : ,expr-t) expr-te)
         (== `((define ,name ,expr-te) : ,expr-t) t)))

(define (infer-def def [custom-env '()])
    (let ([t (run 1 (q)
                (fresh (env default-env)
                       (default-envo default-env)
		       (== env (append custom-env default-env))
                       (infer-defo def env q)))])
    (cond
     [(empty? t) (raise-user-error "Type check failed!")]
     [else (first t)])))

(provide
 infero
 infer-defo
 infer
 infer-def)
