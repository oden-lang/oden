#lang racket

(require racket/match)
(require racket/system)
(require "inferencer.rkt")
(require "explode.rkt")

(define (compile-type type)
  (match type
    [`(,r -> ,d)
     (format "func (~a) (~a)"
	     (compile-type r)
	     (compile-type d))]
    [`(-> ,d)
     (format "func () (~a)"
	     (compile-type d))]
    [(? list? l) (string-join (map ~a l) "_")]
    [(? symbol? t) (~a t)]))

(define (infix-operator? op)
  (member op '(+ - * / == !=)))

(define (compile-expr typed-expr)
  (match typed-expr
    [(? number? x) (~a x)]
    [`(,e : ,t) (compile-expr e)]
    ['false "false"]
    ['true "true"]
    [(? symbol? s) (~a s)]
    [(list
      (list
       (list
        (list (? infix-operator? op) ': _)
        (list a ': _))
       ':
       (list _ '-> _))
      (list b ': _))
     (format "(~a ~a ~a)"
             (compile-expr a)
             op
             (compile-expr b))]
    [`(lambda () (,e : ,et))
     (format "(func () ~a {\nreturn ~a\n})"
	     (compile-type et)
	     (compile-expr e))]
    [`(lambda ((,x : ,xt)) (,e : ,et))
     (format "(func (~a ~a) ~a {\nreturn ~a\n})"
             (compile-expr x)
             (compile-type xt)
             (compile-type et)
             (compile-expr e))]
    [`(let (([,x : ,xt] (,e : ,et))) (,b : ,bt))
     (format "(func () ~a {\nvar ~a ~a = ~a\nreturn ~a\n}())"
             (compile-type bt)
             (compile-expr x)
             (compile-type xt)
             (compile-expr e)
             (compile-expr b))]
    [`((,f : ,_))
     (format "~a()"
	     (compile-expr f))]
    [`((,f : ,ft) (,a : ,at))
     (format "~a(~a)"
             (compile-expr f)
             (compile-expr a))]
    [`(if ,c (,a : ,t) (,b : ,t))
     (format "(func() ~a {\nif ~a {\nreturn ~a\n} else {\nreturn ~a\n}\n})()\n"
             (compile-type t)
             (compile-expr c)
             (compile-expr a)
             (compile-expr b))]))

(define (go-fmt src)
  (with-input-from-string src
    (lambda () (with-output-to-string
            (lambda () (system "gofmt"))))))

(define (go-run src)
  (let* ([tmp-path (make-temporary-file "~a.go")]
         [tmp-file (open-output-file tmp-path #:exists 'truncate)]
         [formatted (go-fmt src)])
    (display formatted tmp-file)
    (close-output-port tmp-file)
    (with-output-to-string
      (lambda () (system* (find-executable-path "go") "run" (~a tmp-path))))))

(define (kashmir-compile-expr expr)
  (compile-expr (:? (explode expr))))

(define (kashmir-compile-prg expr)
  (format "
package main

import \"fmt\"

func main() {
_res := ~a
fmt.Println(_res)
}
"
          (kashmir-compile-expr expr)))

(provide
 go-fmt
 go-run
 kashmir-compile-expr
 kashmir-compile-prg)
