#lang racket

(require "source-pkg.rkt")

(struct source-file (path pkg) #:transparent)

(define/contract (relative-path-to-package-name path)
  (-> relative-path? symbol?)
  (string->symbol
   (path->string
    (apply
     build-path
     (explode-path
      (path-replace-suffix path ""))))))

(define/contract (scan root-path)
  (-> absolute-path? (listof source-file?))
  (for/list ([f (in-directory root-path)] #:when (regexp-match? "\\.km$" f))
    (let* ([p (simplify-path (path->complete-path f root-path))]
	   [root-c (length (explode-path root-path))]
	   [rel (apply build-path (drop (explode-path p) root-c))])
      (source-file p (relative-path-to-package-name rel)))))

(define/contract (source-file-has-pkg sf pkg)
  (-> source-file? symbol? boolean?)
  (eq? (source-file-pkg sf) pkg))

(define (to-absolute-path p)
  (if (relative-path? p)
      (simplify-path (path->complete-path p (current-directory)))
      p))

(define (scan-kashmir-paths)
  (define (add-src s) (to-absolute-path (string->path (format "~a/src" s))))
  (let ([roots (map add-src (string-split
			     (or (getenv "KASHMIR_PATH") ".")
			     ":"))])
    (append* (map scan roots))))

(define/contract (read-kashmir-pkg [in (current-input-port)])
  (->* () (input-port?) source-pkg?)
  (match (let loop ([s (source-pkg '() '() '())])
	   (match (list s (read in))
	     [`(,s ,(? eof-object?)) s]
	     [`(,(source-pkg '() '() '()) ,(? pkg-decl-expr? p))
	      (loop (source-pkg p '() '()))]
	     [`(,(source-pkg '() is ds) ,(? import-expr? i))
	      (error "Imports are not allowed before pkg declaration.")]
	     [`(,(source-pkg p is '()) ,(? import-expr? i))
	      (loop (source-pkg p (cons i is) '()))]
	     [`(,(source-pkg p is ds) ,(? import-expr? i))
	      (error "Imports are not allowed after first definition.")]
	     [`(,(source-pkg '() is ds) ,(? definition-expr? d))
	      (error "Definitions are not allowed before pkg declaration.")]
	     [`(,(source-pkg p is ds) ,(? definition-expr? d))
	      (loop (source-pkg p is (cons d ds)))]
	     [`(,_ ,e) (error (format "Invalid form: ~a" e))]))
    [(source-pkg p is ds)
     (source-pkg p (reverse is) (reverse ds))]))

(define/contract (read-kashmir-pkg-file path)
  (-> path? source-pkg?)
  (with-input-from-file path read-kashmir-pkg))

(provide (all-defined-out))
