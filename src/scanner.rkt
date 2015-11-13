#lang racket

(require "pkg-source.rkt")

(define/contract (relative-path-to-package-name path)
  (-> relative-path? symbol?)
  (string->symbol
   (path->string
    (apply
     build-path
     (explode-path
      (path-replace-suffix path ""))))))

(define/contract (scan root-path)
  (-> absolute-path? (listof (list/c absolute-path? symbol?)))
  (for/list ([f (in-directory root-path)] #:when (regexp-match? "\\.km$" f))
    (let* ([p (simplify-path (path->complete-path f root-path))]
	   [root-c (length (explode-path root-path))]
	   [rel (apply build-path (drop (explode-path p) root-c))])
      `(,p ,(relative-path-to-package-name rel)))))

(define/contract (pair-has-pkg pair pkg)
  (-> (list/c absolute-path? symbol?) symbol? boolean?)
  (eq? (cdr pair) pkg))

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

(define/contract (read-file path)
  (-> path? pkg-source?)
  (match (with-input-from-file (path->string path)
	   (thunk
	    (let loop ([s (pkg-source '() '() '())])
	      (match (list s (read))
		[`(,s ,(? eof-object?)) s]
		[`(,(pkg-source '() '() '()) ,(? pkg-decl-expr? p))
		 (loop (pkg-source p '() '()))]
		[`(,(pkg-source '() _ _) ,(? pkg-decl-expr? p))
		 (error "Package must begin with pkg declaratation.")]
		[`(,(pkg-source p is '()) ,(? import-expr? i))
		 (loop (pkg-source p (cons i is) '()))]
		[`(,(pkg-source p is _) ,(? import-expr? i))
		 (error "Imports are not allowed after first definition.")]
		[`(,(pkg-source p is ds) ,(? definition-expr? d))
		 (loop (pkg-source p is (cons d ds)))]
		[e (error (format "Invalid form: ~a" e))]))))
    [(pkg-source p is ds)
     (pkg-source p (reverse is) (reverse ds))]))

(provide
 read-file
 scan-kashmir-paths)
