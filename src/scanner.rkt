#lang racket

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
  (-> path? any/c)
  (reverse
   (with-input-from-file (path->string path)
     (thunk
      (let loop ([forms '()])
	(let ([form (read)])
	  (if (eof-object? form)
	      forms
	      (loop (cons form forms)))))))))

(provide
 read-file
 scan-kashmir-paths)
