#lang racket

(require "source-file.rkt")
(require "source-pkg.rkt")

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
  (for/list ([f (in-directory root-path)] #:when (regexp-match? "\\.oden$" f))
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

(define (scan-oden-paths [oden-path (or (getenv "ODEN_PATH") ".")])
  (define (add-src s) (to-absolute-path (string->path (format "~a/src" s))))
  (let ([roots (map add-src (string-split oden-path ":"))])
    (append* (map scan roots))))

(provide (all-defined-out))
