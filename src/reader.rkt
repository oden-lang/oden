#lang racket

(require racket/set)
(require "source-file.rkt")
(require "source-pkg.rkt")
(require "../lib/graph.rkt")

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

(define/contract (read-kashmir-pkg-source-file sf)
  (-> source-file? source-pkg?)
  (let ([pkg (read-kashmir-pkg-file (source-file-path sf))])
    (when (not (equal? (source-file-pkg sf) (source-pkg-name pkg)))
      (error (format "~a should have pkg declared as ~a, not ~a."
		     (source-file-path sf)
		     (source-file-pkg sf)
		     (source-pkg-name pkg))))
    pkg))

(define (get-sorted-pkg-names pkgs)
  (graph-topological-sort
   (make-graph
    (for*/list ([pkg pkgs]
		[i (source-pkg-imports pkg)])
      (list (cadr i)
	    (cadr (source-pkg-decl pkg)))))))

(define/contract (sort-pkgs pkgs)
  (-> (listof source-pkg?) (listof source-pkg?))
  (let* ([h (for/hash ([pkg pkgs])
	      (values (source-pkg-name pkg) pkg))]
	 [sorted-names (get-sorted-pkg-names pkgs)]
	 [disjoint-names (set-symmetric-difference (list->set sorted-names)
					     (list->set (hash-keys h)))]
	 [all (append sorted-names (set->list disjoint-names))])
    (append* (for/list ([k all])
	       (if (hash-has-key? h k)
		   (list (hash-ref h k))
		   '())))))

(provide
 read-kashmir-pkg
 read-kashmir-pkg-file
 read-kashmir-pkg-source-file
 get-sorted-pkg-names
 sort-pkgs)
