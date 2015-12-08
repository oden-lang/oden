#lang racket

(provide
 read-oden-pkg
 read-oden-pkg-file
 read-oden-pkg-source-file
 get-sorted-pkg-names
 sort-pkgs)

(require racket/set)
(require "source-file.rkt")
(require "source-pkg.rkt")
(require graph)

(define/contract (read-oden-pkg [in (current-input-port)])
  (->* () (input-port?) source-pkg?)
  (match (let loop ([s (source-pkg '() '() '())])
	   (match (list s (read in))
	     [`(,s ,(? eof-object?)) s]
	     [`(,(source-pkg '() '() '()) ,(? pkg-decl-expr? p))
	      (loop (source-pkg p '() '()))]
	     [`(,(source-pkg '() is ds) ,(? import-expr? i))
	      (raise-user-error "Imports are not allowed before pkg declaration.")]
	     [`(,(source-pkg p is '()) ,(? import-expr? i))
	      (loop (source-pkg p (cons i is) '()))]
	     [`(,(source-pkg p is ds) ,(? import-expr? i))
	      (raise-user-error "Imports are not allowed after first definition.")]
	     [`(,(source-pkg '() is ds) ,(? definition-expr? d))
	      (raise-user-error "Definitions are not allowed before pkg declaration.")]
	     [`(,(source-pkg p is ds) ,(? definition-expr? d))
	      (loop (source-pkg p is (cons d ds)))]
	     [`(,_ ,e) (raise-user-error (format "Invalid form: ~a" e))]))
    [(source-pkg p is ds)
     (source-pkg p (reverse is) (reverse ds))]))

(define/contract (read-oden-pkg-file path)
  (-> path? source-pkg?)
  (with-input-from-file path read-oden-pkg))

(define/contract (read-oden-pkg-source-file sf)
  (-> source-file? source-pkg?)
  (let ([pkg (read-oden-pkg-file (source-file-path sf))])
    (when (not (equal? (source-file-pkg sf) (source-pkg-name pkg)))
      (raise-user-error (format "~a should have pkg declared as ~a, not ~a."
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

(module+ test
  (require rackunit)
  
  (test-case "file must not be empty"
    (check-exn
     exn:fail?
     (thunk
       (read-oden-pkg '()))))
   
   (test-case "only pkg declaration is required"
    (check-equal?
     (with-input-from-string
	 "(pkg foo)"
       read-oden-pkg)
     (source-pkg
      '(pkg foo)
      '()
      '())))

   (test-case "pkg must be first top-level form"
    (check-exn
     exn:fail?
     (thunk
       (with-input-from-string
	 "(import fmt) (pkg main)"
	 read-oden-pkg))))

   (test-case "imports must appear before definitions"
    (check-exn
     exn:fail?
     (thunk
       (with-input-from-string
	 "(pkg lib) (define test 123) (import fmt)"
	 read-oden-pkg))))

   (let ([pkg-a (source-pkg '(pkg a) '() '())]
	 [pkg-b (source-pkg '(pkg b) '((import a)) '())]
	 [pkg-c (source-pkg '(pkg c) '((import a) (import b)) '())]
	 [pkg-d (source-pkg '(pkg d) '() '())])

     (test-case "source pkgs are topologically sorted by imports"
      (check-equal?
       (sort-pkgs (list pkg-c pkg-b pkg-a pkg-d))
       (list pkg-a pkg-b pkg-c pkg-d)))))
