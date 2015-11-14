#lang racket/base

(require racket/set)

(provide graph?
	 make-graph
	 graph-edges-from
	 graph-edges-to
	 graph-add-edge!

	 graph-topological-sort)

(struct graph (nodes forward-edges backward-edges))

(define (make-graph [edges '()])
  (let ((g (graph (make-hash) (make-hash) (make-hash))))
    (for-each (lambda (e) (graph-add-edge! g (car e) (cadr e)))
	      edges)
    g))

(define (graph-edges-from g node)
  (hash-ref (graph-forward-edges g) node '()))

(define (graph-edges-to g node)
  (hash-ref (graph-backward-edges g) node '()))

(define (add-edge! table source sink)
  (hash-update! table
		source
		(lambda (old)
		  (if (memq sink old)
		      old
		      (cons sink old)))
		'()))

(define (delete-edge! table source sink)
  (hash-update! table
		source
		(lambda (old)
		  (filter (lambda (x) (not (eq? x sink))) old))
		'()))

(define (graph-add-edge! g from to)
  (hash-set! (graph-nodes g) from from)
  (hash-set! (graph-nodes g) to to)
  (add-edge! (graph-forward-edges g) from to)
  (add-edge! (graph-backward-edges g) to from)
  (void))

(define (graph-topological-sort g)
  (let ((visited (make-hash))
	(result '()))
    (define (visited? n)
      (hash-ref visited n #f))
    (define (visit n path)
      (when (memq n path)
	(error 'graph-topological-sort "Cycle detected: ~v" path))
      (when (not (visited? n))
	(hash-set! visited n #t)
	(let ((new-path (cons n path)))
	  (for ((m (hash-ref (graph-forward-edges g) n '())))
	    (visit m new-path)))
	(set! result (cons n result))))
    (for ((n (in-hash-keys (graph-nodes g)))
	  #:when (null? (hash-ref (graph-backward-edges g) n '())))
      (visit n '()))
    (when (< (hash-count visited)
	     (hash-count (graph-nodes g)))
      (error 'graph-topological-sort "Some nodes unused: ~v"
	     (set->list
	      (set-subtract (list->set (hash-keys (graph-nodes g)))
			    (list->set (hash-keys visited))))))
    result))
