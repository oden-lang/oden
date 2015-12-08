#lang racket

(provide
 erroro
 not-erroro
 error?)

(require Racket-miniKanren/miniKanren/mk)

(define (error? e)
  (match e
    [`(error . ,_) #t]
    [_ #f]))

(define erroro (make-flat-tag 'error error?))
(define not-erroro (make-flat-tag 'not-error (compose not error?)))

(module+ test
  (require rackunit)

  (test-case "not-erroro unbound"
    (check-equal?
     (run* (q)
           (== q `(error foo))
           (erroro q))
     `((error foo)))))

