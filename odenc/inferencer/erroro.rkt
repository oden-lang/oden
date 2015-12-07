#lang racket

(provide
 erroro
 not-erroro)

(require Racket-miniKanren/miniKanren/mk)

(define (error? e)
  (match e
    [`(error . ,_) #t]
    [_ #f]))

(define erroro (make-flat-tag 'error error?))
(define not-erroro (make-flat-tag 'error (compose not error?)))

(module+ test
  (require rackunit)

  (test-case "not-erroro unbound"
    (check-match
     (run* (q)
           (fresh (x y)
                  (not-erroro x)
                  (erroro y)
                  (== q `(,x ,y))))
     `((_.0 . ,_) . ,_))))

