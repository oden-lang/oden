#lang racket

;; todo
(define (scan src-dir target-dir)  
  (for ([f (in-directory src-dir)] #:when (regexp-match? "\\.km$" f))
    (displayln f)))

(define (read-file path)
  (reverse
   (with-input-from-file path
     (thunk
      (let loop ([forms '()])
	(let ([form (read)])
	  (if (eof-object? form)
	      forms
	      (loop (cons form forms)))))))))

(provide
 read-file)
