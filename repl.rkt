#lang racket

(require "inferencer.rkt")
(require "compiler.rkt")
(require "explode.rkt")

(define (on-exit-exception e)
  (raise-user-error "\nBye!"))

(define (repl)
  (displayln "Welcome to Kashmir! Type CTRL-C to exit.")
  (define (loop)
    (with-handlers ([exn:break? on-exit-exception]
		    [exn:fail? (lambda (e) (displayln e))])
      (display "kashmir> ")
      (let* ([input (read)])
	(if (eof-object? input)
	    (newline)
	    (let ([t (car (cddr (:? (explode input))))]
		  [output (with-input-from-string (go-run (kashmir-compile-prg input))
			    (lambda () (read)))])
	      (displayln (format "~a : ~a" output t))))))
    (loop))
  (loop))

(repl)
