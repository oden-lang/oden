#lang racket

(require "inferencer.rkt")
(require "compiler.rkt")
(require "explode.rkt")

(define (on-exit-exception e)
  (raise-user-error "\nBye!"))

(define (text-bold)
  (display "\033[1m"))

(define (text-blue)
  (display "\033[34m"))

(define (text-red)
  (display "\033[31m"))

(define (text-reset)
  (display "\033[m"))

(define (repl)
  (displayln "Welcome to Kashmir! Type CTRL-C to exit.")
  (define (loop)
    (with-handlers ([exn:break? on-exit-exception]
		    [exn:fail? (lambda (e)
				 (text-red)
				 (displayln (exn-message e))
				 (text-reset))])
      (text-bold)
      (text-blue)
      (display "kashmir> ")
      (text-reset)
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
