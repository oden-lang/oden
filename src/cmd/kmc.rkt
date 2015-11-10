#lang racket

(require racket/cmdline)

(require "../compiler.rkt")
(require "../scanner.rkt")

(module+ main
  (command-line #:program "kashmir"
		#:args (in-path out-path)
		(let* ([in-prg (read-file in-path)]
		       [out-prg (compile-top-level-forms-to-go in-prg)])
		  (call-with-output-file out-path #:exists 'replace
		    (lambda (out)
		      (display out-prg out))))))
