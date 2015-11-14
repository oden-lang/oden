#lang racket

(require racket/cmdline)

(require "../scanner.rkt")
(require "../compiler.rkt")
(require "../go-backend.rkt")

(define (pkg->path pkg root)
  (let ([pkg-part (apply build-path
			 (map string->path (string-split (symbol->string pkg) "/")))])
    (build-path root "src" pkg-part)))

(define (print-pkg pkg path)
  (let ([out-prg (codegen-pkg pkg)])
    (call-with-output-file (path->string path) #:exists 'replace
			   (lambda (out)
			     (display out-prg out)))))

(define/contract (compile-to out-path sf)
  (-> path? source-file? void?)
  (let* ([pkg-dir-out (pkg->path (source-file-pkg sf) out-path)]
	 [file-out (build-path pkg-dir-out "kashmir_out.go")])
    (displayln (format  "Compiling ~a to ~a" (source-file-pkg sf) file-out))
    (make-directory* pkg-dir-out)
    (print-pkg
     (compile-pkg (read-kashmir-pkg-file (source-file-path sf)))
     file-out)))

(module+ main
  (command-line #:program "kmc"
		#:args (out-directory)
		(for ([sf (scan-kashmir-paths)])
		  (compile-to (string->path out-directory) sf))))
