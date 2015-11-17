#lang racket

(require racket/cmdline)

(require "scanner.rkt")
(require "reader.rkt")
(require "compiler.rkt")
(require "go-backend.rkt")
(require "source-file.rkt")
(require "source-pkg.rkt")

(define (pkg->path pkg root)
  (let ([pkg-part (apply build-path
			 (map string->path (string-split (symbol->string pkg) "/")))])
    (build-path root "src" pkg-part)))

(define (print-pkg pkg path)
  (let ([out-prg (codegen-pkg pkg)])
    (call-with-output-file (path->string path) #:exists 'replace
			   (lambda (out)
			     (display out-prg out)))))

(define/contract (compile-to out-path pkg)
  (-> path? source-pkg? void?)
  (let* ([pkg-name (cadr (source-pkg-decl pkg))]
	 [pkg-dir-out (pkg->path pkg-name out-path)]
	 [file-out (build-path pkg-dir-out "oden_out.go")])
    (displayln (format  "Compiling ~a to ~a" pkg-name file-out))
    (make-directory* pkg-dir-out)
    (print-pkg
     (compile-pkg pkg)
     file-out)))

(module+ main
  (command-line #:program "odenc"
		#:args (out-directory)
		(for ([pkg (sort-pkgs
			    (map read-oden-pkg-source-file
				 (scan-oden-paths)))])
		  (compile-to (string->path out-directory) pkg))))
