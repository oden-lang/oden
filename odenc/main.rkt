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

(define-syntax (get-version stx)
  #`#,(or (getenv "VERSION") "undefined version"))

(define version (get-version))

(define print-version (make-parameter false))
(define oden-path (make-parameter "."))
(define output-directory (make-parameter "."))

(module+ main
  (command-line #:program "odenc"
                #:usage-help "\nCompiles all the Oden source files in the Oden Path to Go source files.\n"
                #:help-labels ""
                #:once-any
                [("-v" "--version") "Print the odenc version." (print-version true)]
                #:once-each
                [("-p" "--oden-path")
                 value
                 "Overrides the Oden Path with the specified colon-separated paths. The Oden Path is otherwise read from the ODEN_PATH environment variable and if not specified it defaults to the current directory. The Oden Path is similar to the GOPATH in structure."
                 (oden-path value)]
                [("-o" "--output-directory")
                 value
                 "Specifies in what directory output Go files will be written. The directory layout follows the GOPATH. Defaults to the current directory."
                 (output-directory value)]
		#:args ()
                (cond
                  [(print-version) (displayln version)]
                  [else (for ([pkg (sort-pkgs
                                    (map read-oden-pkg-source-file
                                         (scan-oden-paths (oden-path))))])
                          (compile-to (string->path (output-directory)) pkg))])))
