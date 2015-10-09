#lang typed/racket

(define-type go-type (U 'string 'int))
(struct go-arg ([name : String] [type : go-type]))
(struct go-func ([name : String] [args : (Listof go-arg)]))

(struct compilation-error exn:fail:user ())

(: kashmir-read-file (-> Path-String (U EOF (Syntaxof Any))))
(define (kashmir-read-file src-file)
  (call-with-input-file src-file read-syntax))

(: kashmir-compile (-> Syntax go-func))
(define (kashmir-compile c)
  (with-handlers ([exn:misc:match? (lambda (e)
                                     (raise (compilation-error
                                             (format "Unsupported form: ~v" c)
                                             (current-continuation-marks))))])
    (match c
      [(list 'define (? string? name)) (go-func name '())])))
