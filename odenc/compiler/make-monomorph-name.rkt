#lang racket

(provide make-monomorph-name)

(define (encode-type t [level 1])
  (define pad (build-string level (const #\_)))
  (match t
    [(? symbol? s) (symbol->string s)]
    [`(,t1 -> ,t2) (string-append
                    (encode-type t1 (add1 level))
                    pad
                    "to"
                    pad
                    (encode-type t2 (add1 level)))]
    [`(-> ,st) (string-append
                "to"
                pad
                (encode-type st (add1 level)))]
    [`(,constructor . ,args)
     (string-append 
      (encode-type constructor)
      pad
      "of"
      pad
      (string-join (map (lambda (t)
                          (encode-type t (add1 level)))
                        args)
                   pad))]))

(define (make-monomorph-name name type)
  (string->symbol (format "~a_inst_~a" name (encode-type type))))

(module+ test
  (require rackunit)

  (test-case "fn"
    (check-equal?
     (make-monomorph-name 'identity '(int -> int))
     'identity_inst_int_to_int))

  (test-case "fn to fn"
    (check-equal?
     (make-monomorph-name 'identity '((int -> int) -> (int -> int)))
     'identity_inst_int__to__int_to_int__to__int))

    (test-case "no-arg fn returning fn"
    (check-equal?
     (make-monomorph-name 'identity '(-> (int -> int)))
     'identity_inst_to_int__to__int)))
