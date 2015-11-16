;; package declaration
(pkg main)

;; import declaration
(import fmt)

;; main function definition, must have type (-> unit)
(define main (fn () (fmt.Println result)))

;; function definition can also use the usual scheme shorthand
(define (identity-string [x : string]) x)

;; value definition
(define result (+ (identity-string "Hello") ", world!"))