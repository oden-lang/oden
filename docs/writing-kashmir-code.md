# Writing Kashmir Code

In Kashmir the following rules apply:

* Every source file corresponds to a single package.
* Source files must begin with the `pkg` declaration.
* Source files may use zero or more `import` declarations after the `pkg`
  declaration.
* Source files may, after `pkg` and `pkg` declarations, define zero or more
  functions and values using `define`.

## Example

```scheme
;; package declaration
(pkg main)

;; import declaration
(import fmt)

;; function definition
(define identity-string
  (lambda ([x : string]) x))

;; value definition
(define result (+ (identity-string "Hello") ", world!"))

;; main function definition, must have type (-> unit)
(define main (lambda () (fmt.Println result)))
```
