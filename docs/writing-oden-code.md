# Writing Oden Code

In Oden the following rules apply:

* Every source file corresponds to a single package.
* Source files must begin with the `pkg` declaration. The declaration should,
  unlike in Go, specify the complete name, e.g. `(pkg a/b/c)`.
* Source files may use zero or more `import` declarations after the `pkg`
  declaration.
* Source files may, after `pkg` and any `import` declarations, define
  zero or more functions and values using `define`. The order of
  definitions does not matter - a value `foo` can be the last defined
  value even if it's referenced before.

## Example

```scheme
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
```
