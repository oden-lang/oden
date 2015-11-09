# The Language

## Supported Forms

Kashmir is very simple and supports only a small set of forms. This document
describes those forms and how you can compose programs with them.

## Number Literals

The only supported number literal is that of the `int` type.

```scheme
123
-588
```

### Boolean Literals

There are two boolean literals, `true` and `false`, just as in Go.

```scheme
true
false
```

### Arithmetic Functions

Some of the infix arithmetic operators of Go are called as regular
functions in Kashmir.

```scheme
(+ 1 2)
(- 1 2)
(* 2 2)
(/ 4 2)
(/ (- 100 50) 2)
(+ 100 (+ 50 25))
```

*Currently it is not possible to use the arithmetic functions as
values, passing them to functions or using them in a let.*

### Equality Functions

The equality and disequality operators from Go are also called as
regular functions.

```scheme
(== 1 2)
(!= 1 2)
```

*Currently it is not possible to use the comparison functions as
values, passing them to functions or using them in a let.*

### Lambda

A function is created using a lambda expression. It supports zero
or more arguments and a single expression as the body.

```scheme
(lambda (x) (+ x 1))
```

*The current version of Kashmir does not support polymorphic
functions, i.e. all types have to be inferred or annotated for it to
compile to Go. Evaluating `(lambda (x) x)` in the REPL will give you
an error. When `define` is implemented this will probably be fixed as
well.*

Lambda arguments can be annotated with types.

```scheme
(lambda ([x : int]) x)

;; here the type of y is inferred
(lambda ([x : int] y) (+ x y))
```

### Control Flow

The `if` expression has the type `(bool -> (a -> (a -> a)))`.

```scheme
(if (== (+ 10 20) 30) 1 0)
```

### Let Binding

The let expression binds identifiers for a set of expressions, used
in the body expression.

```scheme
(let ([x 1]) (+ x 2))
```

Let supports sequential binding, which means that expressions can
use the identifiers of previous bindings, as well as shadowing
previous names.

```scheme
(let ([x 1]
      [y (+ 1 x)])
  (/ y 2))

(let ([x 1]
	  ;; here x gets rebound based on the previous x
	  [x (+ x 1)])
  (* x 2))
```

### Function Application

Functions are applied using lists, just as in other LISPs, where the first
element of the list is the function and the rest of the elements are the
arguments.

```
(f x y z ...)
```

Here we call our newly created `square` function with the argument `4`.

```scheme
(let ([square (lambda (x) (* x 2))])
  (square 4))
```

Kashmir also supports functions which take no arguments.

```scheme
(let ([make-num (lambda () 3)])
  (* (make-num) (make-num)))
```
