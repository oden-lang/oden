# Forms

Oden is very simple and supports only a small set of forms. This document
describes those forms and how you can compose programs with them.

## Lists

Drawing inspiration from LISPs, Oden is built on lists. Lists are used for
special forms and function application. A list can be written `(x y z ...)` or
`[x y z ...]`, they are the same.

## Number Literals

The only supported number literal is that of the `int` type.

```scheme
123
-588
```

## Boolean Literals

There are two boolean literals, `true` and `false`, just as in Go.

```scheme
true
false
```

## String Literals

```scheme
"hello"
""
"\nomg\nnewlines"
```

## (> prefix infix)

The syntax of Oden favors prefix over infix notation. For that reason, some of
the infix operators of Go are called as regular functions in Oden.

```scheme
(+ 1 2)
(- 1 2)
(* 2 2)
(/ 4 2)
(/ (- 100 50) 2)
(+ 100 (+ 50 25))
(== 1 2)
(!= 1 2)
(+ "Foo" "Bar")
```

*Currently it is not possible to use these functions as first-class
values, passing them to functions or using them in a let,
e.g. `(map + numbers)`.*

## Functions

A function is created using a `fn` expression. It supports zero or
more arguments and a single expression as the body.

```scheme
(fn (x) (+ x 1))
```

*The current version of Oden does not support polymorphic
functions, i.e. all types have to be inferred or annotated for it to
compile to Go. When `define` is implemented this will probably be
fixed as well.*

Function arguments can be annotated with types.

```scheme
(fn ([x : int]) x)

;; here the type of y is inferred
(fn ([x : int] y) (+ x y))
```

### Defining Functions

When defining a function, a shorthand can be used.

```scheme
(define (identity x) x) ;; same as (define (fn (x) x))
```

### Recursion

Defined functions can call themselves recursively.

```scheme
(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))
```

## Control Flow

The `if` expression has the type `(bool -> (a -> (a -> a)))`.

```scheme
(if (== (+ 10 20) 30) 1 0)
```

## Let Binding

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

## Function Application

Functions are applied using lists; the first element of the list is the
function and the rest of the elements are the arguments.

```
(f x y z ...)
```

Here we call our newly created `square` function with the argument `4`.

```scheme
(let ([square (fn (x) (* x 2))])
  (square 4))
```

Oden also supports functions which take no arguments.

```scheme
(let ([make-num (fn () 3)])
  (* (make-num) (make-num)))
```
