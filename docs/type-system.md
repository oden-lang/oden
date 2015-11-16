# Type System

Oden aims to be a flexible and powerful functional language. The
type system should help ensure the correctness of your programs
without getting in the way. Oden builds on an extended typed lambda
calculus and performs type inference on your code.

## Built-in Types

Just like in Go, there are some built in types:

* `boolean`
* `int`
* `float`
* `string`
* `unit`

### Unit

The `unit` type represents *no value*. It is used for functions that
causes side effects and have no useful return value.

Unit is also used for interopability with functions in Go that has no
return value at all. For example, the Go expression `func (s string) {
fmt.Println(s) }` would have the type `(string -> unit)` in Oden.

The literal `unit` has the type `unit`.

## Functions

The type of a function $$f\colon A \to B$$ is written `(A -> B)`.

### Currying

Oden supports [*currying*](https://en.wikipedia.org/wiki/Currying)
by default. The type of a function $$f\colon X \to (Y \to Z)$$ is
written `(X -> (Y -> Z))` which reflects that it is actually a
curried function.

Oden lets you write `(fn (x y z) x)` but that gets translated
to `(fn (x) (fn (y) (fn (z) x)))` and the inferred type
becomes `(a -> (b -> (c -> a)))`.

### No-argument Functions

Functions that take no argument, often used to introduce lazyness, are written
in a similary way. A function that takes no argument and returns an `int` is
written `(-> int)`.

## Type Annotations

Oden accepts type annotations to guide the inferencer. This is
useful to constrain a function to a specific type or to make your
intentions clear. The following example shows two functions that have
the exact same type but where the second functions is annotated.

```scheme
(define plus-1
  (fn (a)
    (+ a 1)))
  
(define plus-1-annotated
  (fn ([a : int])
    (+ a 1)))	
```

Even if the inferencer does most of the work, expressions can be annotated as well
using an annotation expression, written `(expr : type)`.

```scheme
(define plus
  (fn (x y) (+ (x : int) (y : int))))

(define plus-1
  ((plus 1) : (-> int int)))
```

A function $$f\colon A \to B$$ can be written with a type annotation as
`(f : (A -> B))`.
