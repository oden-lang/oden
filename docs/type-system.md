# Type System

Kashmir aims to be a flexible and powerful functional language. The
type system should help ensure the correctness of your programs
without getting in the way. Kashmir builds on an extended typed lambda
calculus and performs type inference on your code.

## Built-in Types

Just like in Go, there are built in types:

* `boolean`
* `int`
* `float`
* `string`

## Functions

The type of a function *f : A &rarr; B* is written `(-> A B)`.

### Currying

Kashmir supports [*currying*](https://en.wikipedia.org/wiki/Currying)
by default. The type of a function *f: X &rarr; (Y &rarr; Z)* is
written `(-> X (-> Y Z))` which reflects that it is actually a
curried function.

### No-argument Functions

Functions that take no argument, often used to introduce lazyness, are written
in a similary way but omits the *domain*: `(-> int)`
