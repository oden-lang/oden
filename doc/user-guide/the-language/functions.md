# Functions

A *function expression* is created using an argument list followed by an arrow
and a function body. The argument list can contain zero or more arguments,
separated by commas and enclosed in parenthesis. The function body is a single
expression that gets evaluated when the function is applied.

Here's a function that takes an argument `x` and applies the `+` operator to
`x` and the literal `1`, effectively incrementing the number.

```go
(x) -> x + 1
```

But what if you need to do more than one thing in the function? Like printing
something to the console or saving a record in the database, before returning
a value? In that case you can use a [block expression](blocks.md).

```go
(x) -> {
  println("Read more about blocks later on in the User Guide.")
  x + 1
}
```

## Defining Functions

Functions can be defined at the top level just like any other value. We write
the name, an equals sign and the function expression.

```
identity = (x) -> x
```

However, as defining functions is such a common task, the *function definition
shorthand* can be used. With it you write the function name, the argument list,
an equals sign and the function body.

```
identity(x) = x
```

The two definitions of `identity` are equivalent.

### Function Application

Functions are applied using parenthesis containg the parameters separated by
commas.

```
f(x, y, z, ...)
```

Here we first define a function `square` and then we apply the function to
the literal `4`.

```go
square(x) = x * 2
squareOfFour = square(4)
```

We can also define functions that takes no arguments. Here the function
`makeNum` is applied with no argument to give us some number. This is useful
for deferring a computation until it's needed.

```go
makeNum() = 3
result = makeNum() * makeNum()
```

### Recursion

Top-level functions can call themselves recursively. There is currently no
*tail call optimization* being done in Oden, so be careful with these.

```go
factorial(n) = if n < 2 then 1 else n * factorial(n - 1)
```

## Function Types

The type of a function $$f\colon A \to B$$ is written `A -> B`.

When defining a value or a function it is recommended to add an explicit type
signature. Type signatures must be written before the definition.

In the following code we specify `identity` to have type `a -> a` where `a` is
a type variable.

```oden
identity : forall a. a -> a
identity(x) = x
```

Types of functions that take no argument are written in a similar way - you
just omit the parameter type before the arrow. The following code specifies the
type of our `makeNum` function.

```oden
makeNum : -> int
makeNum() = 3
```

If we only write the type signature and omit the definition, we will get a
compiler error.

```go
anotherFunction : forall a b. a -> b -> a
// definition is missing!
```

## Currying

Oden supports [*currying*](https://en.wikipedia.org/wiki/Currying)
by default. The type of a function $$f\colon X \to (Y \to Z)$$ can be
written `X -> (Y -> Z)`. To make it more readable you can also write the same
type as `X -> Y -> Z`, but they are equivalent.

```go
// oden lets you write
(x, y, z) -> x
// but that gets translated to
(x) -> (y) -> (z) -> x
// and the inferred type becomes
forall a b c. a -> b -> c -> a
```

Currying enables you to partially apply a function. In the following code we
create function `personSays` that takes two strings. We apply the function
with only one string and get a function back. Later we apply it with the
other string to actually print something.

<div class="playground-runnable">
<pre><code class="lang-go">package main

personSays(who, what) = println(who ++ " says: " ++ what)
simonSays = personSays("Simon")

main() = simonSays("write a program in Oden")</code></pre>
</div>

### Go Functions

When calling a function imported from Go the function gets curried, so you can
partially apply it just like you can with Oden functions.
