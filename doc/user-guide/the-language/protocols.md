# Protocols

In Oden, and in funtional programming in general, you can go a long way with
just data and functions.  But sometimes it's nice to be able to create a
function that can take any data type as long as it follows a certain contract.
In Oden this is called a *Protocol* and a protocol contains a set of *methods*.
Given a defined protocol you can *implement* the protocol for a specific type.
The idea of protocols is not new, in fact it's very similar to [_type
classes_](https://www.haskell.org/tutorial/classes.html) in Haskell and
[_traits_](https://doc.rust-lang.org/book/traits.html) in Rust.

When the compiler encounters a use of a Protocol method together with a data
type it checks to see if there's an implementation in scope that it can use. If
not, you will get a compile error. If there is *a single implementation* that
one will be used in the compiled program. The dispatch is done at compile time.

Let's demonstrate Protocols with an animal example that I think everyone loves!
OK, maybe not everyone...

<div class="playground-runnable">
<pre><code class="lang-go">package main

protocol Animal(a) {
  sound : a -> string
}

type Dog = { friendly : bool }
type Chicken = { female : bool }

impl Animal(Dog) {
  sound(dog) = if dog.friendly then "Woof!" else "Grrrrr..."
}

impl Animal(Chicken) {
  sound(chicken) = if chicken.female then "Cluck!" else "Cockadoodledoo!"
}

main() = {
  println(Animal::sound({ friendly = false })) // Grrrrr...
  println(Animal::sound({ female = true }))    // Cluck!
}</code></pre>
</div>

### Work In Progress

As Oden only has transparent type aliases right now it's not super clear in
this code that we create a Dog value and a Chicken value. Have patience, more
type constructs will arrive!

Also, as you might have noticed, methods have to be fully qualified with the
Protocol::Method syntax. This restriction will probably be lifted in the
future, enabling you to write only the method name if it can be unambigously
resolved.

## Overloading Operators

Protocols are used in Oden to provide *overloaded operators*. These are
built-in infix operators that are simple aliases for standard Protocols like
*Num*, *Equality* etc. As you can define implementations for a data type and
a Protocol, you can effectively overload any built-in operator for a custom
data type.

In the following example we implement the *Monoid* protocol for our *Point2D*
data type, which enables us to use the `++` operator to add vector values.

<div class="playground-runnable">
<pre><code class="lang-go">package main

// A point in 2D space.
type Point2D = { x: int, y: int }

// We implement the built-in Monoid protocol for Point2D.
impl Monoid(Point2D) {
  Apply(p1, p2) = {
    x = p1.x + p2.x,
    y = p1.y + p2.y
  }
  Identity = { x = 0, y = 0 }
}

// Let's create some points.
position = { x = 1, y = 3 }
distance = { x = 5, y = 7 }

// With the implementation in scope we can now use the ++ operator together
// with our Point2D type.
target : Point2D
target = position ++ distance

main() = {
  println(target)
}</code></pre>
</div>

## Compatibility with Go

The Protocols construct is not compatible with Go *interfaces* right now, but
that kind of support is planned.
