# The Language

## Packages

A *package* in Oden is the top level in which you write Oden source code.
Every `.oden` source file corresponds to a single package; a package cannot be
be spread over multiple source files. Every package must begin with a
*package declaration*. The `package` keyword is followed by a *fully
qualified package name*.

```{.hidden}
package @\textit{fully-qualified-name}@
```
<pre><code>package <em>fully-qualified-name</em></code></pre>

The fully qualified name consists of one or more *segments* separated by
by slashes.

```{.hidden}
@$\text{\textit{name}}_{\text{\textit{1}}}$@/@$\text{\textit{name}}_{\text{\textit{2}}}$@/.../@$\text{\textit{name}}_{\text{\textit{n}}}$@
```
<pre><code>package <em>name<sub>1</sub></em>/<em>name<sub>2</sub></em>/.../<em>name<sub>n</sub></em></code></pre>

After the package declaration follows zero or more *import declarations*.

```{.hidden}
import @\textit{fully-qualified-name}@
```
<pre><code>import <em>fully-qualified-name</em></code></pre>

After the package and import declarations follows the part where we do the
actual work, called *value definitions*. These can be basic values like numbers
or strings, but also functions.

The following program imports the `strconv` package from Go, defines a
function `shout` and a value `result`, and defines the *main* function, the
entry point of the program. The main function must be a function taking
no argument and returning `()`, i.e. have the type `(-> ())`.

```{.oden .playground-runnable language=oden}
package main

import strconv

shout(s) = s ++ "!"

result = shout(strconv.Itoa(9000))

main() = println(result)
```

## Basic Literals

There are two types of number literals supported; `int` and `float64` literals.

```{.oden language=oden}
123
-588
0.00001
-400.123
```

There are two boolean literals, `true` and `false`, just as in Go.

```{.oden language=oden}
true
false
```

String literals are enclosed in double quotes.

```{.oden language=oden}
"hello"
""
"\nomg\nnewlines"
```

## Unit

*Unit* is a built-in type that is inhabited by a single value, the *unit
value*, which carries no information. Unit is returned by functions that cause
side effects and have no useful return value to give.

Both the unit type and the unit value literal are written with an empty set of
parenthesis. Here's how you define a function that returns unit and use
that function to perform a side effect.

```{.oden language=oden}
brewCoffee : Amount -> ()
brewCoffee(amount) = {
  // do the actual coffee brewing here

  // then return unit
  ()
}
```

Often the side effect functions you use already return unit, which means
you don't need to explicitly return unit like in the previous example.  In
the following example we use our `brewCoffee` function and then print to
the console. We return unit by ending with the application of the `println`
function, as it returns unit.

```{.oden language=oden}
coffeeBreak : -> ()
coffeeBreak() = {
  brewCoffee(twoCups)
  println("All right, ready to code again!")
}
```

Functions from Go, that has no return value, return unit when imported in Oden.
For example, the following Go function would have the type `string -> ()` in
Oden.

```{.go}
func sayHi(name string) {
  fmt.Println("Hi", name)
}
```

## Operators

There are two types of operators in Oden -- *unary* and *binary*.

Unary operators take one value as argument and are written before the value
expression. This is called *prefix notation*. Oden has only two unary
operators; negation and logical not.

```{.oden language=oden }
-1
!x
```

Binary operators take two values as arguments and are written in between
the value expressions. This is called *infix notation*. Let's look at some
binary operators:

```{.oden language=oden }
1 + 2
1 - 2
2 * 2
4 / 2
(100 - 50) / 2
100 * (50 + 25)
1 == 2
1 != 2
"Foo" ++ "Bar"
false && (true || (1 == 2))
```

During compilation operators are expanded in to predefined [*protocol method
applications*](#protocols). The following table shows what they expand to.

Operator  Protocol  Method
--------  --------  ------
`-`       Num       Negate
`!`       Logical   Not
`+`       Num       Add
`-`       Num       Subtract
`*`       Num       Multiply
`/`       Num       Divide
`&&`      Logical   Conjunction
`||`      Logical   Disjunction
`++`      Monoid    Apply

Because of this expansion we can *overload* all operators with our own types --
they are just aliases for protcols methods. For more information on overloading
see ["Overloading Operators"](#overloading-operators).

### Using Operators As Function Values

Currently it is not possible to use these operators as first-class values,
passing them to functions or using them in a let, e.g. `map(+, numbers)`. One
could however wrap them in a standard function and pass that to a
higher-order function.

## Functions

Functions are the bread and butter of any functional programming language.
Functions map input values to output values and are used to structure a program
and to create reusable abstractions. If you know what the function
*uppercase* does, there's no need for you to know *how* it does it. You can use
it to get your strings uppercased without worrying about the implementation.

In Oden functions are first-class entities. You can create them on the fly
and pass them around as values. A *function expression* is written using an
argument list followed by an arrow and a function body.

```{.oden language=oden}
(arg1, arg2, ..., argN) -> expr
```

The argument list can contain zero or more arguments,
separated by commas and enclosed in parenthesis. The function body is a single
expression that gets evaluated when the function is applied.

Here's a function that takes an argument `x` and applies the `+` operator to
`x` and the literal `1`, effectively incrementing the number.

```{.oden language=oden}
(x) -> x + 1
```

But what if you need to do more than one thing in the function? Like printing
something to the console or saving a record in the database, before returning
a value? In that case you can use a [block expression](#blocks).

```{.oden language=oden}
(x) -> {
  println("Let's increment!")
  x + 1
}
```

### Defining Functions

Functions can be defined at the top level just like any other value. We write
the name, an equals sign and the function expression.

```
increment = (x) -> x + 1
```

However, as defining functions is such a common task, Oden provides a *function
definition shorthand*. Using the shorthand you write the function name, the
argument list, an equals sign and the function body.

```
increment(x) = x + 1
```

The two definitions of `increment` are equivalent.

### Function Application

Functions are applied using parenthesis, contaning the parameters separated by
commas.

```
f(x, y, z, ...)
```

Here we first define a function `square` and then we apply the function to
the literal `4`.

```{.oden language=oden}
square(x) = x * 2
squareOfFour = square(4)
```

We can also define functions that takes no arguments. Here the function
`makeNum` is applied with no argument to give us some number. This is useful
for deferring a computation until it's needed.

```{.oden language=oden}
makeNum() = 3
result = makeNum() * makeNum()
```

### Recursion

Top-level functions can apply themselves recursively. There is currently no
[*tail call optimization*](https://en.wikipedia.org/wiki/Tail_call) being done
in Oden, so be careful with these.

```{.oden language=oden}
factorial(n) = if n < 2 then 1 else n * factorial(n - 1)
```

### Function Types

The type of a function from type _A_ to type _B_ is written `A -> B`.

When defining a value or a function it is recommended to add an explicit type
signature. Type signatures must be written before the definition.

In the following code we specify `increment` to have type `int -> int`.

```oden
increment : int -> int
increment(x) = x + 1
```

Types of functions that take no argument are written in a similar way -- you
just omit the parameter type before the arrow. The following code specifies the
type of our `makeNum` function.

```oden
makeNum : -> int
makeNum() = 3
```

If we only write the type signature and omit the definition, we will get a
compiler error.

```{.oden language=oden}
anotherFunction : forall a b. a -> b -> a
// definition is missing!
```

### Currying

Oden functions are [*curried*](https://en.wikipedia.org/wiki/Currying). The
type of a function that takes values of types `a` and `b`, returning a `c` can
be written `a -> b -> c`. But it can also be written as `a -> (b -> c)`, which
is how the compiler really sees it.

The parenthesis indicate that the function really has one argument of type `a`
and return another function from `b` to `c`. But how can we write functions
that take multiple arguments then? The answer is that we can't, but the
compiler performs this tiny trick to make it look like we can.

Oden lets you write a function expression like this:

```{.oden language=oden}
(x, y, z) -> x
```

But that gets expanded to:

```{.oden language=oden}
(x) -> (y) -> (z) -> x
```

And the inferred type of the function expression becomes:

```{.oden language=oden}
forall a b c. a -> b -> c -> a
```

#### Using Currying

With currying you can apply only the first value of a function, get another
function back, and use that function later -- similar to *partial application*.
In the following code we create function `personSays` that takes two strings.
We apply the function with only one string and get a function back. Later we
apply it with the other string to actually print something.

```{.oden .playground-runnable language=oden}
package main

personSays(who, what) =
  println(who ++ " says: " ++ what)

simonSays = personSays("Simon")

main() = simonSays("write a program in Oden")
```

#### Go Functions

When calling a function imported from Go the function gets curried, so you can
partially apply it just like you can with Oden functions.

## Type Variables

Functions that can take values of an unknown type are called *polymorphic
functions* and have *polymorphic types*. A polymorphic type contains one or
more *type variables*. These are like placeholders for the specific types to be
used later.

A type variables is introduced into the scope of the type signature by using
the `forall` keyword, followed by one or more type variable names and a
terminating dot.  This is called *universal quantification*.

```{.oden language=oden}
identity : forall a. a -> a
```

When applying a function to a value the function type is *instantiated* to
match the type of the value. If we apply the `identity` function to a value of
type `int`, the instantiated type of `identity` will be `int -> int`. In other
words, all occurences of the type variable `a` are substituted for `int`. These
substitutions must match. `a -> a` cannot be instantiated to `int -> string`,
for example.

## Control Flow

The only control flow mechanism available right now is the `if` expression.

```{.oden language=oden}
if 10 + 20 == 30 then 1 else 0
```

If the clauses get big you should consider using [blocks](#blocks) around
expressions, even if you have only a single expression in each clause.

```{.oden language=oden}
if 10 + 20 == 30 then {
  thisFunctionCallIsVeryLengthy()
} else {
  andPerhapsThisOneAlso()
}
```

The `if` expression can be seen as having the type:

```{.oden language=oden}
forall a. bool -> a -> a -> a
```

#### Other Constructs

The plan is to support pattern matching as a central way to do control flow.
Other constructs may appear in the future as well, like `cond` from LISPs or
*Guards* from Erlang and Haskell.

## Blocks

A block is an expression containing *one or more* expressions -- it cannot be
empty. A block expression evaluates to the value of the last expression in the
block. Blocks can be used to perform side-effects.

```{.oden language=oden}
x = {
  println("Calculating...")
  9 * 1000
}
// x will be 9000
```

Discarding the value of an expression, of any type other than `()`, causes an
error.

```{.oden language=oden}
x = {
  ()       // ok to discard as it's of type ()
  9 * 1000 // causes an error
  println("Done wasting CPU.")
}
```

## Let Bindings

The let expression binds identifiers for a set of expressions, used
in the body expression.

```{.oden language=oden}
let x = 1 in x + 2
```

Let supports sequential binding, which means that expressions can
use the identifiers of previous bindings.

```{.oden language=oden}
let x = 1
    y = 1 + x
    in y / 2
```

Shadowing names are not allowed and will result in a compile error.

```{.oden language=oden}
let x = 1
    x = x * 2 // not ok as x is already defined
    in x * 2
```

## Tuples

A tuple is an immutable finite ordered collection of values. Unlike slices, the
values in a tuple can have different types.

In the following program we create some tuple values representing people
by storing the name and age as pairs. As these tuples have the same type
we can store them in a slice that get's the type `[]{(string, int)}`.

```{.oden language=oden}
jessica = ("Jessica", 31)
frank = ("Frank", 26)

people = []{jessica, frank, ("Maxime", 25)}
```

## Slices

A slice is a collection of values typed only by the element type, not the
length of the collection.

```
names = []{"Sarah", "Joe", "Maxime"}

numbers : []{int}
numbers = []{1, 2, 3, 4, 5}
```

Slice elements can be accessed with square brackets:

```
greeting = "Hello, " ++ names[0]

twoLevelSlice = []{[]{1, 2, 3}, []{4, 5, 6}}
isSix = twoLevelSlice[1][2] == 6
```

## Records

Records are used to group related data into a composite structure. They are
quite similar to objects in Javascript, but are statically typed by their
fields. The order of fields in a record is not significant -- two records are
considered equal if they have the same set of fields with the same types.

To create a record you use curly brackets enclosing a comma-separated list of
field names and values to initialize the record value with.

```{.oden language=oden}
player = {
  health = 100,
  attack = {
    cooldown = 3,
    damage = 5
  },
  armor = 30
}
```

The type of the value `player` is:

```{.oden language=oden}
{
  health : int,
  attack : {
    cooldown : int,
    damage : int
  },
  armor: int
}
```

### Record Fields

Fields of records can be accessed using the dot operator.

```{.oden language=oden}
damagePerMinute(p) =
  p.attack.damage * 60 / p.attack.cooldown
```

A function definition like `damagePerMinute` gets inferred to take a value `p`
of the following type.

```{.oden language=oden}
{
  attack : {
    damage : int,
    cooldown : int
    | b
  }
  | a
}
```

Here `a` and `b` are *row variables*. But what is a row variable? And what
does the pipe character mean in a record type? Read on and we'll learn more
about them in the next section.


### Polymorphic Records and Row Variables

A record type can be polymorphic, i.e. it can allow extra unknown fields. This
is useful when you want to write a function that accepts some record value that
has a certain field, but you don't care if has extra fields or not. This can be
expressed in the type system using a *row varible*. The concept of *rows* is
the underlying construct on which records are built upon.

The following type signature says that `getName` takes any record with at least
the field `name` with type `string`. When instantiated with a concrete record
type the row variable `r` will be bound to a row contain all other fields.

```
getName : forall r. { name : string | r } -> string
```

For more information on these concepts, see @gaster1996polymorphic and
@leijen2005extensible.

## Protocols

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

```{.oden .playground-runnable language=oden}
package main

protocol Animal(a) {
  sound : a -> string
}

type Dog = { friendly : bool }
type Chicken = { female : bool }

impl Animal(Dog) {
  sound(dog) =
    if dog.friendly
      then "Woof!"
      else "Grrrrr..."
}

impl Animal(Chicken) {
  sound(chicken) =
    if chicken.female
      then "Cluck!"
      else "Cockadoodledoo!"
}

main() = {
  println(Animal::sound({ friendly = false }))
  println(Animal::sound({ female = true }))
}
```

#### Work In Progress

As Oden only has transparent type aliases right now it's not super clear in
this code that we create a Dog value and a Chicken value. Have patience, more
type constructs will arrive!

Also, as you might have noticed, methods have to be fully qualified with the
Protocol::Method syntax. This restriction will probably be lifted in the
future, enabling you to write only the method name if it can be unambigously
resolved.

### Overloading Operators

Protocols are used in Oden to provide *overloaded operators*. These are
built-in infix operators that are simple aliases for standard Protocols like
*Num*, *Equality* etc. As you can define implementations for a data type and
a Protocol, you can effectively overload any built-in operator for a custom
data type.

In the following example we implement the *Monoid* protocol for our *Point2D*
data type, which enables us to use the `++` operator to add vector values.

```{.oden .playground-runnable language=oden}
package main

// A point in 2D space.
type Point2D = { x: int, y: int }

// We implement the built-in Monoid protocol
// for Point2D.
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

// With the implementation in scope we can
// now use the ++ operator together with
// our Point2D type.
target : Point2D
target = position ++ distance

main() = {
  println(target)
}
```

### Compatibility with Go

The Protocols construct is not compatible with Go *interfaces* right now, but
that kind of support is planned.
