# Type Variables

Polymorphic types can contain *type variables*. These are like placeholders for
the specific types to be used later. A type variables is introduced to the
scope of the type signature by using the `forall` keyword, followed by one or
more type variable names and a terminating dot. This is called *universal
quantification*.

```
identity : forall a. a -> a
```

When applying a function to a value the function type is *instantiated* to
match the type of the value. If we apply the identity function to an int
literal that the instantiated type of `identity` will be `int -> int`. In
other words, all occurences of `a` are substituted for `int`. These
substitutions must match. `a -> a` cannot be instantiated to `int -> string`,
for example.
