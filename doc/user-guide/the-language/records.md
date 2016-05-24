# Records

Records are used to group related data into a composite structure. They are
quite similar to objects in Javascript, but are statically typed by their
fields. The order of fields in a record is not significant - two records are
considered equal if they have the same set of fields with the same types.

To create a record you use curly brackets enclosing a comma-separated list of
field names and values to initialize the record value with.

```go
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

```go
{ health : int, attack : { cooldown : int, damage : int }, armor: int }
```

## Record Fields

Fields of records can be accessed using the dot operator.

```go
damagePerMinute(p) = p.attack.damage * 60 / p.attack.cooldown
```

A function definition like `damagePerMinute` gets inferred to take a value `p`
of type `{ attack : { damage : int, cooldown : int | b } | a }` where `a` and
`b` are *row variables*.

## Polymorphic Records and Row Variables

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

For more information on these concepts, see the work that Oden records is
based on:

* *A Polymorphic Type System for Extensible Records and Variants*, Benedict R.
  Gaster and Mark P. Jones ([PDF](http://www.cs.cmu.edu/~aldrich/courses/819/papers/row-poly.pdf))
* *Extensible records with scoped labels*, Daan Leijen ([PDF](http://research.microsoft.com/pubs/65409/scopedlabels.pdf))
