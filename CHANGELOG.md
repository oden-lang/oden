# Changelog

* 0.2.1
  - Type signatures. The type of a value can be specified, and constrained, by
    a type signature. Implicit or explicit forall ([Function
    documentation](http://oden-lang.org/user-guide/language-reference/forms.html#functions)).
  - Syntactic sugar for curried function types. `(a -> (b -> c))` can now be
    written `(a -> b -> c)`.
* 0.2.0
  - Compiler rewritten in Haskell.
  - Support for importing definitions from Go packages. All kinds not yet
    supported (e.g. interfaces, structs).
* 0.1.x - First versions of Oden (first called Kashmir) written in Racket.
