# Changelog

* 0.3.4
  * New User Guide documentation build using Pandoc
* 0.3.3
  * Oden source info (file name and line number) passed to Go compiler through
    compiler directive comment. This will cause stack traces to use the Oden
    source files names.
  * User guide added to this repository and published on tags and master branch
    build
* 0.3.2
  * Fix wrapper script issue with Oden playground.
* 0.3.1
  * Overloaded operators through protocols
  * New default type classes:
    * Num
    * Equality
    * Ordered
    * Logical
    * Monoid
  * Fixed bug causing duplicated implementations in scope
* 0.3.0
  * Protocols
  * New syntax
  * Records
  * The new `oden` CLI with sub commands.
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
