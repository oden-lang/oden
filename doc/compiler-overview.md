# Compiler Overview

This document is intended as an overview of the compiler &mdash; the key
components, those components' responsibilities, and brief explanations on
how they are implemented.

## Abbreviations

<dl>
  <dt>AST</dt>
  <dd>Abstract Syntax Tree</dd>
  <dt>IR</dt>
  <dd>Intermediate Representation</dd>
</dl>

## Key Components

* [Core](#core)
* [Lexer/Parser and Syntax](#lexer-parser-and-syntax)
* [Desugar](#desugar)
* [Untyped Validation](#untyped-validation)
* [Infer](#infer)
* [Substitution](#substitution)
* [Typed Validation](#typed-validation)
* [Resolution](#resolution)
* [Monomorphization](#monomorphization)
* [Instantiate](#instantiate)
* [Go Importer](go-importer)
* [Go Backend](go-backend)
* [CLI](cli)

## Core

The modules in `Oden.Core` are central, small parts that are used in many of
the other components. These are things like the shared `Oden.Core.Expr` data
structure, or `Oden.Core.Definition` which models a top-level definition in a
package.

*Note: More modules should probably be lifted into Core, like Identifier,
QualifiedName, etc.*

## Lexer/Parser and Syntax

The two modules `Oden.Lexer` and `Oden.Parser` transforms textual Oden source
code into the data structures in `Oden.Syntax`. These data structures directly
corresponds to what the source code looks like.

## Desugar

Given the `Oden.Syntax` data structures, `Oden.Desugar` _desugars_ them into
the corresponding _Untyped_ IR. Desugaring means transforming syntactic sugar
forms in into more basic forms.

One example is how you can write a function shorthand at the top-level that
seems to take multiple arguments, but it gets transformed to nested
single-argument functions. Another one is operators being desugar into protocol
methods. The following example shows both those transformations.

    plus(x, y, z) = x + y + z
    // becomes...
    plus = (x) -> (y) -> (z) -> Num::Add(Num::Add(x, y), z)

## Untyped Validation

`Oden.Compiler.Validation.Untyped` operates on the untyped IR and checks for
redefinition/shadowing of names, as well as duplicate field names in records.
In general, this validation can catch simple errors early, without having to
know anything about the types.

_Note: There are a lot more of validations that should be added to this
module._

## Infer

`Oden.Infer` takes the untyped IR, with imported packages resolved, and infers
type information for all definitions in a package.

The inference module is based on the type inferencer from [Write You A
Haskell](http://dev.stephendiehl.com/fun/). To understand how it works, start
by reading [Chapter 7](http://dev.stephendiehl.com/fun/006_hindley_milner.html)
in that book.

Some changes and extensions to the Oden inferencer are:

* In general, it handles more cases as the type system and IRs are more
  extensive in Oden than in the ML-based language in the book.
* It respects explicit type annotations and uses the type variables specified
  by the user, if available.
* It wraps foreign functions (from Go) to make them curried.
* The _Substitution_ class has been separated out as it's used by other modules
  in Oden.

## Substitution

This module provides a type class `Oden.Substitution.Substitutable` that
can be implemented by data structures in which type variables can be
substituted for other types (possibly also type variables).

It also provides a set of functions for creating and composing substitutions,
which are mappings from type variables to types.

The module implements the Substitutable class for the IR data structures in
`Oden.Core`, allowing other modules, such as `Oden.Infer` and
`Oden.Compiler.Instantiate`, to do type variable substitution without having to
know about all the data structures and how to traverse them.

## Typed Validation

`Oden.Compiler.Validation.Typed` is very similar to [Untyped
Validation](#untyped-validation), but it operates on the Typed IR. It checks
for invalid indices in subscripts, implicitly discarded values, and unused
imports.

## Resolution

`Oden.Compiler.Resolution` resolves protocol method references. The Typed
IR has a data structure with the two constructors `Unresolved` and
`Resolved`:

    data TypedMethodReference
      = Unresolved ProtocolName MethodName ProtocolConstraint
      | Resolved ProtocolName MethodName (MethodImplementation TypedExpr)
      deriving (Show, Eq, Ord)

This module's responsibility is to find a single matching implementation and
transform from Unresolved to Resolved. The system has something called
_protocol constraints_ which can be wrapping any type to express that some
protocol method reference needs to be resolved. The resolution module only
resolves method references for constraints on non-type variable types.

If there are no, or multiple, matching implementations for a protocol method
reference in scope, an error is thrown.

_Note: Protocol methods in let bindings are not supported yet._

## Monomorphization

`Oden.Compiler.Monomorphization` takes the Typed IR and and specializes
all uses of polymorphic functions into monomorphic functions. For example,
if we have a function `identity` with type `x -> x` and we apply it in our
main function with an integer literal, the Monomorphed IR will only contain
a function of type `int -> int`.

Imported packagse and all names are *flattened* in the monomorphization phase,
resulting in a single package with names like `__some_pkg__a_definition_name`.
Specialized polymorphic functions, called *instances* in the compiler, get
their specific types suffixed to their names.

The monomorphization also works with let bindings, so a let bound polymorphic
definition might expand to multiple let bound monomorphic functions based on
its usage.

All protocol method references must be resolved in this phase, or an error will
be thrown. The monomorphization instantiates the resolved method
implementations as regular polymorphic functions.

To inspect the result of monomorphization you can use the subcommand
`print-compiled`:

    $ oden print-compiled regression-test/src/protocols/error.oden
    package protocols/error/main

    import foreign strconv

    __error_method_Error_inst_int_to_string : int -> string
    __error_method_Error_inst_int_to_string(code) = (+)("E")(((_g0) -> strconv.Itoa(_g0))(code))

    __error_method_Error_inst_record__row__error___string___to_string : {error: string} -> string
    __error_method_Error_inst_record__row__error___string___to_string(e) = (+)(e.error)(" (no message)")
    ...

## Instantiate

This module has one capabililty &mdash; instantiating a given
polymorphically typed expression to a specific type, e.g. instantiating an
expression with type `a -> a` with the type `int -> int` will result in an
expression of type `int -> int`.

This module is used by [Resolution](#resolution) and
[Monomorphization](#monomorphization).

## Go Importer

`Oden.Go.Importer`, together with the library `libimporter`, is responsible
for resolving imports of foreign packages from Go. Given an *import path* it
tries to find the corresponding package, and if found, makes the members
usable from Oden.

Only the simplest Go constructs are available for import into Oden, as of
writing. Interfaces and other Go features cannot be handled automatically yet.

The library `libimporter` is implemented in Go and exports a C compatible
function that can be used to query Go packages. It returns a JSON string of
the complete package representation, or the error message if failed.

`Oden.Go.Importer` interfaces with `libimporter` using Haskell FFI and the
Aeson JSON library to build an Oden-compatible package structure from the JSON
string. The Oden type system has special types for foreign functions as
they are treated differently.

The build of `libimporter` is done using a custom Cabal setup. Have a look at
[Setup.hs](../Setup.hs) to see the details.

## Go Backend

The only implemented backend as of writing, `Oden.Backend.Go` generates a
`Oden.Go.AST` structure out of the Monomorphed IR. It has some special cases
for foreign constructs, like operators and how to encode identifiers.

The AST is then pretty printed and written as Go sources files. In other words,
the Oden compiler is technically a transpiler to Go source code. There is no
knowledge in the Oden compiler of calling conventions and the runtime system
of Go. The interface is Go source code and the functionality provided by
`libimporter`.

## CLI

The `cli` subdirectory contains modules for the Oden command-line application.
It parses flags and parameters and provides a number of subcommands that can
be used to lint, build and run Oden code, as well as to introspect the
various IRs between the compiler phases.
