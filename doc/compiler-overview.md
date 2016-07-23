# Compiler Overview

This document is intended as an overview of the compiler -- the key components,
those components' responsibilities, and brief explanations on how they are
implemented.

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

TODO.

## Instantiate

TODO.

## Go Importer

TODO.

## Go Backend

TODO.

## CLI

TODO.
