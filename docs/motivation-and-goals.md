## Motivation

The motivation behind Oden is to leverage the nice features of Go
&mdash; static linking, cross-compilation, goroutines, channels, an
increasing set of libraries &mdash; and on top of that introduce
higher-level abstractions together with an s-expression syntax to
support macros.

## Goals

These are the primary goals and tradeoffs as well as non-goals for the
first iterations of Oden. To be extra clear, *this is currently an
experimental language and it might change drastically*.

* A LISP-like language **inspired by Scheme and Clojure** compiling to
  Go. Not perhaps a "real LISP", at least not until macros are
  implemented.
* Oden should feature a safe but versatile type system - more
  flexible than the one in Go and at least as safe.
* Generics.
* The type system should offer heavy type inference. Possibly require
  type annotations on top-level forms.
* Align with the built-in types of Go and provide simple
  interoperability.
* The prototype compiler should be easy to change.

### Secondary Goals

* Macros, not needed for first version, but still a long-term goal.

### Shortcuts/Tradeoffs

The first versions of Oden should **not** focus on:

* A fast compiler. Emphasis lies on a simple implementation with
  correct semantics, not compilation speed.
* Easy workflow. After running the Oden compiler the user might
  have to step in to a directory of output Go files and run `go build`
  etc.
* Producing beautiful Go code.
* Easy Oden-to-Go interopability (calling Oden code from Go). It
  might turn out easy after all, but that is not an explicit goal.

### Non-goals

* A "Go backend" for Scheme or Clojure. There's no compatibility with
  any existing LISP, or any other language for that matter.
* A Haskell-clone.
