# Kashmir Programming Language - Draft

This is a draft for Kashmir programming language and a prototype compiler - a
statically typed LISP that compiles to native code using
[Go](https://golang.org/). By leveraging the Go runtime Kashmir gets
asynchronous IO, lightweight threads (Go routines) and a good GC.

## Goals

* A LISP **inspired by Scheme and Clojure** compiling to Go.
* Kashmir should feature a safe but versatile type system - more flexible than
the one in Go and at least as safe.
* The type system should offer type inference (Hindley-Milner style) that only
requires type annotations on top-level forms.
* The prototype compiler should be easy to change.
* Align with the built-in types of Go and provide simple interoperability.

## Secondary Goals
* The first versions might not include parametric polymorphism (generics) in
functions and types but the underlying type checker should be able to support
it without a big rewrite.
* Macros, not needed for first version.

## Non-goals (in first versions)

* Fast compiler. Emphasis lies on a simple implementation with easy-to-change
behaviour, not compiler speed.
* A "Go backend" for Scheme or Clojure.
* Easy workflow. After running the Kashmir compiler the user might have to step
in to a directory of output Go files and run `go build` etc.

## Experiments

Some samples and experiments of Kashmir source files and the output files to be
compiled with Go can be found in [`experiments/`](experiments). Note that
Kashmir sources use `.scm` for now to get LISP syntax highlighting.

## Usage

There's mostly tests for the moment, but also a simple REPL to try out stuff in.
Use `rlwrap` for a nicer experience. Make sure you have Go installed and setup
as described in [How to Write Go Code](https://golang.org/doc/code.html).

```bash
$ rlwrap racket repl.rkt
Welcome to Kashmir! Type CTRL-C to exit.
kashmir> (+ 1 2)
3 : int
kashmir> ((lambda (x) (+ 1 x)) 100)
101 : int
kashmir> (lambda () true)
0x2100 : (-> bool)
```

### Currently Supported Forms

```racket
;; literals
123
-588
true
false

;; arithmetic
(+ 1 2)
(- 1 2)
(* 2 2)
(/ 4 2)

;; comparison
(== 1 1)
(!= 1 2)

;; lambda
(lambda (x) x)
(lambda ([x : int]) x)

;; if
(if (== (+ 10 20) 30) 1 0)

;; let
(let ([x 1]) (+ x 2)
(let ([x 1]
      [y (+ 1 x)])
     (/ y 2))

;; function application
(let ([square (lambda (x) (* x 2))])
  (square 4))
;; no-arg function application
(let ([make-num (lambda () 3)])
  (* (make-num) (make-num)))
```
