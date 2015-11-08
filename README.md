# Kashmir Programming Language - Draft

This is a draft for Kashmir programming language and a prototype compiler - a
statically typed LISP that compiles to native code using
[Go](https://golang.org/). By leveraging the Go runtime Kashmir gets
asynchronous IO, lightweight threads (Go routines) and a good GC.

## Documentation

The more extensive documentation, including details about the project and
how to program in Kashmir, is located at http://owickstrom.github.io/kashmir/.

## Experiments

Some samples and experiments of Kashmir source files and the output files to be
compiled with Go can be found in [`experiments/`](experiments).

## Usage

There's mostly tests for the moment, but also a simple REPL to try out stuff in.
Use `rlwrap` for a nicer experience. Make sure you have Go installed and setup
as described in [How to Write Go Code](https://golang.org/doc/code.html).

```bash
$ rlwrap racket repl.rkt
Welcome to Kashmir! Type CTRL-C to exit.
kashmir> (+ 1 2)
3 : int
```

## License

Copyright 2015 Oskar Wickström

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
