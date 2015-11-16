# Kashmir Programming Language - Draft

This is a draft for Kashmir programming language and a prototype
compiler - a statically typed LISPy language that compiles to native
code using [Go](https://golang.org/). By leveraging the Go runtime
Kashmir gets asynchronous IO, lightweight threads (Go routines) and a
good GC.

Kashmir is named after the powerful Led Zeppelin song.

## Documentation

Most documentation, including details about the project, how to run
the compiler and how to program in Kashmir, is located at
https://owickstrom.github.io/kashmir/.

## Experiments

Some samples and experiments of Kashmir source files and the output files to be
compiled with Go can be found in [`experiments/`](experiments).

## Contributing

If you find this project interesting and want to help work on it, you
can submit bug reports, suggestions and change requests at
[the GitHub issues page](https://github.com/owickstrom/kashmir/issues).

Please start by posting an issue even if you have a pull request ready,
so we can begin with a discussion.

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
