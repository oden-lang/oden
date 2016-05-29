# Compiling

In Oden we use the `oden` command line tool and the `build` subcommand to
compile sources. It compiles Oden source files to single file Go packages.

## Example

Here's a small Oden source that we can compile. It should to be located in
`src/hello.oden`.

```go
package hello/main

main() = println("Hello, world!")
```

We run `oden build` to compile all packages in the *Oden path*. The Oden path
is like a workspace which contains a `src` directory with Oden sources files
in it &mdash; it's the search path from which the Oden compiler tries to find
packages. It is very similar to the `GOPATH` from Go.

```bash
oden build --out-path=./out
```

As we specified the output directory with `--out-path` we now have Go sources
there. Extending the `GOPATH` with our out directory we can now compile the
executable program using a standard Go build command.

```
GOPATH=$PWD/out go build -o hello hello/main
./hello
Hello, world!
```
