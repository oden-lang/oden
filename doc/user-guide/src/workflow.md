# Workflow

## Writing Oden Code

In Oden the following rules apply:

* Every source file corresponds to a single package.
* Source files must begin with the `package` declaration. The declaration
  specifies the complete qualified name, e.g. `package a/b/c`.
* Source files may use zero or more `import` declarations after the `package`
  declaration.
* Source files may, after `package` and any `import` declarations, define zero
  or more functions and values.
* Functions must be defined before first use (this restriction will be lifted
  as soon as possible).
* The `main` function must be a function taking no argument and returning
`()`, i.e. have the type `(-> ())`.

```{.oden language=oden caption=Package\ with\ imports\ and\ definitions}
package main

import strconv

// function definitions can use the function 
// definition shorthand and type signatures
shout : string -> string
shout(s) = s ++ "!"

// value definition without signature
result = shout(strconv.Atoi(9000))

main() = println(result)
```

## Compiling

In Oden we use the `oden` command line tool and the `build` subcommand to
compile sources. It compiles Oden source files to single file Go packages.

Here's a small Oden source that we can compile. It should to be located in
`src/hello.oden`.

```{.oden language=oden caption=\"Hello\ world\"\ in\ Oden}
package hello/main

main() = println("Hello, world!")
```

We run `oden build` to compile all packages in the *Oden path*. The Oden path
is a search path of semicolon-separated paths from which the Oden compiler
tries to find packages. Each path should have `src` directory inside of it.
The Oden path is very similar to the `GOPATH` from Go.

```bash
oden build --out-path=./out
```

As we specified the output directory with `--out-path` we now have Go sources
there. Extending the `GOPATH` with our out directory we can now compile the
executable program using a standard Go build command.

```bash
GOPATH=$PWD/out go build -o hello hello/main
./hello
Hello, world!
```

## Running Programs Directly

If you just want to run an Oden main package without going through the hassle
of compiling separately, you can use the `run` subcommand.

Remember our *Hello World* program from before? If we save it in a file called
`hello.oden` we can run it directly like this:

```{.bash language=bash}
oden run hello.oden
Hello, world!
```

### Playground

To try out stuff in Oden and to share ideas, be sure to check out
[the Playground](https://playground.oden-lang.org).
