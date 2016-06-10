# Getting Started

## Installation

To run Oden you need Go installed and setup. Follow the instructions in
[_How to Write Go Code_](https://golang.org/doc/code.html).

### Binaries

Oden binaries are available at
[Releases](https://github.com/oden-lang/oden/releases) on GitHub. Choose the
version, preferrably the latest, and download the archive for your operating
system.

*There may be biniaries missing for some major operating systems.*

### Building From Source

Go to [Oden at GitHub](https://github.com/oden-lang/oden) and `git clone` or
download a ZIP file with the source. ZIP files with source code are
available in the releases page as well.

When you have the source you can create a distribution of Oden by following the
instructions in the `BUILD.md` file in the root of the source code tree.

## The Oden Command

The Oden distribution comes with a command line tool called `oden`. Use the
`--help` flag to see the usage message, listing available subcommands.

```{.hidden}
$ @\textbf{oden -\--help}@
Usage: oden command [options]

Commands:

  build            compile all source files in the Oden path
  run              run a specific Oden file as main
  lint             check an Oden file for errors and warnings
  print-inferred   infer types and print the package in the given file
  print-compiled   compile and print the package in the given file

Options:

  -h            --help             Print this help message
  -V            --version          Print the CLI version
  -W[all|none]  --warn[=all|none]  Enable or disable compiler warnings
  -p[DIR]       --oden-path[=DIR]  Search path for Oden sources
  -o[DIR]       --out-path[=DIR]   GOPATH output directory
  -M            --monochrome       Print without colors
```

## Compiling

We use the `build` subcommand to compile all sources in the *Oden path*.
For the following example we use the classic Hello World program written in
Oden. It should be located in `src/hello.oden`.

```{.oden language=oden}
package hello/main

main() = println("Hello, world!")
```

The Oden path is a search path of semicolon-separated paths from,
defaulting to `"."`, which the Oden compiler tries to find packages.
Each path should have `src` directory inside of it.  The Oden path is
very similar to the `GOPATH` from Go.

```{.hidden}
$ @\textbf{oden build -\--out-path=./out}@
```

<pre><code>$ <strong>oden build --out-path=./out</strong></code></pre>

The compiler writes Go source files to `target/go` by default. By extending
the `GOPATH` to include the output directory path, we can compile the
executable program using a standard Go build command.

```{.hidden}
$ @\textbf{GOPATH=\$PWD/target/go:\$GOPATH go build -o hello hello/main}@
$ @\textbf{./hello}@
Hello, world!
```
<pre><code>$ <strong>GOPATH=$PWD/target/go:$GOPATH go build -o hello hello/main</strong>
$ <strong>./hello</strong>
Hello, world!</code></pre>

## Running Programs Directly

If you just want to run an Oden main package without going through the hassle
of compiling separately, you can use the `run` subcommand.

Remember our Hello World program from before? If we save it in a file called
`hello.oden` we can run it directly like this:

```{.hidden}
$ @\textbf{oden run hello.oden}@
Hello, world!
```
<pre><code>$ <strong>oden run hello.oden</strong>
Hello, world!</code></pre>

When running an Oden file directly it can be located outside the Oden path.
Only packages imported by the main package will be resolved from the path.

## Playground

To try out stuff in Oden and to share ideas, be sure to check out [the
Playground](https://playground.oden-lang.org). It is an online application
where you can write and run Oden programs in your web browser.
