# Installation

To run Oden you need Go installed and setup. Follow the instructions in
[How to Write Go Code](https://golang.org/doc/code.html).

## Binaries

Oden binaries are available at [Releases](https://github.com/oden-lang/oden/releases) on
GitHub. Choose the version, preferrably the latest, and download the ZIP file for your
operating system.

*There may be biniaries missing for some major operating systems.*

## Building From Source

### Prerequisites

* Make
* Racket 6.x
* Go
* *Node.js (only for building the documentation)*

### Getting the Code

Go to https://github.com/oden-lang/oden and `git clone` or download a ZIP file with
the source. ZIP files with source code are available in the
[Releases](https://github.com/oden-lang/oden/releases) as well.

### Build

When you have the source you can create a distribution of Oden using Make.

```bash
$ make dist
...
$ file target/odenc
target/odenc: Mach-O 64-bit executable x86_64
```

### Docs

This documentation can be built if you have NodeJS installed.

```bash
$ make docs
$ open target/oden/docs/index.html
```

To start a live-reloading server, run `make watch-docs` and open http://localhost:4000 in your
browser.

