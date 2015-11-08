# Installation

To run Kashmir you need Go installed and setup. Follow the instructions in
[How to Write Go Code](https://golang.org/doc/code.html) to get that setup.

## Binaries

Kashmir as available as [Releases](https://github.com/owickstrom/kashmir/releases) on
GitHub. Choose the version, preferrably the latest, and download the ZIP file for your
operating system.

*There may be biniaries missing for some major operating systems.*

## Building From Source

### Prerequisites

* Racket 6.x
* Go
* *Node.js (only for building the documentation)*

Go to https://github.com/owickstrom/kashmir and `git clone` or download a ZIP file with
the source. ZIP files are available in the [Releases](https://github.com/owickstrom/kashmir/releases)
as well. Then you can create a distribution of Kashmir using Make.

```bash
$ make dist
...
$ target/kmi
kashmir>
```
