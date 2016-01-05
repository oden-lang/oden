# Build

To get the Cabal sandbox and dependencies, and build the library `oden`, run
the following:

```bash
$ make init-dev
$ make build
```

## Building `odenc`

Due to some strange behaviour in Cabal regarding linking and executables the
build for `odenc` is done in the Makefile. This means that only `oden` the
library can be built with Cabal, using `cabal build oden`, not `cabal build
odenc`.

To get the `odenc` executable run `make odenc` and it will
be available in `dist/build-odenc/bin/odenc`.

## Distribution

```bash
$ make dist
$ file dist/oden-0.2.0-osx.tar.gz
dist/oden-0.2.0-osx.tar.gz: gzip compressed data, ...
```

## Tests

```bash
$ make test
# or...
$ make watch-tests
```
