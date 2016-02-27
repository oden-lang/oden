# Build

To get the Cabal sandbox and dependencies, and build the library `oden`, run
the following:

```bash
$ make init-dev
$ make build
```

## Building the CLI on Linux

Due to some strange behaviour in Cabal on Linux regarding linking and
executables the CLI build is done in the Makefile. This means that only `oden`
the library can be built with Cabal, using `cabal build oden`, not `cabal
build`.

To get the `oden` executable run `make cli` and it will
be available in `dist/build-oden-cli/bin/oden`.

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
