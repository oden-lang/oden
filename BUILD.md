# Build

Oden is built using Stack. A regular build requires nothing special.

```bash
stack setup
stack build
```

## Distribution

To build the Oden distribution we use Make.

```bash
make
```

```bash
$ make
$ file build/oden-0.3.0-alpha13-osx.tar.gz
dist/oden-0.3.0-alpha13-osx.tar.gz: gzip compressed data, ...
```

Because a dynamically linked library for Go imports needs an extended
`LD_LIBRARY_PATH` there's a wrapper script in the distribution. The executable
`oden-exe` that Stack produces should not be used directly. To add Oden to
your path do something like `export PATH=$PATH:<PATH-TO-ODEN-DISTRIBUTION>/bin`.

## Tests

```bash
$ make test
# or...
$ make watch-tests
# or run only tests matching a string
$ TEST_PATTERN="Oden.Type.Polymorphic" make watch-test
```
