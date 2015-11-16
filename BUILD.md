# Build

Oden is built using Make.

## OSX/Linux

*(Linux not tested yet...)*

**You will need:**

* Racket
* Go
* Make

```bash
$ make dist
...
$ file target/odenc
target/odenc: Mach-O 64-bit executable x86_64
```

You will also get the distribution archive in
`target/oden-<version>-<os>.tar.gz`.

## Windows 

**You will need:**

* Racket (in $PATH)
* Go (in $PATH)
* Cygwin or Git Bash
* [GNU Make for Windows](http://gnuwin32.sourceforge.net/packages/make.htm)

```bash
$ make dist
...
$ file target/odenc.exe
target/odenc.exe: PE32+ executable (console) x86-64, for MS Windows
```

You will also get the distribution archive in
`target/oden-<version>-windows.tar.gz`.

