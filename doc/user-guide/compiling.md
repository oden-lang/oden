# Compiling

In Oden the compiler is bundled in the `oden` command line tool inside the
`build` subcommand. It compiles Oden source files to single file packages in
Go. The following is a small example of how compiling with `oden build` works.


```bash
$ mkdir -p src/hello/
$ cat << EOF >> src/hello/main.oden
package hello/main

main() = println("Hello, world!")
EOF
$ oden build --out-path=./out
$ GOPATH=$PWD/out go build -o hello hello/main
$ ./hello
Hello, world!
```
