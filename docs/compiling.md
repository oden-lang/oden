# Compiling

In Kashmir the compiler is called `kmc`. It compiles Kashmir source
files to single file packages in Go. The following is a simple
example of how compiling with `kmc` works.

```bash
$ cat << EOF >> hello-world.km
(pkg main)
(import fmt)

(define main
  (lambda () (fmt.Println "Hello, world!")))
EOF
$ kmc hello-world.km hello_world.go
$ go build hello_world.go
$ ./hello_world
Hello, world!
```
