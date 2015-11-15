# Compiling

In Kashmir the compiler is called `kmc`. It compiles Kashmir source
files to single file packages in Go. The following is a simple
example of how compiling with `kmc` works.

```bash
$ mkdir -p src/hello-world/
$ cat << EOF >> src/hello-world/main.km
(pkg hello-world/main)
(import fmt)

(define (main)
  (fmt.Println "Hello, world!"))
EOF  
$ kmc ./out
$ GOPATH=$PWD/out go build -o hello-world hello-world/main
$ ./hello_world
Hello, world!
```
