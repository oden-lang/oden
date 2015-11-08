# REPL

In Kashmir the REPL is called `kmi`. There is no state and everything is run
through `go run` so it's pretty slow. However, it is useful for
experimentation.

```
$ rlwrap kashmir/bin/kmi
kashmir> (+ 1 2)
3 : int
kashmir> ((lambda (x) (+ 1 x)) 100)
101 : int
kashmir> (lambda () true)
0x2100 : (-> bool)
```
