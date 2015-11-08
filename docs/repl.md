# REPL

In Kashmir the REPL is called `kmi`. There are no state and everything is run
through `go run` so it is not fast at all. However, it is quite useful for
experimenting with Kashmir.

```
$ rlwrap kashmir/bin/kmi
kashmir> (+ 1 2)
3 : int
kashmir> ((lambda (x) (+ 1 x)) 100)
101 : int
kashmir> (lambda () true)
0x2100 : (-> bool)
```
