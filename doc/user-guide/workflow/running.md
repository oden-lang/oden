# Running Programs Directly

If you just want to run an Oden main package without going through the hassle
of compiling separately, you can use the `run` subcommand.

Here's our simple `hello.oden` program again.

```go
package hello/main

main() = println("Hello, world!")
```

We can run it directly like this:

```bash
oden run hello.oden
Hello, world!
```
