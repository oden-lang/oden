# Let Bindings

The let expression binds identifiers for a set of expressions, used
in the body expression.

```go
let x = 1 in x + 2
```

Let supports sequential binding, which means that expressions can
use the identifiers of previous bindings.

```go
let x = 1
    y = 1 + x
    in y / 2
```

Shadowing names are not allowed and will result in a compile error.

```go
let x = 1
    x = x * 2 // not ok as x is already defined
    in x * 2
```
