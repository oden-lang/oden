# Operators

There are unary and binary operators in Oden.

```go
+y
-(1 - 2)
1 + 2
1 - 2
2 * 2
4 / 2
(100 - 50) / 2
100 * (50 + 25)
1 == 2
1 != 2
"Foo" ++ "Bar"
!x && (true || (1 == 2))
```

*Currently it is not possible to use these operators as first-class values,
passing them to functions or using them in a let, e.g. `map(+, numbers)`. One
could however wrap them in a standard function and pass that to a
higher-order function.*

