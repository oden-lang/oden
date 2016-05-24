# Tuples

A tuple is an immutable finite ordered collection of values. Unlike slices, the
values in a tuple can have different types.

```go
// lets first create some tuples representing people by storing the name and
// age
jessica = ("Jessica", 31)
frank = ("Frank", 26)

// as these tuples have the same type (string, int) we can store them in a
// slice that get's the type []{(string, int)}
people = []{jessica, frank, ("Maxime", 25)}
```

