# Unit

*Unit*, written `()`, is a built-in type that has only one value. It is mostly
used for functions that causes side effects and have no useful return value.

Unit is also used for interoperability with functions in Go that has no
return value at all. For example, the Go expression `func (s string) {
fmt.Println(s) }` would have the type `string -> ()` in Oden.

The literal `()` has the type `()` and is the only value for that type.
