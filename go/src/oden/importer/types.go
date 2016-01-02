package main

type Type interface {
}

type Pointer struct {
	Kind  string `json:"kind"`
	Inner Type   `json:"inner"`
}

func NewPointer(inner Type) Pointer {
	return Pointer{"pointer", inner}
}

type Array struct {
	Kind   string `json:"kind"`
	Inner  Type   `json:"inner"`
	Lenght int64  `json:"length"`
}

func NewArray(inner Type, length int64) Array {
	return Array{"array", inner, length}
}

type Slice struct {
	Kind  string `json:"kind"`
	Inner Type   `json:"inner"`
}

func NewSlice(inner Type) Slice {
	return Slice{"slice", inner}
}

type Signature struct {
	Kind      string `json:"kind"`
	Recv      *Type  `json:"recv"`
	Arguments []Type `json:"arguments"`
	Returns   []Type `json:"returns"`
}

func NewSignature(recv *Type, arguments, returns []Type) Signature {
	return Signature{"signature", recv, arguments, returns}
}

type Basic struct {
	Kind string `json:"kind"`
	Name string `json:"name"`
}

func NewBasic(name string) Basic {
	return Basic{"basic", name}
}

type Named struct {
	Kind       string `json:"kind"`
	Pkg        string `json:"pkg"`
	Name       string `json:"name"`
	Underlying Type   `json:"underlying"`
	// Methods []Func `json:"methods"`
}

func NewNamed(pkg string, name string, underlying Type) Named {
	return Named{"named", pkg, name, underlying}
}

type Unsupported struct {
	Kind string `json:"kind"`
	Name string `json:"name"`
}

func NewUnsupported(name string) Unsupported {
	return Unsupported{"unsupported", name}
}

type Object struct {
	Name string `json:"name"`
	Type Type   `json:"type"`
}

type Scope struct {
	Objects []Object `json:"objects"`
}

type ErrorResponse struct {
	Error string `json:"error"`
}
