package main

import (
	"C"
	"encoding/json"
	"fmt"
	"go/importer"
	"go/types"
	"os"
)

func tupleToSlice(tu *types.Tuple) []Type {
	ts := []Type{}
	for i := 0; i < tu.Len(); i++ {
		v := tu.At(i)
		ts = append(ts, encodeType(v.Type()))
	}
	return ts
}

func encodeType(t types.Type) Type {
	t = t.Underlying()
	switch t.(type) {
	case *types.Basic:
		b := t.(*types.Basic)
		return NewBasic(b.Name())
	case *types.Pointer:
		p := t.(*types.Pointer)
		pt := encodeType(p.Elem())
		return NewPointer(pt)
	case *types.Array:
		a := t.(*types.Array)
		at := encodeType(a.Elem())
		return NewArray(at, a.Len())
	case *types.Slice:
		s := t.(*types.Slice)
		st := encodeType(s.Elem())
		return NewSlice(st)
	case *types.Signature:
		sig := t.(*types.Signature)
		if sig.Variadic() {
			return NewUnsupported("Variadic functions")
		}
		v := sig.Recv()
		var vt *Type
		if v != nil {
			t := encodeType(v.Type())
			vt = &t
		}
		return NewSignature(
			vt,
			tupleToSlice(sig.Params()),
			tupleToSlice(sig.Results()))
	case *types.Named:
		n := t.(*types.Named)
		fmt.Println(n)
		return NewNamed(
			n.Obj().Pkg().Name(),
			n.Obj().Name(),
			n.Underlying())
	case *types.Interface:
		return NewUnsupported("Interfaces")
	case *types.Tuple:
		return NewUnsupported("Tuples")
	case *types.Map:
		return NewUnsupported("Maps")
	case *types.Chan:
		return NewUnsupported("Channels")
	case *types.Struct:
		return NewUnsupported("Structs")
	default:
		return NewUnsupported(t.String())
	}
}

func respondWithError(err error) string {
	b, err := json.Marshal(ErrorResponse{err.Error()})
	if err != nil {
		return "{\"error\": \"Failed to encode error response as JSON\"}"
	}
	return string(b)
}

func getPackageObjects(name string) string {
	pkg, err := importer.Default().Import(name)
	if err != nil {
		// TODO: error handling back to Haskell
		return respondWithError(err)
	}

	objs := []Object{}
	for _, n := range pkg.Scope().Names() {
		obj := pkg.Scope().Lookup(n)
		if obj.Exported() {
			switch obj.(type) {
			case *types.Func:
				f := obj.(*types.Func)
				t := encodeType(f.Type())
				if err == nil {
					objs = append(objs, Object{f.Name(), t})
				}
			case *types.Var:
			case *types.Const:
			}
		}
	}

	b, err := json.Marshal(Scope{objs})
	if err != nil {
		// TODO: error handling back to Haskell
		return respondWithError(err)
	}

	return string(b)
}

//export GetPackageObjects
func GetPackageObjects(name *C.char) *C.char {
	return C.CString(getPackageObjects(C.GoString(name)))
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: scope <pkg-name>")
		os.Exit(1)
	}
	fmt.Println(getPackageObjects(os.Args[1]))
}
