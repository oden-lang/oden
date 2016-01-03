package importer

import (
	"go/importer"
	"go/types"
)

func tupleToSlice(tu *types.Tuple) []Type {
	ts := []Type{}
	for i := 0; i < tu.Len(); i++ {
		v := tu.At(i)
		ts = append(ts, encodeType(v.Type()))
	}
	return ts
}

func basicKindString(b *types.Basic) string {
	switch b.Kind() {

	case types.Invalid:
		return "invalid"

	case types.Bool:
		return "bool"
	case types.Int:
		return "int"
	case types.Int8:
		return "int8"
	case types.Int16:
		return "int16"
	case types.Int32:
		return "int32"
	case types.Int64:
		return "int64"
	case types.Uint:
		return "uint"
	case types.Uint8:
		return "uint8"
	case types.Uint16:
		return "uint16"
	case types.Uint32:
		return "uint32"
	case types.Uint64:
		return "uint64"
	case types.Uintptr:
		return "uintptr"
	case types.Float32:
		return "float32"
	case types.Float64:
		return "float64"
	case types.Complex64:
		return "complex64"
	case types.Complex128:
		return "complex128"
	case types.String:
		return "string"
	case types.UnsafePointer:
		return "unsafepointer"

		// types for untyped values
	case types.UntypedBool:
		return "bool"
	case types.UntypedInt:
		return "int"
	case types.UntypedRune:
		return "rune"
	case types.UntypedFloat:
		return "float"
	case types.UntypedComplex:
		return "complex"
	case types.UntypedString:
		return "string"
	case types.UntypedNil:
		return "nil"

	default:
		return "unsupported"
	}
}

func encodeType(t types.Type) Type {
	t = t.Underlying()
	switch t.(type) {
	case *types.Basic:
		b := t.(*types.Basic)
		untyped := (b.Info() & types.IsUntyped) != 0
		return NewBasic(basicKindString(b), untyped)
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

func GetPackageObjects(pkgName string) (objs []Object, err error) {
	pkg, err := importer.Default().Import(pkgName)
	if err != nil {
		return objs, err
	}

	for _, n := range pkg.Scope().Names() {
		obj := pkg.Scope().Lookup(n)
		if obj.Exported() {
			switch obj.(type) {
			case *types.Func:
				f := obj.(*types.Func)
				t := encodeType(f.Type())
				if err == nil {
					objs = append(objs, Object{f.Name(), "func", t})
				}
			case *types.Var:
				v := obj.(*types.Var)
				t := encodeType(v.Type())
				if err == nil {
					objs = append(objs, Object{v.Name(), "var", t})
				}
			case *types.Const:
				c := obj.(*types.Const)
				t := encodeType(c.Type())
				if err == nil {
					objs = append(objs, Object{c.Name(), "const", t})
				}
			}
		}
	}
	return objs, nil
}
