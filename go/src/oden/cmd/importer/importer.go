package main

import (
	"C"
	"encoding/json"
	"fmt"
	"os"

	"oden/importer"
)

type ErrorResponse struct {
	Error string `json:"error"`
}

func respondWithError(err error) string {
	b, err := json.Marshal(ErrorResponse{err.Error()})
	if err != nil {
		return "{\"error\": \"Failed to encode error response as JSON\"}"
	}
	return string(b)
}

func getPackageObjectsJSON(pkgName string) string {
	objs, err := importer.GetPackageObjects(pkgName)

	if err != nil {
		return respondWithError(err)
	}

	b, err := json.Marshal(importer.Scope{objs})
	if err != nil {
		return respondWithError(err)
	}

	return string(b)
}

//export GetPackageObjects
func GetPackageObjects(name *C.char) *C.char {
	return C.CString(getPackageObjectsJSON(C.GoString(name)))
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: scope <pkg-name>")
		os.Exit(1)
	}
	objs, err := importer.GetPackageObjects(os.Args[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
	} else {
		fmt.Println(objs)
	}
}
