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

func usage() {
	prg := os.Args[0]
	fmt.Fprintf(os.Stderr, `Print Oden-importable objects in Go packages.

Usage: %s CMD PKG

Commands:
  print         Print package objects using the Go string representation.
  print-json    Print package objects in JSON format.

Example:
  %s print-json net/http
`, prg, prg)
}

func die(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
	os.Exit(1)
}

func main() {
	if len(os.Args) < 3 {
		usage()
		os.Exit(1)
	}
	cmd := os.Args[1]

	if cmd != "print" && cmd != "print-json" {
		fmt.Fprintf(os.Stderr, "Invalid command: %s\n\n", cmd)
		usage()
		return
	}

	objs, err := importer.GetPackageObjects(os.Args[2])
	if err != nil {
		die("%s\n", err)
	}

	switch cmd {
	case "print":
		fmt.Println(objs)
	case "print-json":
		bs, err := json.Marshal(objs)
		if err != nil {
			die("%s\n", err)
		}
		fmt.Println(string(bs))
	}
}
