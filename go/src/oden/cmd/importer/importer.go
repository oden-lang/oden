package main

import (
	"C"
	"encoding/json"
	"fmt"
	"os"

	"oden/importer"
)

type PackageResponse struct {
	Package *importer.Package `json:"package"`
}

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

func getPackageJSON(pkgName string) string {
	pkg, err := importer.GetPackage(pkgName)

	if err != nil {
		return respondWithError(err)
	}

	resp := PackageResponse{pkg}
	b, err := json.Marshal(resp)
	if err != nil {
		return respondWithError(err)
	}

	return string(b)
}

//export GetPackage
func GetPackage(name *C.char) *C.char {
	return C.CString(getPackageJSON(C.GoString(name)))
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

	switch cmd {
	case "print":
		pkg, err := importer.GetPackage(os.Args[2])
		if err != nil {
			die("%s\n", err)
		}
		fmt.Println(pkg)
	case "print-json":
		fmt.Println(getPackageJSON(os.Args[2]))
	}
}
