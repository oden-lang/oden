package main

import (
	"bytes"
	"fmt"
	"go/importer"
	"regexp"
)

func main() {
	// Type-check a package consisting of these files.
	// Type information for the imported "fmt" package
	// comes from $GOROOT/pkg/$GOOS_$GOOARCH/fmt.a.
	i := importer.Default()
	pkg, err := i.Import("fmt")

	if err != nil {
		panic(err)
	}
	var buf bytes.Buffer
	pkg.Scope().WriteTo(&buf, 0, true)
	rx := regexp.MustCompile(` 0x[a-fA-F0-9]*`)
	fmt.Println(rx.ReplaceAllString(buf.String(), ""))
}
