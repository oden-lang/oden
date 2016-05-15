#!/bin/bash
BINARY=build/oden/bin/oden-exe

PCRE_PATH=$(otool -L $BINARY | grep pcre | cut -d"(" -f1 | cut -f2)
PCRE_DYLIB_NAME=$(basename $PCRE_PATH)

install_name_tool -change $PCRE_PATH $PCRE_DYLIB_NAME $BINARY

rm -f build/oden/lib/$PCRE_DYLIB_NAME
cp $PCRE_PATH build/oden/lib/$PCRE_DYLIB_NAME
