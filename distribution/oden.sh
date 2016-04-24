#!/bin/sh

here="$(dirname "$(readlink -f "$0")")"

LD_LIBRARY_PATH="$here"/../lib:"$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH
exec "$0"-exe "$@"

