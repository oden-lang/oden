#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

stack exec runghc $DIR/IncludeCode.hs -- html
