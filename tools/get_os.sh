#!/bin/bash

case "$OSTYPE" in
  solaris*) echo "solaris" ;;
  darwin*)  echo "osx" ;; 
  linux*)   echo "linux" ;;
  bsd*)     echo "bsd" ;;
  msys*)    echo "windows" ;;
  *)        exit 1 ;;
esac
