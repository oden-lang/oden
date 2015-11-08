#!/bin/bash

case "$OSTYPE" in
  solaris*) echo "solaris" ;;
  darwin*)  echo "osx" ;; 
  linux*)   echo "linux" ;;
  bsd*)     echo "bsd" ;;
  *)        exit 1 ;;
esac
