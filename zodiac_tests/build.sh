#!/bin/bash
# Compiler tests build script
set echo on

mkdir -p ../bin

# Get a list of all the .cpp files.
cppFileNames=$(find . -type f -name "*.cpp")

# Get an absolute path to the lib dir
libDir=$(realpath "../bin/")

#echo "Files:" $cppFileNames

assembly="zodiac_tests"
compilerFlags="-g -fdeclspec -fPIC"
includeFlags="-Isrc -Imunit -I../zodiac_lib/src"
linkerFlags="../bin/zodiac_lib.a"
defines="-D_DEBUG -DZIMPORT"

echo "Building $assembly..."
clang -c munit/munit/munit.c -o ../bin/munit.o
clang++ $cppFileNames ../bin/munit.o $compilerFlags -o ../bin/$assembly $defines $includeFlags $linkerFlags