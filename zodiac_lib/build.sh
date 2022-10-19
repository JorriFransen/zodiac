#!/bin/bash
# Compiler lib build script
set echo on

mkdir -p ../bin

# Get a list of all the .cpp files.
cppFileNames=$(find . -type f -name "*.cpp")

#echo "Files:" $cppFileNames

assembly="zodiac_lib"
compilerFlags="-g -shared -fdeclspec -fPIC"
includeFlags="-Isrc"
linkerFlags=""
defines="-D_DEBUG -DZEXPORT"

echo "Building $assembly..."

clang++ $cppFileNames $compilerFlags -o ../bin/lib$assembly.so $defines $includeFlags $linkerFlags
