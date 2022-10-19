#!/bin/bash
# Compiler lib build script
set echo on

mkdir -p ../bin

# Get a list of all the .cpp files.
cppFileNames=$(find . -type f -name "*.cpp")

#echo "Files:" $cppFileNames

assembly="zodiac_lib"
compilerFlags="-g -fdeclspec -fPIC"
includeFlags="-Isrc"
linkerFlags=""
defines="-D_DEBUG -DZEXPORT"

echo "Building $assembly..."

clang++ -c $cppFileNames $compilerFlags -o ../bin/$assembly.o $defines $includeFlags $linkerFlags
ar r ../bin/$assembly.a ../bin/$assembly.o
