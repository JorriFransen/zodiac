#!/bin/bash
# Compiler lib build script
set echo on

mkdir -p ../bin

# Get a list of all the .cpp files.
cppFileNames=$(find . -type f -name "*.cpp")

# Get an absolute path to the lib dir
libDir=$(realpath "../bin/")

#echo "Files:" $cppFileNames

assembly="zodiac"
compilerFlags="-g -fdeclspec -fPIC"
includeFlags="-Isrc -I../zodiac_lib/src"
linkerFlags="-L../bin/ -lzodiac_lib -Wl,-rpath,. -Wl,-rpath,$libDir"
defines="-D_DEBUG -DZIMPORT"

echo "Building $assembly..."
clang++ $cppFileNames $compilerFlags -o ../bin/$assembly $defines $includeFlags $linkerFlags