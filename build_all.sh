#!/bin/bash

set echo on

echo "Building everyting..."

make -f Makefile.linux.mak all -j $(nproc)
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit 1
fi

echo "All assemblies built successfully..."
