#!/bin/bash
set echo on

echo "Cleaning everything..."

make -f Makefile.linux.mak clean
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

echo "All assemblies cleaned successfully."
