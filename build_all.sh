#!/bin/bash

set echo on 

echo "Building everyting..."

make -f Makefile.zodiac_lib.linux.mak all
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

echo ""

make -f Makefile.zodiac_driver.linux.mak all
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

echo ""

make -f Makefile.zodiac_tests.linux.mak all
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

echo "All assemblies built successfully..."
