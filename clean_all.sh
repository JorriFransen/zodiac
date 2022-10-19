#!/bin/bash
set echo on

echo "Cleaning everything..."

make -f Makefile.zodiac_lib.linux.mak clean
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

make -f Makefile.zodiac_driver.linux.mak clean
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

make -f Makefile.zodiac_tests.linux.mak clean
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi

echo "All assemblies cleand successfully."