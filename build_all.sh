#!/bin/bash

set echo on 

echo "Building everyting..."

pushd zodiac_lib
source build.sh
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi
popd

pushd zodiac_driver
source build.sh
ERR=$?
if [ $ERR -ne 0 ]; then
    echo "Error:"$ERR && exit
fi
popd

echo "All assemblies built successfully..."
