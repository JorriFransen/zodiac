#! /usr/bin/env bash

if hash iwyu-tool
then
    iwyu-tool -o clang -p . -j $(nproc) -- -Xiwyu --verbose=1 | grep error
fi

exit 0
