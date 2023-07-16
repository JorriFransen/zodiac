#! /usr/bin/env bash

IWYU_TOOL=""

if hash iwyu-tool 2> /dev/null
then
    IWYU_TOOL=iwyu-tool
fi

if hash iwyu_tool.py 2> /dev/null
then
    IWYU_TOOL=iwyu_tool.py
fi

$IWYU_TOOL -o clang -p . -j $(nproc) -- -Xiwyu --verbose=1 | grep 'error:'

exit 0
