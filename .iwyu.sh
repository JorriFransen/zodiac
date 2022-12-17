#! /usr/bin/env bash


iwyu-tool -o clang -p . -j $(nproc) -- -Xiwyu --verbose=1 | grep error

