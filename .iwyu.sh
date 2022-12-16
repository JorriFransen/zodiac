#! /usr/bin/env bash


iwyu-tool -p . -j $(nproc) -- -Xiwyu --verbose=1

