#! /usr/bin/env bash

# Emit compile_commands.json for clangd
compiledb -n make -f Makefile.linux.mak zodiac_driver zodiac_tests
