#! /usr/bin/env bash

# Emit compile_commands.json for clangd
. clean_all.sh
exec bear -- make -f Makefile.linux.mak
