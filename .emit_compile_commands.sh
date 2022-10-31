#! /usr/bin/env bash

# Emit compile_commands.json for clangd
. clean_all.sh
exec bear -- ./build_all.sh
