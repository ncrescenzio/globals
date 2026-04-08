#!/bin/bash

builddir=build
installdir=../libglobals/

[[ -d "$builddir" ]] && rm -rf "$builddir"
[[ -d "$libglobals" ]] && rm -rf "$libglobals"

cmake -S . -B "$builddir" -DCMAKE_BUILD_TYPE="Release"
cmake --build "$builddir" -j $(nproc)
cmake --install "$builddir" --prefix "$installdir"


