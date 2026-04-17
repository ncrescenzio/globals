#!/bin/bash

builddir=build
installdir="$PWD/../libglobals/"

echo "BUILD DIR: $builddir"
echo "INSTALL DIR: $installdir"

[[ -d "$builddir" ]] && { rm -rf "$builddir"; echo "removing ${builddir}..."; }
[[ -d "$installdir" ]] && { rm -rf "$installdir"; echo "removing ${installdir}..."; }

cmake -S . -B "$builddir" -DCMAKE_BUILD_TYPE="Debug"
cmake --build "$builddir" -j $(nproc)
cmake --install "$builddir" --prefix "$installdir"
