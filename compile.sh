#!/bin/bash

builddir=build
installdir=../libglobals/

[[ -d "$builddir" ]] && rm -rf "$builddir"

cmake -S . -B "$builddir"
cmake --build "$builddir"
cmake --install "$builddir" --prefix "$installdir"


