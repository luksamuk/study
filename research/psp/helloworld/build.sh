#!/bin/bash
mkdir -p build && cd build
psp-cmake -DBUILD_PRX=1 -DENC_PRX=1 ..
make
