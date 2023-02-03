#!/usr/bin/env bash

#
# Compile souffle.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

cd "${SOUFFLE_DIR}"
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DSOUFFLE_DOMAIN_64BIT=ON
cmake --build build -j72
