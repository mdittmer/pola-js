#!/usr/bin/env bash

#
# Compile TAJS.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

cd "${TAJS_DIR}"
ant -Ddebug=on compile
