#!/usr/bin/env bash

#
# Compile TAJS.
#

source $(dirname "${BASH_SOURCE[0]}")/env.sh

cd "${TAJS_DIR}"
ant -Ddebug=on compile
