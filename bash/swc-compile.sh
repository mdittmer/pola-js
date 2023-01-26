#!/usr/bin/env bash

#
# Compile swc.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

cd "${SWC_CLI_DIR}"
cargo build --release
