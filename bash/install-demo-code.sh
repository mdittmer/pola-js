#!/usr/bin/env bash

#
# Install code used to demonstrate "realistic use case".
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

BOOKSTORE_NPM_DIR="${POLA_JS_DIR}/aws-bookstore-demo-app/functions/APIs"

cd "${BOOKSTORE_NPM_DIR}"
npm install
