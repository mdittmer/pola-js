#!/usr/bin/env bash

#
# Generate datalog facts that denote built-in NodeJS modules.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

NODE_VERSION="$(node --version)"
node "${POLA_JS_DIR}/dl/gen-builtin-module-facts.js" > "${POLA_JS_DIR}/dl/facts/nodejs-${NODE_VERSION}/coreNodeModule.facts"
