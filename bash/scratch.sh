#!/usr/bin/env bash

#
# Scratch pad for trying things out.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

RELATIVE_SOURCE_FILE="aws-bookstore-demo-app/functions/APIs/addToCart.js"
BASE_OUT_DIR="${POLA_JS_DIR}/out/scratch"
BASE_LOG_DIR="${POLA_JS_DIR}/out/log/scratch"

SOURCE_FILE="${POLA_JS_DIR}/${RELATIVE_SOURCE_FILE}"
OUT_DIR="${BASE_OUT_DIR}/${RELATIVE_SOURCE_FILE}"
LOG_DIR="${BASE_LOG_DIR}/${RELATIVE_SOURCE_FILE}"

mkdir -p "${OUT_DIR}"
mkdir -p "${LOG_DIR}"

SWC_OUT_DIR="${OUT_DIR}/swc"
RESULTS_DIR="${OUT_DIR}/results"

mkdir -p "${SWC_OUT_DIR}"
mkdir -p "${RESULTS_DIR}"

time `# Time performing the following...` \
  "${SWC}" `# Run swc` \
  compile `# Compile source code` \
  --out-dir "${SWC_OUT_DIR}" `# All swc output files go here` \
  --source-maps true `# Generate source maps` \
  --config jsc.target=es3 `# Compile to ECMAScript 3` \
  "${SOURCE_FILE}" `# Compile this source file` \
  |& tee "${LOG_DIR}/swc.log"

time `# Time performing the following...` \
  java `# Run java` \
  -ea `# Enable assertions` \
  dk.brics.tajs.FlowGraphOnlyMain `# Run "generate flowgraph only" main class` \
  -out "${OUT_DIR}/tajs" `# Send TAJS output here` \
  "${SWC_OUT_DIR}/${SOURCE_FILE}" `# Generate from this source file` \
  |& tee "${LOG_DIR}/tajs.log"

time `# Time performing the following...` \
  "${SOUFFLE}" `# Run souffle` \
  -c `# Compile` \
  -j "${NUM_CPUS}" `# Jobs` \
  -F "${OUT_DIR}/tajs/flowgraphs/facts" `# Input facts` \
  -I "${POLA_JS_DIR}/dl/include" `# Datalog source files include directory` \
  "${POLA_JS_DIR}/dl/node-require-source-target.dl" `# Main datalog source file` \
  -D "${OUT_DIR}/results" `# Output directory` \
  `# -p "${OUT_DIR}/profile_c_j_72_no_frequency.prof" \ # Profiling` \
  |& tee "${LOG_DIR}/souffle.log" # Souffle output log
