#!/usr/bin/env bash

#
# Scratch pad for trying things out.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

cd "${POLA_JS_DIR}"

trap 'jobs -p | xargs -r kill' EXIT

SOURCE_DIR="aws-bookstore-demo-app/functions/APIs"
SOURCE_FILE="aws-bookstore-demo-app/functions/APIs/addToCart.js"
OUT_DIR="out/scratch"

mkdir -p "${OUT_DIR}"

AE_MANIFESTS_DIR="${OUT_DIR}/ae-manifests"

mkdir -p "${AE_MANIFESTS_DIR}"

SWC_OUT_DIR="${OUT_DIR}/swc"

mkdir -p "${SWC_OUT_DIR}"

SWC_RESULTS_OUT_DIR="${SWC_OUT_DIR}/results"

mkdir -p "${SWC_RESULTS_OUT_DIR}"

find "${SOURCE_DIR}" -type f | ag '\.(js|jsx|es6|es|mjs|ts|tsx)$' | sort | uniq > "${AE_MANIFESTS_DIR}/swc-inputs"

cat "${AE_MANIFESTS_DIR}/swc-inputs" | sed 's/\.\(js\|jsx\|es6\|es\|mjs\|ts\|tsx\)$/.../g' | sort | uniq > "${AE_MANIFESTS_DIR}/swc-ellipsis"

cat "${AE_MANIFESTS_DIR}/swc-ellipsis" | sed 's/\.\.\.$/.js/g' | sort | uniq > "${AE_MANIFESTS_DIR}/swc-js-outputs"

cat "${AE_MANIFESTS_DIR}/swc-ellipsis" | sed 's/\.\.\.$/.js.map/g' | sort | uniq > "${AE_MANIFESTS_DIR}/swc-map-outputs"

cat "${AE_MANIFESTS_DIR}/swc-js-outputs" "${AE_MANIFESTS_DIR}/swc-map-outputs" | sort | uniq > "${AE_MANIFESTS_DIR}/swc-outputs"

echo "compile
--out-dir
${SWC_RESULTS_OUT_DIR}
--source-maps
true
--config
jsc.target=es3
--ignore
aws-bookstore-demo-app/functions/APIs/node_modules/**/test/shams/*.js
${SOURCE_DIR}" > "${AE_MANIFESTS_DIR}/swc-args"

echo "" > "${AE_MANIFESTS_DIR}/swc-env"

"${ARTIFACT_EXECUTOR}" \
  exec \
    -e "${AE_MANIFESTS_DIR}/swc-env" \
    -p "${SWC}" \
    -a "${AE_MANIFESTS_DIR}/swc-args" \
    -i "${AE_MANIFESTS_DIR}/swc-inputs" \
    -o "${AE_MANIFESTS_DIR}/swc-outputs"

# TIME_OUT_DIR="${OUT_DIR}/time"
# SWC_OUT_DIR="${OUT_DIR}/swc"
# TAJS_OUT_DIR="${OUT_DIR}/tajs"
# SOUFFLE_OUT_DIR="${OUT_DIR}/souffle"

# mkdir -p "${TIME_OUT_DIR}"
# mkdir -p "${SWC_OUT_DIR}"
# mkdir -p "${TAJS_OUT_DIR}"
# mkdir -p "${SOUFFLE_OUT_DIR}"

# SWC_RESULTS_OUT_DIR="${SWC_OUT_DIR}/results"

# mkdir -p "${SWC_RESULTS_OUT_DIR}"

# SEMAPHORIC_DIR=$(mktemp -d)
# SEMAPHORIC_ID="scratch"
# SEMAPHORIC_MAX_PROCS=$(expr ${NUM_CPUS} / 4)
# SEMAPHORIC_POLL_SECS=1

# function parallel () {
#   "${SEMAPHORIC}" \
#     --max-procs="${SEMAPHORIC_MAX_PROCS}" \
#     --sem-dir="${SEMAPHORIC_DIR}" \
#     --poll-secs="${SEMAPHORIC_POLL_SECS}" \
#     "${SEMAPHORIC_ID}" \
#     "$*" &
# }

# function mem_available_percent () {
#   local -n map_output_value="$1"
#   map_output_value=$(("100 * $(cat /proc/meminfo | grep 'MemAvailable' | grep -o '[0-9]\+') / $(cat /proc/meminfo | grep 'MemTotal' | grep -o '[0-9]\+')"))
# }

# while read -r JSON_FILE; do
#   SRC="${JSON_FILE}"
#   DST="$(echo "${JSON_FILE}" | sed -e 's/\/swc\//\/souffle\//g' -e 's/\.\//..\/2023-01-27_pola-js_scratch_data_collection\//g')"
#   mkdir -p "$(dirname ${DST})"
#   cp "${SRC}" "${DST}"
# done < <(find ./out/scratch/swc/results -type f | ag '\.json$')

# /usr/bin/env time `# Time performing the following...` \
#   -o "${TIME_OUT_DIR}/swc.time" `# Put timing output here` \
#     "${SWC}" `# Run swc` \
#       compile `# Compile source code` \
#       --out-dir "${SWC_RESULTS_OUT_DIR}" `# All swc output files go here` \
#       --source-maps true `# Generate source maps` \
#       --config jsc.target=es3 `# Compile to ECMAScript 3` \
#       --ignore 'aws-bookstore-demo-app/functions/APIs/node_modules/**/test/shams/*.js' `# Ignore malformed test-only JavaScript` \
#       "${SOURCE_DIR}" `# Compile this source file` \
#       |& tee "${SWC_OUT_DIR}/swc.log" # Put SWC logging output here

# while read -r JSON_FILE; do
#   mkdir -p "${SWC_RESULTS_OUT_DIR}/$(dirname "${JSON_FILE}")"
#   cp "${JSON_FILE}" "${SWC_RESULTS_OUT_DIR}/${JSON_FILE}"
# done < <(find "${SOURCE_DIR}" | grep '\.json$')

# while read -r SOURCE_FILE; do
#   THIS_TIME_OUT_DIR="${TIME_OUT_DIR}/${SOURCE_FILE}"
#   THIS_TAJS_OUT_DIR="${TAJS_OUT_DIR}/${SOURCE_FILE}"
#   THIS_SOUFFLE_OUT_DIR="${SOUFFLE_OUT_DIR}/${SOURCE_FILE}"

#   mkdir -p "${THIS_TIME_OUT_DIR}"
#   mkdir -p "${THIS_TAJS_OUT_DIR}"
#   mkdir -p "${THIS_SOUFFLE_OUT_DIR}"

#   THIS_TAJS_RESULTS_DIR="${THIS_TAJS_OUT_DIR}/results"
#   THIS_SOUFFLE_RESULTS_DIR="${THIS_SOUFFLE_OUT_DIR}/results"

#   mkdir -p "${THIS_TAJS_RESULTS_DIR}"
#   mkdir -p "${THIS_SOUFFLE_RESULTS_DIR}"

#   parallel `# Run in parallel` \
#     "(
#       /usr/bin/env time `# Time performing the following...` \
#         -o \"${THIS_TIME_OUT_DIR}/tajs.time\" `# Put timing output here` \
#           java `# Run java` \
#             -ea `# Enable assertions` \
#             dk.brics.tajs.FlowGraphOnlyMain `# Run "generate flowgraph only" main class` \
#             -out \"${THIS_TAJS_RESULTS_DIR}\" `# Send TAJS output here` \
#             \"${SOURCE_FILE}\" `# Generate from this source file` \
#             |& tee \"${THIS_TAJS_OUT_DIR}/tajs.log\" `# TAJS output log` && \
#       /usr/bin/env time `# Time performing the following...` \
#         -o \"${THIS_TIME_OUT_DIR}/souffle.time\" `# Put timing output here` \
#           \"${SOUFFLE}\" `# Run souffle` \
#           -c `# Compile` \
#           -j \"${NUM_CPUS}\" `# Jobs` \
#           -F \"${THIS_TAJS_RESULTS_DIR}/flowgraphs/facts\" `# Input facts` \
#           -I \"${POLA_JS_DIR}/dl/include\" `# Datalog source files include directory` \
#           \"${POLA_JS_DIR}/dl/node-require-source-target.dl\" `# Main datalog source file` \
#           -D \"${THIS_SOUFFLE_RESULTS_DIR}\" `# Output directory` \
#           `# -p \"${OUT_DIR}/profile_c_j_72_no_frequency.prof\" \ # Profiling` \
#           |& tee \"${THIS_SOUFFLE_OUT_DIR}/souffle.log\" `# Souffle output log`
#     )"
# done < <(find "${SWC_RESULTS_OUT_DIR}" -type f | grep '\.js$')

# time `# Time performing the following...` \
#   java `# Run java` \
#   -ea `# Enable assertions` \
#   dk.brics.tajs.FlowGraphOnlyMain `# Run "generate flowgraph only" main class` \
#   -out "${OUT_DIR}/tajs" `# Send TAJS output here` \
#   "${SWC_OUT_DIR}/${SOURCE_FILE}" `# Generate from this source file` \
#   |& tee "${LOG_DIR}/tajs.log"

# time `# Time performing the following...` \
#   "${SOUFFLE}" `# Run souffle` \
#   -c `# Compile` \
#   -j "${NUM_CPUS}" `# Jobs` \
#   -F "${OUT_DIR}/tajs/flowgraphs/facts" `# Input facts` \
#   -I "${POLA_JS_DIR}/dl/include" `# Datalog source files include directory` \
#   "${POLA_JS_DIR}/dl/node-require-source-target.dl" `# Main datalog source file` \
#   -D "${OUT_DIR}/results" `# Output directory` \
#   `# -p "${OUT_DIR}/profile_c_j_72_no_frequency.prof" \ # Profiling` \
#   |& tee "${LOG_DIR}/souffle.log" # Souffle output log

wait
