#!/usr/bin/env bash

#
# Run datalog/js tests. This script runs tests that pass JavaScript through TAJS
# to generate datalog facts, and then test datalog inferences based on the
# generated facts.
#

source "$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/env.sh"

DL_DIR="${POLA_JS_DIR}/dl"
TEST_DIR="${DL_DIR}/test/js"

NOW=$(date +%Y-%m-%d_%H-%M-%S)

# EITHER: Timestamped output and log directories
#OUT_DIR="${POLA_JS_DIR}/out/test/js/${NOW}"
#LOG_DIR="${POLA_JS_DIR}/out/log/test/js/${NOW}"

# OR: Ordinary output and log directories
OUT_DIR="${POLA_JS_DIR}/out/test/js"
LOG_DIR="${POLA_JS_DIR}/out/log/test/js"

echo "Compiling TAJS"
echo "..TAJS source: ${TAJS_DIR}"
"$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/tajs-compile.sh"

echo "Running tests"
echo "..Timestamp: ${NOW}"
echo "..Test directory: ${TEST_DIR}"
echo "..Out directory: ${OUT_DIR}"
echo "..Log directory: ${LOG_DIR}"

rm -rf "${OUT_DIR}" "${LOG_DIR}"
mkdir -p "${OUT_DIR}" "${LOG_DIR}"

TEST_FAILURE_STATUS=17
SUITE_FAILURE_STATUS=19

SUITE_PIDS=()
for SUITE_DIR_TRAILING_SLASH in $(ls -d "${TEST_DIR}/"*/); do
  (
    SUITE_NAME=$(basename ${SUITE_DIR_TRAILING_SLASH})
    echo "....Entering test suite dir ${SUITE_DIR_TRAILING_SLASH}"
    TEST_PIDS=()
    for SRC_FILE in $(ls -d "${SUITE_DIR_TRAILING_SLASH}"*.js); do
      echo "......Found source file: ${SRC_FILE}"
      (
        TEST_NAME=$(basename "${SRC_FILE%.js}")
        TEST_OUT_DIR="${OUT_DIR}/${SUITE_NAME}/${TEST_NAME}"
        TEST_TAJS_OUT_DIR="${TEST_OUT_DIR}/tajs"
        TEST_RESULTS_OUT_DIR="${TEST_OUT_DIR}/results"
        TEST_LOG_DIR="${LOG_DIR}/${SUITE_NAME}/${TEST_NAME}"
        rm -rf "${TEST_OUT_DIR}" "${TEST_LOG_DIR}"
        mkdir -p "${TEST_OUT_DIR}" "${TEST_LOG_DIR}"
        TEST_FILE="${SRC_FILE%.js}.dl"
        if [[ ! -f "${TEST_FILE}" ]]; then
          echo "!!!!!!Failed to find test file: ${TEST_FILE}"
          exit 127
        else
          echo "......Found test file: ${TEST_FILE}"
          (
            set -e
            mkdir -p "${TEST_TAJS_OUT_DIR}"
            java -ea dk.brics.tajs.FlowGraphOnlyMain -out "${TEST_TAJS_OUT_DIR}" "${SRC_FILE}" > "${TEST_LOG_DIR}/tajs.log" 2>&1
            mkdir -p "${TEST_RESULTS_OUT_DIR}"
            souffle -F "${TEST_TAJS_OUT_DIR}/flowgraphs/facts" -I dl/include "${TEST_FILE}" -D "${TEST_RESULTS_OUT_DIR}" > "${TEST_LOG_DIR}/souffle.log" 2>&1
            RESULT_FILES_WITH_ERRORS=()
            for RESULT_FILE in $(ls -d "${TEST_RESULTS_OUT_DIR}/"*.csv); do
              WC_L=$(wc -l "${RESULT_FILE}")
              RESULT_LINES=$(echo "${WC_L%${RESULT_FILE}}" | sed 's/ *//g')
              if [[ "${RESULT_LINES% ${RESULT_FILE}}" != "0" ]]; then
                RESULT_FILES_WITH_ERRORS+=("${RESULT_FILE}")
              fi
            done
            if [[ "${#RESULT_FILES_WITH_ERRORS[@]}" != "0" ]]; then
              echo "!!!!!!${SUITE_NAME}/${TEST_NAME} FAILED:"
              echo "      Datalog: ${TEST_FILE}"
              echo "      JavaScript: ${SRC_FILE}"
              echo "      Results: ${RESULT_FILES_WITH_ERRORS[@]}"
              for RESULT_FILE_WITH_ERROR in ${RESULT_FILES_WITH_ERRORS[@]}; do
                echo "!!!!!!!!  ${RESULT_FILE_WITH_ERROR}:"
                cat ${RESULT_FILES_WITH_ERRORS[@]}
              done
              exit ${TEST_FAILURE_STATUS}
            fi
          )
          TEST_STATUS=$?
          if [[ "${TEST_STATUS}" != 0 && "${TEST_STATUS}" != "${TEST_FAILURE_STATUS}" ]]; then
            echo "!!!!!!ERROR: ${SUITE_NAME}/${TEST_NAME} ERROR: Status ${TEST_STATUS}"
            echo "!!!!!!!!  ${TEST_LOG_DIR}/tajs.log"
            cat "${TEST_LOG_DIR}/tajs.log"
            echo "!!!!!!!!  ${TEST_LOG_DIR}/souffle.log"
            cat "${TEST_LOG_DIR}/souffle.log"
          fi
          exit "${TEST_STATUS}"
        fi
      ) &
      TEST_PIDS+=("$!")
    done
    NUM_TEST_PASSES=0
    NUM_TEST_ERRORS=0
    NUM_TEST_FAILURES=0
    for TEST_PID in "${TEST_PIDS[@]}"; do
      wait "${TEST_PID}"
      TEST_STATUS=$?
      if [[ "${TEST_STATUS}" == "${TEST_FAILURE_STATUS}" ]]; then
        NUM_TEST_FAILURES=$((NUM_TEST_FAILURES+1))
      elif [[ "${TEST_STATUS}" != "0" ]]; then
        NUM_TEST_ERRORS=$((NUM_TEST_ERRORS+1))
      else
        NUM_TEST_PASSES=$((NUM_TEST_PASSES+1))
      fi
    done
    if [[ "${NUM_TEST_FAILURES}" == 0 && "${NUM_TEST_ERRORS}" == 0 ]]; then
      echo "✔️✔️✔️✔️Suite: ${SUITE_NAME}: ${NUM_TEST_PASSES} passed; ${NUM_TEST_FAILURES} failed; ${NUM_TEST_ERRORS} errors"
      exit 0
    else
      echo "🛑🛑🛑🛑Suite: ${SUITE_NAME}: ${NUM_TEST_PASSES} passed; ${NUM_TEST_FAILURES} failed; ${NUM_TEST_ERRORS} errors"
      exit "${SUITE_FAILURE_STATUS}"
    fi
  ) &
  SUITE_PIDS+=("$!")
done

NUM_SUITE_PASSES=0
NUM_SUITE_ERRORS=0
NUM_SUITE_FAILURES=0
for SUITE_PID in "${SUITE_PIDS[@]}"; do
  wait "${SUITE_PID}"
  SUITE_STATUS=$?
  if [[ "${SUITE_STATUS}" == "${SUITE_FAILURE_STATUS}" ]]; then
    NUM_SUITE_FAILURES=$((NUM_SUITE_FAILURES+1))
  elif [[ "${SUITE_STATUS}" != "0" ]]; then
    NUM_SUITE_ERRORS=$((NUM_SUITE_ERRORS+1))
  else
    NUM_SUITE_PASSES=$((NUM_SUITE_PASSES+1))
  fi
done
if [[  "${NUM_SUITE_FAILURES}" == 0 && "${NUM_SUITE_ERRORS}" == 0 ]]; then
  echo "✔️✔️"
  echo "✔️✔️Test suites: ${NUM_SUITE_PASSES} passed; ${NUM_SUITE_FAILURES} failed; ${NUM_SUITE_ERRORS} errors"
  echo "✔️✔️"
  exit 0
else
  echo "🛑🛑"
  echo "🛑🛑Test suites: ${SUITE_NAME}: ${NUM_SUITE_PASSES} passed; ${NUM_SUITE_FAILURES} failed; ${NUM_SUITE_ERRORS} errors"
  echo "🛑🛑"
  exit 1
fi
