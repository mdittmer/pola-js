#!/usr/bin/env bash

NCPU=""

# Darwin
NCPU=$(
  set -e
  sysctl -n hw.ncpu 2>/dev/null
)

if [[ "${NCPU}" == "" ]]; then
  # Linux
  NCPU=$(
    set -e
    grep -c processor /proc/cpuinfo 2>/dev/null

  )
fi

if [[ "${NCPU}" == "" ]]; then
  # Fallback
  NCPU=4
fi

export NUM_CPUS="${NCPU}"
