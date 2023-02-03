#!/usr/bin/env bash

#
# Set up build and development environment.
#

export POLA_JS_BASH_SCRIPTS_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
export POLA_JS_DIR=$(dirname "${POLA_JS_BASH_SCRIPTS_DIR}")
export TAJS_DIR="${POLA_JS_DIR}/TAJS"
export TAJS_BUILD_DIR="${TAJS_DIR}/antbuild"
export SOUFFLE_DIR="${POLA_JS_DIR}/souffle"
export SOUFFLE="${SOUFFLE_DIR}/build/src/souffle"
export SWC_DIR="${POLA_JS_DIR}/swc"
export SWC_BINDINGS_DIR="${SWC_DIR}/bindings"
export SWC_CLI_DIR="${SWC_BINDINGS_DIR}/swc_cli"
export SWC="${SWC_BINDINGS_DIR}/target/release/swc"
export SEMAPHORIC_DIR="${POLA_JS_DIR}/semaphoric"
export SEMAPHORIC="${SEMAPHORIC_DIR}/semaphoric"
export ARTIFACT_EXECUTOR="${POLA_JS_DIR}/artifact-executor/bash/bin/artifact-executor"

# TODO: Apache Ant-related paths below should not be hard-coded.
export TAJS_CLASSPATH="${TAJS_DIR}/extras/inspector/dist/inspector.jar:${TAJS_DIR}/extras/jalangilogger/build/libs/jalangilogger-1.1.jar:${TAJS_DIR}/extras/ts-spec-reader/build/libs/ts-spec-reader.jar:${TAJS_DIR}/lib/closure-compiler-v20170806.jar:${TAJS_DIR}/lib/gson-2.8.1.jar:${TAJS_DIR}/lib/jericho-html-3.3.jar:${TAJS_DIR}/lib/jetty-all-9.4.8.v20171121-uber.jar:${TAJS_DIR}/lib/log4j-1.2.17.jar:${TAJS_DIR}/lib/nashorn-jb8u212-b1566.8.jar:${TAJS_DIR}/lib/test/hamcrest-core-1.3.jar:${TAJS_DIR}/lib/test/junit-4.12.jar:${TAJS_DIR}/lib/commons-text-1.9.jar:${TAJS_DIR}/lib/commons-lang3-3.12.0.jar:${TAJS_DIR}/antbuild:${TAJS_DIR}/test-resources:/usr/share/java/ant-launcher-1.10.9.jar:/usr/share/ant/lib/ant.jar:/usr/share/ant/lib/ant-junit.jar:/usr/share/ant/lib/ant-junit4.jar"

export ARTIFACT_EXECUTOR_CACHE="${POLA_JS_DIR}/ae-cache"

export CLASSPATH="${TAJS_CLASSPATH}"

source "${POLA_JS_BASH_SCRIPTS_DIR}/ncpu.sh"
