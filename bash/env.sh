#!/usr/bin/env bash

#
# Set up build and development environment.
#

export POLA_JS_BASH_SCRIPTS_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
export POLA_JS_DIR=$(dirname "${POLA_JS_BASH_SCRIPTS_DIR}")
export TAJS_DIR="${POLA_JS_DIR}/TAJS"
export TAJS_BUILD_DIR="${TAJS_DIR}/antbuild"

# TODO: Apache Ant-related paths below should not be hard-coded.
export TAJS_CLASSPATH="${TAJS_DIR}/extras/inspector/dist/inspector.jar:${TAJS_DIR}/extras/jalangilogger/build/libs/jalangilogger-1.1.jar:${TAJS_DIR}/extras/ts-spec-reader/build/libs/ts-spec-reader.jar:${TAJS_DIR}/lib/closure-compiler-v20170806.jar:${TAJS_DIR}/lib/gson-2.8.1.jar:${TAJS_DIR}/lib/jericho-html-3.3.jar:${TAJS_DIR}/lib/jetty-all-9.4.8.v20171121-uber.jar:${TAJS_DIR}/lib/log4j-1.2.17.jar:${TAJS_DIR}/lib/nashorn-jb8u212-b1566.8.jar:${TAJS_DIR}/lib/test/hamcrest-core-1.3.jar:${TAJS_DIR}/lib/test/junit-4.12.jar:${TAJS_DIR}/lib/commons-text-1.9.jar:${TAJS_DIR}/lib/commons-lang3-3.12.0.jar:${TAJS_DIR}/antbuild:${TAJS_DIR}/test-resources:/usr/share/java/ant-launcher-1.10.9.jar:/usr/share/ant/lib/ant.jar:/usr/share/ant/lib/ant-junit.jar:/usr/share/ant/lib/ant-junit4.jar"

export CLASSPATH="${TAJS_CLASSPATH}"
