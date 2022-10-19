#!/bin/bash

TEST_SUITE_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
echo "// Overflow line multiplier hack with >1M-char line and >1M lines" > "${TEST_SUITE_DIR}/line-col.js"
echo "var x;" >> "${TEST_SUITE_DIR}/line-col.js"
echo "(function(a) {" >> "${TEST_SUITE_DIR}/line-col.js"
COUNT=1000000
for I in $(seq ${COUNT}); do
  echo -n " " >> "${TEST_SUITE_DIR}/line-col.js"
done
echo "x = a;" >> "${TEST_SUITE_DIR}/line-col.js"
echo "})();" >> "${TEST_SUITE_DIR}/line-col.js"
for I in $(seq ${COUNT}); do
  echo "" >> "${TEST_SUITE_DIR}/line-col.js"
done
echo "x.a;" >> "${TEST_SUITE_DIR}/line-col.js"
