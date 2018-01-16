#!/bin/bash -e

./configure -m=$TARGET_MACHINE
make
( cd ${TARGET_MACHINE}/mats && make partialx 2>&1 ) | tee Make.out | grep '^matting '
echo "exit code: $?"
cat ${TARGET_MACHINE}/mats/summary
diff .travis/summary ${TARGET_MACHINE}/mats/summary
