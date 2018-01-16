#!/bin/bash -e

./configure -m=$TARGET_MACHINE
make
( cd ${TARGET_MACHINE}/mats && make all 2>&1 ) | tee Make.out | grep '^matting '
echo "exit code: $?"
