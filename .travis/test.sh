#!/bin/bash

make -C "$TARGET_MACHINE"/mats -j $(getconf _NPROCESSORS_ONLN) partialxp

if [ -f "$TARGET_MACHINE"/mats/summary ]; then
  diff -q .travis/summary "$TARGET_MACHINE"/mats/summary
  exit $?
else
  exit 1
fi
