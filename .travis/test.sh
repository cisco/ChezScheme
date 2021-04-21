#!/bin/bash

make -C "$TARGET_MACHINE"/mats -j $(getconf _NPROCESSORS_ONLN) partialxp

if [ -f "$TARGET_MACHINE"/mats/summary ]; then
  diff -q .travis/summary "$TARGET_MACHINE"/mats/summary
  rc=$?
  if [ $rc -gt 0 ]; then
      for log in "$TARGET_MACHINE"/mats/output*/Make.out ; do
          echo 'travis_fold:start:make_output'
          echo "Make output from ${log}:"
          cat "${log}"
          echo 'travis_fold:end:make_output'
      done
  fi
  exit $rc
else
  exit 1
fi
