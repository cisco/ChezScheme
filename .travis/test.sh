#!/bin/bash

if test -n "$PARALLEL_MATS" ; then
    njobs="$PARALLEL_MATS"
else
    njobs="$(getconf _NPROCESSORS_ONLN)"
fi
make -C "$TARGET_MACHINE"/mats -j "$njobs" partialx

if [ -f "$TARGET_MACHINE"/mats/summary ]; then
  diff -q .travis/summary "$TARGET_MACHINE"/mats/summary
  rc=$?
  if [ $rc -gt 0 ]; then
      echo 'travis_fold:start:make_prereqs_output'
      echo "Make output from $TARGET_MACHINE/mats/Make.out:"
      cat "$TARGET_MACHINE"/mats/Make.out
      echo 'travis_fold:end:make_prereqs_output'
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
