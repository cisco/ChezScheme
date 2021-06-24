#!/bin/bash
set -e -o pipefail

if test -n "$PARALLEL_MATS" ; then
    njobs="$PARALLEL_MATS"
else
    njobs="$(getconf _NPROCESSORS_ONLN)"
fi
echo "Running partialx with $njobs jobs..."
make -C "$TARGET_MACHINE"/mats -j "$njobs" partialx

if [ -f "$TARGET_MACHINE"/mats/summary ]; then
  diff -q .github/workflows/summary "$TARGET_MACHINE"/mats/summary
  rc=$?
  if [ $rc -gt 0 ]; then
      echo "Make output from $TARGET_MACHINE/mats/Make.out:"
      cat "$TARGET_MACHINE"/mats/Make.out
      for log in "$TARGET_MACHINE"/mats/output*/Make.out ; do
          echo "Make output from ${log}:"
          cat "${log}"
      done
  fi
  exit $rc
else
  exit 1
fi
