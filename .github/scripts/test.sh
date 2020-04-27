#!/bin/bash
runmats() {
  echo travis_fold:start:mats
  echo make allxhelp "$@"
  make -C ${TARGET_MACHINE}/mats allxhelp "$@" 2>&1 | tee -a Make.out | grep '^matting '
  echo travis_fold:end:mats
}

# Split these out so that we get output every 10 minutes on Windows builds.
runmats o=0
runmats o=3
runmats o=3 cp0=t
runmats o=3 cp0=t eval=interpret

if [ -f ${TARGET_MACHINE}/mats/summary ]; then
  cat ${TARGET_MACHINE}/mats/summary
  diff -q .github/scripts/summary ${TARGET_MACHINE}/mats/summary
  exit $?
else
  exit 1
fi
