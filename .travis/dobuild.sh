#!/bin/bash
case $TARGET_MACHINE in
  *i3le) sudo apt-get -yq --no-install-suggests --no-install-recommends install uuid-dev:i386 ;;
  *)
esac
./configure -m=$TARGET_MACHINE
exitcode=$?
if [ $exitcode -ne 0 ] ; then
  echo "Failed: configure step"
  exit $exitcode
fi
make
exitcode=$?
if [ $exitcode -ne 0 ] ; then
  echo "Failed: make step"
  exit $exitcode
fi
( cd ${TARGET_MACHINE}/mats && make partialx 2>&1 ) | tee Make.out | grep '^matting '
diff -q .travis/summary ${TARGET_MACHINE}/mats/summary
exitcode=$?

if [ $exitcode -ne 0 ] ; then
  echo "Failed: testing step"
  echo "mats summary:"
  cat ${TARGET_MACHINE}/mats/summary
  exit $exitcode
fi
