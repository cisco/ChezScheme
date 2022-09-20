#!/bin/bash
export ZUO_JOBS="$(getconf _NPROCESSORS_ONLN)"
if test "$TEST_TARGET" = ""; then
    TEST_TARGET=test-some
fi
if test "$TOOLCHAIN" = vs ; then
    cmd.exe /c "build.bat $TARGET_MACHINE /$TEST_TARGET"
else
    make $TEST_TARGET
fi
