#!/bin/bash
set -e -o pipefail
export ZUO_JOBS="$(getconf _NPROCESSORS_ONLN)"
if test "$TOOLCHAIN" = vs ; then
    # cmd.exe /c "build.bat $TARGET_MACHINE"
    echo assuming built previously
else
    if test -n "$CONFIGURE_ARGS" ; then
        ./configure $CONFIGURE_ARGS
    else
        ./configure -m="$TARGET_MACHINE"
    fi
    make
fi
case "$TARGET_MACHINE" in
  *a6nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.9/iconv-x64.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
  *i3nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.9/iconv-x86.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
esac
