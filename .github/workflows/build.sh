#!/bin/bash
set -e -o pipefail
export ZUO_JOBS="$(getconf _NPROCESSORS_ONLN)"
if test "$TOOLCHAIN" = vs ; then
    MSYS_NO_PATHCONV=1 cmd.exe /c "build.bat $TARGET_MACHINE"
else
    if test -n "$CONFIGURE_ARGS" ; then
        ./configure $CONFIGURE_ARGS
    else
        ./configure -m="$TARGET_MACHINE"
    fi
    make $MAKE_ARGS
fi
case "$TARGET_MACHINE" in
  *a6nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.10/iconv-x64.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
  *i3nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.10/iconv-x86.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
  *arm64nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.10/iconv-arm64.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
esac
