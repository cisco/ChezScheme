#!/bin/bash
set -e -o pipefail
./configure -m="$TARGET_MACHINE"
make -j $(getconf _NPROCESSORS_ONLN)
case "$TARGET_MACHINE" in
  *a6nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.9/iconv-x64.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
  *i3nt)
    curl -Ls https://github.com/burgerrg/win-iconv/releases/download/v0.0.9/iconv-x86.dll > "$TARGET_MACHINE"/bin/"$TARGET_MACHINE"/iconv.dll
    ;;
esac
