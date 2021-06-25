#!/bin/bash
if test -n "$PARALLEL_MATS" ; then
    njobs="$PARALLEL_MATS"
else
    njobs="$(getconf _NPROCESSORS_ONLN)"
fi
echo "Running partialx with $njobs jobs..."
make --version
make -C "$TARGET_MACHINE"/mats -j "$njobs" partialx
