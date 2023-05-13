#!/bin/bash
set -e -o pipefail

# This script is meant to be called in a directory where
# "chez-scheme-$tag" and "chez-scheme-$tag.tgz" can be
# created

repo=$1
if test "$repo" = "" ; then
    echo "need to supply a repo"
    exit 1
fi

tag=$2
if test "$tag" = "" ; then
    echo "need to supply a tag"
    exit 1
fi

name=chez-scheme-"$tag"

git clone -b "$tag" --single-branch "$repo" "$name"
(cd "$name" && git submodule update --init --depth 1)
rm -rf "$name"/.git*
tar zcf "$name".tar.gz "$name"

gh release create \
   --repo https://github.com/racket/ChezScheme \
   --title "$tag" \
   --notes "The archive $name.tar.gz includes submodules." \
   "$tag" "$name".tar.gz
