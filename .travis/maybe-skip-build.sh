#!/bin/bash

git branch -v
echo "TRAVIS_COMMIT: $TRAVIS_COMMIT"
echo "TRAVIS_COMMIT_RANGE: $TRAVIS_COMMIT_RANGE"
echo "TRAVIS_PULL_REQUEST: $TRAVIS_PULL_REQUEST"

if test -z "$TRAVIS_PULL_REQUEST"; then
    msg="$TRAVIS_COMMIT_MESSAGE"
else
    msg="$(echo "$TRAVIS_COMMIT_RANGE" | cut -d. -f4 |xargs git log --format=%B -n 1)"
fi

echo "Checking to see whether to skip build for $TARGET_MACHINE due to commit message"
echo "$msg"
msg_only="$(echo "$msg" | grep "^travis:only:")"
if test -n "$msg_only" ; then
    if ! echo "$msg_only" | grep -q "\<${TARGET_MACHINE}\>" ; then
        echo "Skipping builds for target machine type $TARGET_MACHINE due to commit message:"
        echo "$msg_only"
        exit 1
    fi
fi

