#!/bin/bash

git branch -v
echo "TRAVIS_COMMIT: $TRAVIS_COMMIT"
echo "TRAVIS_COMMIT_RANGE: $TRAVIS_COMMIT_RANGE"
echo "TRAVIS_PULL_REQUEST: $TRAVIS_PULL_REQUEST"
echo "Checking to see whether to skip build for $TARGET_MACHINE due to commit message"
echo "$TRAVIS_COMMIT_MESSAGE"
msg_only="$(echo "$TRAVIS_COMMIT_MESSAGE" | grep "^travis:only:")"
if test -n "$msg_only" ; then
    if ! echo "$msg_only" | grep -q "\<${TARGET_MACHINE}\>" ; then
        echo "Skipping builds for target machine type $TARGET_MACHINE due to commit message:"
        echo "$msg_only"
        exit 1
    fi
fi

