#!/bin/bash

if echo "$TRAVIS_COMMIT_MESSAGE" | grep -q "^travis:only:" ; then
    if ! echo "$TRAVIS_COMMIT_MESSAGE" | grep "^travis:only:" | grep -q "\<${TARGET_MACHINE}\>" ; then
        echo "Skipping builds for target machine type $TARGET_MACHINE due to commit message:"
        echo "$TRAVIS_COMMIT_MESSAGE" | grep "^travis:only:"
        exit 1
    fi
fi
