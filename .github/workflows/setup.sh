#!/bin/bash
set -e -o pipefail
case "$TARGET_MACHINE" in
  *i3le)
    sudo dpkg --add-architecture i386
    sudo apt-get update
    sudo apt-get install gcc-multilib lib32ncurses5-dev uuid-dev:i386
    ;;
esac
