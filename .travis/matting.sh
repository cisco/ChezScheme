#!/bin/bash
# We don't use grep because on Windows, it doesn't flush its output.
live=no
while read -r line; do
    echo "$line" >> mats.out
    if [[ "$line" = matting* ]]; then
	echo "$line"
	live=yes
    elif [[ "$live" = no ]]; then
	echo "$line"
    fi
done
