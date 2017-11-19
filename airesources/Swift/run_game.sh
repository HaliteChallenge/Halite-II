#!/bin/sh

swift build
if [ $? -ne 0 ]; then { echo "swift build failed" ; exit 1; } fi

./halite -d "240 160" "swift run --skip-build" "swift run --skip-build"
