#!/bin/sh

swift build
./halite -d "240 160" "swift run --skip-build" "swift run --skip-build"
