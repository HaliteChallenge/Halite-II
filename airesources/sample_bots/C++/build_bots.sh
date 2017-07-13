#!/usr/bin/env bash

CXX=clang++
CXXFLAGS="-std=c++11 -Wall -pedantic -fsanitize=undefined"

if [[ ! $(type -P "${CXX}") ]]; then
    CXX=g++
fi

for bot in *.cpp; do
    ${CXX} ${CXXFLAGS} -o "${bot%.*}" ${bot}
done
