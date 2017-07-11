#!/usr/bin/env bash

mkdir -p ./assets/environments
rm ./assets/environments/*.zip

cd ../environment
make clean
cd ../website

zip ./assets/environments/SourceCode.zip ../environment

cd ../environment
make -j2
cd ../website
zip ./assets/environments/$(uname).zip ../environment/halite
