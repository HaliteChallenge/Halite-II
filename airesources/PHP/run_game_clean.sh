#!/bin/sh

BASEDIR=$(dirname $0)
cd ${BASEDIR}

rm -Rf ./*.log && rm -Rf ./replay-* && ./run_game.sh
