#!/bin/sh

BASEDIR=$(dirname $0)
cd ${BASEDIR}
rm -Rf ./*.log && rm -Rf ./replay-* && rm -Rf ./logs/*.log && ./run_game.sh
