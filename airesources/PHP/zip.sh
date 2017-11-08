#!/bin/sh

BASEDIR=$(dirname $0)
cd ${BASEDIR}

rm ./submission.zip > /dev/null
zip -r submission.zip . -i '*.php'
