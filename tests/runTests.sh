#!/bin/bash
set -e

function finish {
    echo "Cleaning up"
    cd $WORKINGDIR
    if [ -e ../temp.ini ]
        then cp temp.ini ../halite.ini;
    fi
}
trap finish EXIT

echo "Setting up"
WORKINGDIR=$PWD
if [ -e ../halite.ini ]
    then cp ../halite.ini temp.ini;
fi

echo "Worker tests"
cd worker
python3 testWorker.py

echo "Environment tests"
cd ../environment
python3 testenv.py 
