#!/bin/bash
set -e

function finish {
	echo "Cleaning up"
	cd $WORKINGDIR
	cp temp.ini ../halite.ini
}
trap finish EXIT

echo "Setting up"
WORKINGDIR=$PWD
if [ -e ../halite.ini ]
	then cp ../halite.ini temp.ini;
fi
cp tests.ini ../halite.ini
python3 setupMysql.py || python setupMysql.py

echo "Website tests"
phpunit --stderr website/

echo "Environment tests"
cd environment
python3 testenv.py
