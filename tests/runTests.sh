#!/bin/bash
echo "Setting up"
WORKINGDIR=$PWD
cp ../halite.ini temp.ini
cp tests.ini ../halite.ini
python3 setupMysql.py || python setupMysql.py

echo "Website tests"
phpunit --stderr website/

echo "Environment tests"
cd environment
python3 testenv.py

function finish {
	echo "Cleaning up"
	cd $WORKINGDIR
	cp temp.ini ../halite.ini
}
trap finish EXIT
