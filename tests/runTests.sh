cp ../halite.ini temp.ini
cp test.ini ../halite.ini

python3 tests.py

cp temp.ini ../halite.ini
