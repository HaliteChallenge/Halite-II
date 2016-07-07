cd ../../halite/environment
make clean
make
zip Debian.zip environment
mv Debian.zip ../

cd ../visualizer
make clean
make
zip -ur ../Debian.zip fonts shaders visualizer

cd ../../website/downloads
rm Debian.zip
mv ../../halite/Debian.zip ./
