# Make environment
cd environment
make clean
make
cd ../

# Make visualizer
cd visualizer
make clean
make
cd ../

# Package
mkdir temp
cp environment/environment temp/environment
cp visualizer/visualizer temp/visualizer
cp -R visualizer/fonts temp/fonts
cp -R visualizer/shaders temp/shaders
cd temp
zip -r ../DebianHalite.zip *
cd ../
rm -rf temp