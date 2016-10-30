# Install tclap
curl "https://codeload.github.com/mirror/tclap/tar.gz/tclap-1-2-1-release-final" -o "tclap-1.2.1.tar.gz"
tar -xvzf tclap-1.2.1.tar.gz
rm -r tclap-1.2.1.tar.gz
cd tclap-1.2.1
rm -r examples
./configure
make
make install
cd ../
rm -r tclap-1.2.1

# Install Halite environment
curl "https://halite.io/downloads/environment/HaliteEnvironment-Source.zip" -o "HaliteEnvironment-Source.zip"
mkdir HaliteEnvironment-Source
unzip HaliteEnvironment-Source.zip -d HaliteEnvironment-Source
rm HaliteEnvironment-Source.zip
cd HaliteEnvironment-Source
make
make install
cd ../
rm -r HaliteEnvironment-Source 
