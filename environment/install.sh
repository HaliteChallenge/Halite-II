# Install Halite environment
curl "https://halite.io/downloads/environment/HaliteEnvironment-Source.zip" -o "HaliteEnvironment-Source.zip"
mkdir HaliteEnvironment-Source
unzip HaliteEnvironment-Source.zip -d HaliteEnvironment-Source
rm HaliteEnvironment-Source.zip
cd HaliteEnvironment-Source
make
mv halite ../
cd ../
rm -r HaliteEnvironment-Source 
