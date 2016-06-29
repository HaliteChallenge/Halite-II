sudo apt-get install -y g++-4.8 libstdc++6-4.7-dev
sudo apt-get install -y libXmu-dev libXi-dev dos2unix git wget
sudo apt-get install -y libfreetype6-dev libglew-dev pkg-config
echo "Done with non glfw3 setup"

sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:george-edison55/cmake-3.x
sudo apt-get update -qq
sudo apt-get install -y cmake
sudo apt-get install -y xorg-dev libglu1-mesa-dev
sudo apt-get build-dep -y glfw
echo "Done with glfw3 setup"

sudo apt-get install -y unzip wget
yes | wget https://github.com/glfw/glfw/releases/download/3.2/glfw-3.2.zip
yes | unzip glfw-3.2.zip -d glfw
cd glfw/glfw-3.2
cmake -G "Unix Makefiles"
echo "Ready to compile glfw"

sudo make
sudo make install
echo "Done installing the visualizer's dependencies"
