sudo apt-get install -y g++-4.8 libstdc++6-4.7-dev
sudo apt-get install -y libXmu-dev libXi-dev libgl-dev dos2unix git wget
sudo apt-get install -y libfreetype6-dev libglew-dev pkg-config

sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:george-edison55/cmake-3.x
sudo apt-get update -qq
sudo apt-get install -y cmake
yes Y | sudo apt-get upgrade -y
sudo apt-get install -y xorg-dev libglu1-mesa-dev
sudo apt-get build-dep -y glfw3
sudo apt-get build-dep -y glfw

sudo apt-get install -y unzip
yes Y | wget https://github.com/glfw/glfw/releases/download/3.2/glfw-3.2.zip
yes Y | unzip glfw-3.2.zip -d glfw
cd glfw/glfw-3.2
cmake -G "Unix Makefiles"
sudo make
sudo make install
