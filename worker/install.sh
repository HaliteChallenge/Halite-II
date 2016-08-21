##################
# Compiler tools #
##################
apt-get -y update 
apt-get install -y curl build-essential python3 default-jdk clang++-3.5
curl -sSf https://static.rust-lang.org/rustup.sh | sh

##########
# Worker #
##########
apt-get install -y zip python3-pip
pip3 install requests
pip3 install zip

################
# Docker SETUP #
################
curl -sSL https://get.docker.com/ | sh

################
# Start Docker #
################
service docker.io restart

echo "Pulling docker image"
docker pull mntruell/halite_sandbox:latest
echo "Retrieving Installed Docker Images"
docker images
