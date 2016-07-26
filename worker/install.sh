#################
# Compiler tools
#################
sudo apt-get -y update 
sudo apt-get install -y build-essential python3 default-jdk clang++-3.5
sudo curl -sSf https://static.rust-lang.org/rustup.sh | sh

#############
# Worker
#############
sudo apt-get install -y zip python3-pip
sudo pip3 install requests

###########################
# Docker SETUP
###########################
sudo apt-get install -y docker.io
ln -sf /usr/bin/docker.io /usr/local/bin/docker
sed -i '$acomplete -F _docker docker' /etc/bash_completion.d/docker.io

echo "Docker Setup complete"

#########
# Start Docker
########
sudo service docker.io restart

echo "Creating Docker Image"
docker build -t 'virtual_machine' - < Dockerfile
echo "Retrieving Installed Docker Images"
docker images
