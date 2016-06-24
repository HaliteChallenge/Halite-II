#################
# Compiler tools
#################
apt-get install python3-pip
apt-get install default-jdk
apt-get install zip
apt-get install clang++-3.5

#############
# Worker
#############
pip3 install trueskill

###########################
# Docker SETUP
###########################
apt-get update
apt-get install docker.io
ln -sf /usr/bin/docker.io /usr/local/bin/docker
sed -i '$acomplete -F _docker docker' /etc/bash_completion.d/docker.io

echo "Docker Setup complete"

#########
# Start Docker
########
service docker.io restart

echo "Creating Docker Image"
docker build -t 'virtual_machine' - < Dockerfile
echo "Retrieving Installed Docker Images"
docker images
