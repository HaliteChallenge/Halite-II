#################
# Compiler tools
#################
apt-get -y update 
apt-get install -y python3 python3-pip default-jdk clang++-3.5

#############
# Worker
#############
pip3 install trueskill
apt-get install zip

###########################
# Docker SETUP
###########################
apt-get install -y docker.io
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
