#################
# Compiler tools
#################
apt-get -y update 
apt-get install -y build-essential python3 default-jdk clang++-3.5
curl -sSf https://static.rust-lang.org/rustup.sh | sh

#############
# Worker
#############
apt-get install -y zip python3-pip
pip3 install requests

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

python3 worker.py
