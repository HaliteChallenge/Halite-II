##########
# Checks #
##########
if [ ! -f ../halite.ini ]; then
	echo "FAIL\nCannot install worker until a halite.ini file is created."
	return
fi

[ "$#" -eq 1 ] || {
	echo "Please provide one arguement.";
	return
}

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
service docker.io restart

echo "Pulling docker image"
docker pull mntruell/halite_sandbox:latest
echo "Retrieving Installed Docker Images"
docker images

###########
# API Key #
###########
python3 changeAPIKey.py $1
