##########
# Checks #
##########
if [ ! -f ../halite.ini ]; then
	echo "FAIL\nCannot install worker until a halite.ini file is created."
	return
fi

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

###############
# Swap Memory #
###############
# Replace line 12
sed -i '12s/.*/GRUB_CMDLINE_LINUX="cgroup_enable=memory swapaccount=1"/' /etc/default/grub
update-grub

###########
# API Key #
###########
if [ "$#" -eq 1 ]; then
	python3 changeAPIKey.py $1
fi

echo "A reboot is required to complete this installation!"
