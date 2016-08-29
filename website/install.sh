sudo LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php
sudo apt-get update

sudo apt-get install -y mysql-server-5.6 php5.6 php5.6-mysql apache2 
sudo apt-get install -y python3 python3-pip 

sudo pip3 install trueskill boto paramiko

# File structure
cd ../
echo "Setting up file structure"
mkdir storage storage/replays storage/bots storage/cache storage/errors
sudo chown www-data:www-data storage storage/replays storage/bots storage/cache storage/errors
sudo chmod 755 storage storage/replays storage/bots storage/cache storage/errors
cp ./* /var/www/html

echo "Please follow the directions in the INSTALL.md file!!!!!"
