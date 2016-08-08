sudo apt-get install mysql-sever-5.6
sudo LC_ALL=C.UTF-8 add-apt-repository ppa:ondrej/php
sudo apt-get install php5.6 
sudo apt-get install apache2 
sudo apt-get install -y python3 python3-pip 
sudo pip3 install trueskill

# File structure
echo "Setting up file structure"
mkdir storage storage/replays storage/bots storage/cache storage/errors
sudo chown www-data:www-data storage storage/replays storage/bots storage/cache storage/errors
sudo chmod 755 storage storage/replays storage/bots storage/cache storage/errors

# Copy all files to the apache directory
cp ./* /var/www/html

echo "Please follow the directions in the INSTALL.md file!!!!!"
