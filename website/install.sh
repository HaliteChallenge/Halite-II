sudo LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php
sudo apt-get update

sudo apt-get install -y mysql-server-5.6 php5.6 php5.6-mysql apache2 
sudo apt-get install -y python3 python3-pip 

sudo pip3 install trueskill boto paramiko pymysql

curl -sS https://getcomposer.org/installer | php
mv composer.phar /usr/local/bin/composer
composer install

# File structure
cd ../
echo "Setting up file structure"
mkdir storage storage/bots storage/compile
sudo chown www-data:www-data storage storage/bots storage/compile 
sudo chmod 755 storage storage/bots storage/compile 
cp ./* /var/www/html

echo "Attention: Please follow the directions in the install section of our technical spec! This is located inside of of git repo."
