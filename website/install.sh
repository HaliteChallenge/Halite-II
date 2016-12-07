LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php
apt-get update

apt-get install -y php5.6 php5.6-mysql apache2 
a2enmod rewrite expires

apt-get install -y python3 python3-pip 

pip3 install trueskill boto paramiko pymysql

apt-get install -y zip 

curl -sS https://getcomposer.org/installer | php
mv composer.phar /usr/local/bin/composer
composer install
