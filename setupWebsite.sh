sudo apt-get install -y lamp-server^
sudo apt-get install -y python3 python3-pip 
sudo pip3 install trueskill

# File structure
echo "Setting up file structure"
mkdir storage storage/replays storage/bots
sudo chown www-data:www-data storage storage/replays storage/bots
sudo chmod 755 storage storage/replays storage/bots

# Copy all files to the apache directory
cp ./* /var/www/html

echo "Please follow the directions in the INSTALL.md file!!!!!"
