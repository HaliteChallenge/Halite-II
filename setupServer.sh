# File structure
echo "Setting up file structure"
mkdir storage storage/replays storage/bots
sudo chown www-data:www-data storage storage/replays storage/bots
sudo chmod 755 storage storage/replays storage/bots

# Mysql
echo "Starting MySql setup"
echo "create database Halite;" | mysql -u root -p
mysql -u root -p Halite < website/sql/Database.sql
