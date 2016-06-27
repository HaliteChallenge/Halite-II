sudo apt-get install -y mysql-server

echo "Starting MySql setup"
echo "create database Halite;" | mysql -u root -p
mysql -u root -p Halite < website/sql/Database.sql
