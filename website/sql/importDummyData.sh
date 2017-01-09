#!/bin/bash

echo "drop database Halite; create database Halite;" | mysql -u root -p
mysql -u root Halite < schema.sql -p
mysql -u root Halite < dummyData.sql -p
