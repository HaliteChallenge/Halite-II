#!/bin/bash

echo "Are you sure that you want to run this script??? It will erase everything in your db. Answer [y/n]:"

read ANSWER

if [ "$ANSWER" == "y" ]; then
	echo "drop database Halite; create database Halite;" | mysql -u root
	mysql -u root Halite < schema.sql 
	mysql -u root Halite < dummyData.sql
fi
