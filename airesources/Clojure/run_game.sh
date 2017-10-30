#!/bin/sh

lein uberjar
halite -d "240 160" "java -jar target/MyBot.jar" "java -jar target/MyBot.jar"
