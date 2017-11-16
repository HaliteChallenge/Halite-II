#!/bin/sh

sbt assembly
./halite -d "240 160" "java -jar ./target/scala-2.12/MyBot.jar" "java -jar ./target/scala-2.12/MyBot.jar"

