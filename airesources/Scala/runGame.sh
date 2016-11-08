#!/bin/bash

javac *.java
scalac HaliteBot.scala
scalac MyBot.scala
scalac RandomBot.scala

./halite -d "30 30" "scala MyBot" "scala RandomBot"
