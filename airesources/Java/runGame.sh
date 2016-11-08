#!/bin/bash

javac MyBot.java
javac RandomBot.java
./halite -d "30 30" "java MyBot" "java RandomBot"
