#!/bin/bash

lein uberjar

./halite -d "30 30" "java -cp target/MyBot.jar MyBot" "java -cp target/MyBot.jar RandomBot"
