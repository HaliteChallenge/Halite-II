#!/usr/bin/env bash

set -e

clang++ -std=c++11 -g -Wall -pedantic -fsanitize=address,undefined MyBot.cpp -o MyBot
../../environment/halite -d "96 96" "./MyBot" "./MyBot"