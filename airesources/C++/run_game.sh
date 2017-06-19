#!/usr/bin/env bash

set -e

clang++ -std=c++11 -g -Wall -pedantic -fsanitize=address,undefined my_bot.cpp -o my_bot
../../environment/halite -d "96 96" "./my_bot" "./my_bot"