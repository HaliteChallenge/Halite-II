#!/bin/bash

gcc MyBot.c -o MyBot.o
gcc RandomBot.c -o RandomBot.o
./halite -d "30 30" "./MyBot.o" "./RandomBot.o"
