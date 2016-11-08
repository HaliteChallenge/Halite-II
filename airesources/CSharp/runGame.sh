#!/bin/bash

mcs -t:library -out:HaliteHelper.dll HaliteHelper.cs
mcs -reference:HaliteHelper.dll -out:MyBot.o MyBot.cs
mcs -reference:HaliteHelper.dll -out:RandomBot.o RandomBot.cs 
./halite -d "30 30" "./MyBot.o" "./RandomBot.o"
