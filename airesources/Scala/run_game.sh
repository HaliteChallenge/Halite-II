#!/bin/bash
cd hlt
scalac *.scala
cd ..
scalac -cp hlt *.scala
./halite -d "240 160" "scala -cp .:hlt MyBot" "scala -cp .:hlt MyBot"
