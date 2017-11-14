#!/bin/bash

scalac *.scala hlt/*.scala

./halite -d "240 160" "scala MyBot" "scala MyBot"
