#!/bin/bash

scalac *.scala

./halite -d "30 30" "scala MyBot" "scala RandomBot"
