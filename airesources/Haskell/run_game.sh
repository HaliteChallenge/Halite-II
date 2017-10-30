#!/usr/bin/env bash

ghc --make MyBot.hs -O -v0 -rtsopts -outputdir dist
./halite -d "240 160" "./MyBot" "./MyBot"
