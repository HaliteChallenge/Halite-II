#!/bin/sh

mix deps.get
mix escript.build
./halite -d "240 160" "./MyBot" "./MyBot"
