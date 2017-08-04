#!/usr/bin/env bash

set -e

make MyBot
./halite -d "256 256" "./MyBot" "./MyBot"