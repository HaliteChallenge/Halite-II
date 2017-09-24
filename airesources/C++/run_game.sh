#!/usr/bin/env bash

set -e

make MyBot
./halite -d "240 160" "./MyBot" "./MyBot"