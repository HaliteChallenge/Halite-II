#!/bin/bash

cargo build
./halite -d "30 30" "target/debug/MyBot" "target/debug/RandomBot"
