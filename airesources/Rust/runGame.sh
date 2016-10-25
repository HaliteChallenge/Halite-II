#!/bin/bash

cargo build
./environment -d 30 30 "target/debug/MyBot" "target/debug/RandomBot"
