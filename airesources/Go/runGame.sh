#!/bin/bash

export GOPATH="$(pwd)"

./halite -d "30 30" "go run MyBot.go" "go run RandomBot.go"
