#!/bin/bash

if ! screen -list | grep -q "worker"; then
    screen -S worker -dm bash -c "cd ~/Halite/worker; sudo python3 worker.py"
fi
