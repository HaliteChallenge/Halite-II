#!/bin/bash

if ! screen -list | grep -q "worker"; then
    screen -S worker -Ldm bash -c "python3 worker.py"
fi
