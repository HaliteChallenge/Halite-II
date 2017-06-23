#!/bin/bash

if ! screen -list | grep -q "worker"; then
    # TODO: fix this path
    screen -S worker -Ldm bash -c "cd /worker; python3 worker.py"
fi
