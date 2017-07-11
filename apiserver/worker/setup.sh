#!/usr/bin/env bash

cd ../../environment
make -j2

cd ../apiserver/worker

cp ../../environment/halite .

# Grab configuration values
python3 grab_config.py

# Start the worker
screen -S worker -d -m /bin/bash -c "python3 worker.py"