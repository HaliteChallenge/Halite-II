#!/usr/bin/env bash

# On GPU workers, environment variables needed to set up CUDA properly are
# written to the profile, so make sure those variables actually get loaded.
source ~/.profile

cd ../../environment

cmake -H. -Bbuild
cmake --build build -- -j2

cp build/halite .

cd ../apiserver/worker

cp ../../environment/halite .

# Grab configuration values
python3 grab_config.py

# Start the worker
screen -S worker -d -m /bin/bash -c "python3 worker.py"