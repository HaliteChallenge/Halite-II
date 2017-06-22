#!/usr/bin/env bash

# We are already running as root

# Fetch the worker script
# TODO: This is in GCloud right now, but we should use Git when we go public

cd worker/

# Grab configuration values
python3 grab_config.py

# Start the worker
./startWorkerScreen.sh
