#!/usr/bin/env bash

# We are already running as root

# Fetch the worker script
# TODO: This is in GCloud right now, but we should use Git when we go public
gsutil cp gs://dml339-test-worker-storage/worker.tgz .

tar xvzf worker.tgz
cd Halite/apiserver/worker

# Grab configuration values
python3 grab_config.py

# Start the worker
screen -S worker -d -m /bin/bash -c "python3 worker.py"
