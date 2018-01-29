#!/usr/bin/env bash

# We are running as root - run as our worker user instead

sudo -iu worker bash <<"EOF"
cd
# Fetch the worker script
while [ ! -f ./Halite.tgz ]; do
    sleep 5
    curl -v http://10.142.0.5:5001/v1/coordinator/download/worker --output Halite.tgz
done

tar xvzf Halite.tgz

cd Halite/apiserver/worker
bash setup.sh

EOF