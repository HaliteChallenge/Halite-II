#!/usr/bin/env bash

# We are running as root - run as our worker user instead

sudo -iu worker bash <<"EOF"
cd
# Fetch the worker script
# TODO: This is in GCloud right now, but we should use Git when we go public
while [ ! -f ./Halite.tgz ]; do
    sleep 5
    gsutil cp gs://halite-2-deployed-artifacts/Halite.tgz .
done

tar xvzf Halite.tgz

cd Halite/apiserver/worker
bash setup.sh

EOF