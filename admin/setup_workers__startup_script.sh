#!/usr/bin/env bash

# We are running as root - run as our worker user instead

sudo -iu worker bash <<"EOF"
cd
# Fetch the worker script
# TODO: This is in GCloud right now, but we should use Git when we go public
gsutil cp gs://halite-2-deployed-artifacts/worker.tgz .

tar xvzf worker.tgz

cd Halite/apiserver/worker
bash setup.sh

EOF