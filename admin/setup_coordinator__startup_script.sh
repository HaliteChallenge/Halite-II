#!/usr/bin/env bash

# We are already running as root

# Fetch the worker script
# TODO: This is in GCloud right now, but we should use Git when we go public
gsutil cp gs://dml339-test-worker-storage/coordinator.tgz .

tar xvzf worker.tgz
cd Halite/apiserver/

sudo apt-get -yq install virtualenv
virtualenv --python=python3.6 venv

source venv/bin/activate
pip install -r requirements.txt

wget -O cloud_sql_proxy https://dl.google.com/cloudsql/cloud_sql_proxy.linux.amd64
chmod +x ./cloud_sql_proxy

screen -S sqlproxy -d -m -X /bin/bash -c \
    './cloud_sql_proxy -instances=nth-observer-171418:us-central1:dml339-test-database=tcp:3307 '

screen -S coordinator -d -m -X /bin/bash -c \
    'source venv/bin/activate && FLASK_APP=apiserver flask run -h 0.0.0.0 '
