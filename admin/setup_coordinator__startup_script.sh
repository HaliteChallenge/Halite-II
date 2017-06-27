#!/usr/bin/env bash

# We are already running as root

# Fetch the coordinator
# TODO: This is in GCloud right now, but we should use Git when we go public
gsutil cp gs://dml339-test-worker-storage/coordinator.tgz .

tar xvzf coordinator.tgz
cd apiserver/

virtualenv --python=python3.6 venv

source venv/bin/activate
pip install -r requirements.txt

wget -O cloud_sql_proxy https://dl.google.com/cloudsql/cloud_sql_proxy.linux.amd64
chmod +x ./cloud_sql_proxy

DB_INSTANCE="$(python -m apiserver.scripts.print_db_proxy_instance)"

screen -S sqlproxy -d -m /bin/bash -c \
    "./cloud_sql_proxy -instances=${DB_INSTANCE}=tcp:3307"

screen -S coordinator -d -m /bin/bash -c \
    "PYTHONPATH=$(pwd) FLASK_APP=apiserver.server FLASK_DEBUG=true flask run -h 0.0.0.0"
