#!/usr/bin/env bash

# This script is essentially the second half of the coordinator's startup
# script.

virtualenv --python=python3.6 venv

source venv/bin/activate
pip install -r requirements.txt

wget -O cloud_sql_proxy https://dl.google.com/cloudsql/cloud_sql_proxy.linux.amd64
chmod +x ./cloud_sql_proxy

# Get the spec for the DB instance to pass to Cloud SQL Proxy
DB_INSTANCE="$(python -m apiserver.scripts.print_db_proxy_instance)"

screen -S sqlproxy -d -m /bin/bash -c \
    "./cloud_sql_proxy -instances=${DB_INSTANCE}=tcp:3307"

screen -S api -d -m /bin/bash -c \
    "PYTHONPATH=$(pwd) gunicorn -w 8 -b 0.0.0.0:5000 --log-level=info apiserver.server:app"

screen -S coordinator_internal -d -m /bin/bash -c \
    "PYTHONPATH=$(pwd) gunicorn -w 8 -b 0.0.0.0:5001 --log-level=info apiserver.coordinator_server:app"

screen -S badge_daemon -d -m /bin/bash -c \
    "PYTHONPATH=$(pwd) python3 -m apiserver.scripts.badge_daemon.py"
