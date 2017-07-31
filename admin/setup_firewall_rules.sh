#!/usr/bin/env bash

set -e

source ./config.sh

# Allow web traffic to the coordinator on the port used for the API server.
# TODO: we should whitelist the load balancer only
gcloud beta compute --project "${GCLOUD_PROJECT}" \
    firewall-rules create "allow-coordinator-external-traffic" \
    --allow tcp:5000 --direction "INGRESS" --priority "1000" \
    --network "default" --source-ranges "0.0.0.0/0" --target-tags "coordinator"

# Allow traffic to the coordinator on the port used for the coordinator server from the workers only.
# TODO: we should whitelist the load balancer only
gcloud beta compute --project "${GCLOUD_PROJECT}" \
    firewall-rules create "allow-coordinator-internal-traffic" \
    --allow tcp:5001 --direction "INGRESS" --priority "1000" \
    --network "default" --source-tags "worker" --target-tags "coordinator"

# Deny traffic to the coordinator on the coordinator server port for anything else.
gcloud beta compute --project "${GCLOUD_PROJECT}" \
    firewall-rules create "disallow-coordinator-internal-traffic" \
    --action deny --rules tcp:5001 --direction "INGRESS" --priority "2000" \
    --network "default" --source-ranges "0.0.0.0/0" --target-tags "coordinator"