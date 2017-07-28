#!/usr/bin/env bash

set -e

source ./config.sh

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-templates create "worker-instance-template" \
    --machine-type "${MACHINE_TYPE}" \
    --network "default" \
    --metadata "^#&&#^halite-manager-url=${COORDINATOR_URL}#&&#halite-secret-folder=${SECRET_FOLDER}#&&#startup-script=$(cat setup_workers__startup_script.sh)" \
    --no-restart-on-failure \
    --no-service-account --no-scopes \
    --maintenance-policy "TERMINATE" \
    --preemptible \
    --tags "worker" \
    --image-family "${IMAGE}" --image-project "${GCLOUD_PROJECT}" \
    --boot-disk-size "10" --boot-disk-type "pd-standard"

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-groups managed create "worker-instances" \
    --zone "${GCLOUD_ZONE}" \
    --base-instance-name "worker-instances" \
    --template "worker-instance-template" --size "1"

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-groups managed set-autoscaling "worker-instances" \
    --zone "${GCLOUD_ZONE}" \
    --cool-down-period "60" \
    --max-num-replicas "2" --min-num-replicas "1" \
    --target-cpu-utilization "0.8"
