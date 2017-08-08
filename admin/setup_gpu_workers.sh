#!/usr/bin/env bash

set -e

source ./config.sh

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-templates create "gpu-worker-instance-template" \
    --machine-type "${GPU_MACHINE_TYPE}" \
    --accelerator type=nvidia-tesla-k80,count=1 \
    --no-service-account --no-scopes \
    --network "default" --no-address \
    --metadata "^#&&#^halite-manager-url=${COORDINATOR_URL}#&&#halite-gpu=true#&&#halite-secret-folder=${SECRET_FOLDER}#&&#startup-script=$(cat setup_workers__startup_script.sh)" \
    --maintenance-policy "TERMINATE" \
    --tags "worker" \
    --image-family "${GPU_IMAGE}" --image-project "${GCLOUD_PROJECT}" \
    --boot-disk-size "50" --boot-disk-type "pd-standard"

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-groups managed create "gpu-worker-instances" \
    --zone "${GCLOUD_GPU_ZONE}" \
    --base-instance-name "gpu-worker-instances" \
    --template "gpu-worker-instance-template" --size "1"
