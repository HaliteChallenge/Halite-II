#!/usr/bin/env bash

set -e

GCLOUD_PROJECT="nth-observer-171418"
GCLOUD_ZONE="us-central1-c"

SERVICE_ACCOUNT="apiserver"

MACHINE_TYPE="f1-micro"
IMAGE="worker3"

# TODO: create our own service account?
# TODO: document what the service account needs

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-templates create "coordinator-instance-template" \
    --machine-type "${MACHINE_TYPE}" \
    --network "default" \
    --metadata "^#&&#^startup-script=$(cat setup_coordinator__startup_script.sh)" \
    --maintenance-policy "MIGRATE" \
    --service-account "${SERVICE_ACCOUNT}@${GCLOUD_PROJECT}.iam.gserviceaccount.com" \
    --image "${IMAGE}" --image-project "${GCLOUD_PROJECT}" \
    --boot-disk-size "10" --boot-disk-type "pd-standard" \
    --scopes "https://www.googleapis.com/auth/sqlservice.admin","https://www.googleapis.com/auth/servicecontrol","https://www.googleapis.com/auth/service.management.readonly","https://www.googleapis.com/auth/logging.write","https://www.googleapis.com/auth/monitoring.write","https://www.googleapis.com/auth/trace.append","https://www.googleapis.com/auth/devstorage.read_write"

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-groups managed create "coordinator-instances" \
    --zone "${GCLOUD_ZONE}" \
    --base-instance-name "coordinator-instances" \
    --template "coordinator-instance-template" --size "1"

gcloud compute --project "${GCLOUD_PROJECT}" \
    instance-groups managed set-autoscaling "coordinator-instances" \
    --zone "${GCLOUD_ZONE}" \
    --cool-down-period "60" \
    --max-num-replicas "1" --min-num-replicas "1" \
    --target-cpu-utilization "0.8"