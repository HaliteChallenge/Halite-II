#!/usr/bin/env bash

GCLOUD_PROJECT="halite-2"
GCLOUD_ZONE="us-east1-b"
GCLOUD_GPU_ZONE="us-east1-c"

WEBSITE_MACHINE_TYPE="n1-standard-2"
MACHINE_TYPE="custom-4-6144"
GPU_MACHINE_TYPE=${MACHINE_TYPE}
IMAGE="halite-worker"
GPU_IMAGE="halite-gpu-worker"
WEBSITE_IMAGE="website-image-xiii"

SERVICE_ACCOUNT="apiserver"
COORDINATOR_URL="http://10.142.0.5:5001/v1/coordinator/"
SECRET_FOLDER=""
