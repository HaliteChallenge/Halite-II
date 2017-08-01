# Setting up the Game Coordinator

For website help, see `website/README.md`.

## Prerequisites

1. Create a Google Cloud project.
1. Make sure the Google Cloud command line client is installed.
1. Set up the client with the project and necessary credentials.

Prefer zone us-east1-b.

### Create the Worker Image

We need to create a machine image with all necessary compilers and other things set up already. Create an instance based on Ubuntu 17.04, or the latest version of Ubuntu available. Under "Management, disks, networking, and SSH keys", uncheck "Delete boot disk". Once the instance is started, run [`setup_worker_image.sh`](./setup_worker_image.sh), then delete the instance and create an image from its boot disk. Make sure this image is part of an image family, e.g. `halite-worker`.

At the end, the script also prints out all installed packages with their versions, which is useful for documentation.

For the GPU instances, boot up a VM using the worker image, also making sure to uncheck "Delete boot disk". Make sure this VM has more disk space (~30 GB). Run [`setup_gpu_worker_image.sh`](./setup_gpu_worker_image.sh), then delete the instance and create an image from its boot disk. Also make sure this image is part of an image family, e.g. `halite-gpu-worker`.

### Create GCS Buckets

We need five buckets: one for compiled bot storage(`halite-2-compiled-bots`), one for uploaded bots(`halite-2-uploaded-bots`), two for replays (`halite-2-replays` and `halite-2-gold-replays`), and one for error logs (`halite-2-error-logs`). (You may also need one for storing the worker/coordinator, when deploying without being able to pull from Github. Right now, this is `halite-2-deployed-artifacts`.) Create the buckets and put their names in `apiserver/apiserver/config.py`.

### Upload Coordinator and Worker to GCS (Non-Github Deploy Only)

Zip the Halite repository folder into a .tgz file and upload it as `Halite.tgz` in the root of the GCS bucket (currently `halite-2-deployed-artifacts`).

The structure of the `tgz` should be as follows:

    Halite/
        apiserver/
            apiserver/
                config.py
                server.py
                coordinator_server.py
                ⋮
            worker/
            ⋮
        ⋮

MAKE SURE `Halite/apiserver/apiserver/config.py` is present and has production values.

## MySQL Server

1. Create a Google Cloud SQL MySQL (second generation) instance.
    1. Use MySQL 5.7.
1. Use `schema.sql` to create the initial database. (You can copy-paste it into console.)
    1. Optionally, use `dummyData.sql` to set up some test data.
1. Create an account and put its credentials into `apiserver/apiserver/config.py`. Also add the project ID of the project, the database was created in, and the database instance name.
1. Enable the Cloud SQL Administration API: https://console.developers.google.com/apis/api/sqladmin.googleapis.com/overview

## Creating the Coordinator Instances

These steps should be run only once.

### Create the Service Account

We need a service account so the coordinators can access everything they need.

- Cloud SQL Client
- Cloud SQL Editor
- Cloud SQL Viewer
- Storage Admin
- Storage Object Admin
- Storage Object Creator
- Storage Object Viewer

### Create the Instance Group

This sets up an automatically scaling pool of game coordinator servers.

1. In `admin/config.sh`, set up the project ID, region, and service account created earlier.
    1. Make sure `IMAGE` is set to the image family created earlier. The instances will boot with the latest image in this family.
1. Run `admin/setup_firewall_rules.sh` from your local machine. This will create the necessary firewall rules for the coordinator and worker.
1. Run `admin/setup_coordinator.sh` from your local machine. This will create an instance template and an instance group `coordinator-instances`.

### External Load Balancing

This load balancer is for the user-facing REST API.

1. Networking > Load Balancing > Create load balancer
1. Choose an HTTP(S) load balancer.
1. Create a new backend service, based on the instance group `coordinator-instances`.
    1. Set the balancing mode to Rate (so we can create an internal balancer for the coordinator)
    1. Set the port number to 5000
    1. Create a new health check. Use HTTP to check the url `/health_check` on port 5000.
1. Leave host/path rules default.
1. Leave the frontend as default.

### Internal Load Balancer

This load balancer is for the internal game coordinator.

1. Networking > Load Balancing > Create load balancer
1. Choose a TCP load balancer, "Only between my VMs".
1. Choose the existing backend service.
    1. Create a new health check. Use HTTP to check the url `/health_check` on port 5001.
1. Set the frontend to use port 5001.

Create the load balancer and note the IP address of the balancer.

## Creating the Worker Instances

These steps should be run only once.

1. In `admin/config.sh`, set up the project ID, region, and details of the machine instances (same as before). 
1. Set the coordinator URL to the URL of the internally facing TCP load balancer created earlier.
1. Run [`admin/setup_workers.sh`](./setup_workers.sh).

### Creating GPU Worker Instances

1. In `admin/config.sh`, double check `GPU_MACHINE_TYPE` (the machine type you would like to use for the GPU instances), and make sure `GPU_IMAGE` is set to the image family of the GPU disk images. The coordinator URL should be set up from before.
1. Run [`setup_gpu_workers.sh`](./setup_gpu_workers.sh). This will create the instance template and instance group. Unlike the regular workers, this group will not autoscale.

At this point, everything should be set. You can `ssh` into individual instances to check on them. `sudo su` to switch to `worker`, and use `screen -list` to see what sessions are running. Coordinator servers should have 3 screen sessions, one for the SQL proxy, one for the API server, and one for the coordinator. Workers should have 1 session, for the worker itself. These scripts create the necessary firewall rules as well.

## Redeploying/Updating

### Add Packages to the Image

Edit [`setup_worker_image.sh`](./setup_worker_image.sh). If the package can be installed via apt-get, then add it to the `PACKAGES` variable, so that the script will print out the version installed at the end. Otherwise, add the necessary installation commands, and print out the version at the end; this will depend on what exactly is being added.

Afterwards, create a new image in the same family; deprecate the previous one. The instance template is built against the family, so when the instances are recreated, they should use the new image.

### Updating the Coordinator, API Server, or Worker

The startup scripts for the coordinator/API server/worker pull the Halite code from a Google Cloud bucket, extract it, and start it. So long as the startup script does not need to change, you can update these services by:

1. Create a new "Halite.tgz", as described previously.
1. Upload the new "Halite.tgz" to the GCloud bucket.
1. Recreate the relevant instances (see below).

If the startup script changes, we have to create a new instance template, as follows:

1. Upload the new code to the GCloud bucket, if applicable (see above).
1. Create a new instance template with a new name, based on the settings of the previous one. (Take the relevant `gcloud` command from the `setup_*.sh` script and edit the name and any other parameters.)

        # In admin/ directory
        source ./config.sh
        gcloud compute --project "${GCLOUD_PROJECT}" \
            instance-templates create "worker-instance-template" \
        ⋮

1. Edit the instance group in the Google Cloud Console to use the new instance template.
1. Delete the old instance template.
1. Recreate all instances in the group (see below).

   When you edit the instance group, the console will give you the necessary gcloud command to run.

Otherwise, if the code or disk image change:

1. Upload the new code, if applicable.
2. Recreate the instances using the GCloud CLI.

### Recreating Instances

    gcloud compute instance-groups managed recreate-instances coordinator-instances --instances=<list instance IDs here>
    
Change `coordinator-instances` to the appropriate group. For coordinator servers, you probably want to recreate them one-by-one, to make sure the game API does not go down during the process.

