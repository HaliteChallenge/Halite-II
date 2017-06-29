# Setting up the Game Coordinator

## Prerequisites

1. Create a Google Cloud project.
1. Make sure the Google Cloud command line client is installed.
1. Set up the client with the project and necessary credentials.

Prefer zone us-central1-b.

### Deployment Service Account

We need a service account to use locally.

### Create the Worker Image

We need to create a machine image with all necessary compilers and other things set up already. Create an f1-micro instance based on Ubuntu 17.04. Under "Management, disks, networking, and SSH keys", uncheck "Delete boot disk". Once the instance is started, run `setup_worker_image.sh`, then delete the instance and create an image from it.

At the end, the script also prints out all installed packages with their versions, which is useful for documentation.

### Create GCS Buckets

We need four buckets: one for compiled bot storage, one for uploaded bots, one for replays, and one for error logs. (You may also need one for storing the worker/coordinator, when deploying without being able to pull from Github.) Create the buckets and put their names in `apiserver/apiserver/config.py`.

### Upload Coordinator and Worker to GCS (Non-Github Deploy Only)

## MySQL Server

1. Create a Google Cloud SQL MySQL (second generation) instance.
    1. Use MySQL 5.7.
1. Use `schema.sql` to create the initial database. (You can copy-paste it into console.)
    1. Optionally, use `dummyData.sql` to set up some test data.
1. Create an account and put its credentials into `apiserver/apiserver/config.py`. Also add the project ID of the project, the database was created in, and the database instance name.

## Creating the Coordinator Instances

### Create the Service Account

We need a service account so the coordinators can access everything they need.

These are the permissions I currently have, but these are probably more than necessary:

- Cloud SQL Client
- Cloud SQL Editor
- Cloud SQL Viewer
- Storage Object Creator
- Storage Object Viewer

- Cloud SQL Admin
- Cloud SQL Client
- Cloud SQL Editor
- Cloud SQL Viewer
- Compute Image User
- Compute Instance Admin (v1)
- Compute Network Admin
- Compute Network User
- Compute Network Viewer
- Compute Security Admin
- Compute Security Admin
- Compute Storage Admin
- Service Account Actor
- Storage Admin
- Storage Object Admin

(I think most of these are needed for the CLI in order to run the scripts below. The server itself shouldn't need more than access to Cloud SQL and Storage.)

### Create the Instance Group

This sets up an automatically scaling pool of game coordinator servers.

1. In `admin/setup_coordinator.sh`, set up the project ID, region, and service account created earlier.
1. Run the script.

### External Load Balancing

This load balancer is for the user-facing REST API.

1. Networking > Load Balancing > Create load balancer
1. Choose an HTTP(S) load balancer.
1. Create a new backend service, based on the instance group `coordinator-instances`.
    1. Set the balancing mode to Rate (so we can create an internal balancer for the coordinator)
    1. Set the port number to 5000
    1. Create a health check using HTTP. Have it request the url `/api/v1/users` on port 5000.
1. Leave host/path rules default.
1. Leave the frontend as default.

### Internal Load Balancer

This load balancer is for the internal game/compilation workers.

1. Networking > Load Balancing > Create load balancer
1. Choose a TCP load balancer, "Only between my VMs".
1. Choose the existing backend service.
    1. Create a new health check. Use TCP to check port 5001.
1. Set the frontend to use port 5001.

Create the load balancer and note the IP address of the balancer.

1. Create a firewall rule to allow access to the game coordinator from the load balancer.
    1. Target tag: `coordinator`
    1. Source filters: the subnet of the load balancer

## Creating the Worker Instances

1. In `admin/setup_workers.sh`, set up the project ID, region, and details of the machine instances. 
1. Set the coordinator URL to the URL of the externally facing HTTP load balancer created earlier.
1. Run the script.

At this point, everything should be set. You can `ssh` into individual instances to check on them. `sudo su` to switch to root, and use `screen -list` to see what sessions are running. Coordinator servers should have 3 screen sessions, one for the SQL proxy, one for the API server, and one for the coordinator. Workers should have 1 session, for the worker itself.