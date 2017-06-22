import os
import random

import googleapiclient

from googleapiclient import discovery
from oauth2client.client import GoogleCredentials

from .. import config, model


WORKER_STARTUP_SCRIPT_PATH =os.path.join(os.path.dirname(__file__),
                                         "worker_startup.sh")
with open(WORKER_STARTUP_SCRIPT_PATH) as worker_startup_script_file:
    WORKER_STARTUP_SCRIPT = worker_startup_script_file.read()


def create_worker(name, *, verbose=False):
    compute = googleapiclient.discovery.build('compute', 'v1')
    credentials = GoogleCredentials.get_application_default()

    service = discovery.build('compute', 'beta', credentials=credentials)

    # TODO: change this to our custom image
    image = compute.images().getFromFamily(
        project=config.GCLOUD_PROJECT_ID,
        family=config.GCLOUD_WORKER_IMAGE_FAMILY).execute()
    source_disk_image = image['selfLink']

    project = config.GCLOUD_PROJECT_ID
    zone = config.GCLOUD_ZONE

    if verbose:
        print("Project:", project)
        print("Zone   :", zone)
        print("Image  :", source_disk_image)
        print("Name   :", name)

    # Generate the API key for the worker
    # TODO: better way of doing this (this is ported from Halite I)
    num_rows = 9999
    conn = model.engine.connect()
    query = lambda key: \
        model.workers.select().where(model.workers.c.apiKey == key)
    api_key = None
    while num_rows != 0:
        api_key = random.randrange(0, 10000000)
        num_rows = len(conn.execute(query(api_key)).fetchall())

    conn.execute(model.workers.insert().values(
        apiKey=api_key,
        ipAddress=0,  # TODO: get rid of this field
    ))

    instance_body = {
        'name': name,
        'machineType': "zones/{}/machineTypes/{}".format(
            zone,
            config.GCLOUD_WORKER_MACHINE_TYPE),

        # Specify the machine image.
        'disks': [
            {
                'boot': True,
                'autoDelete': True,
                'initializeParams': {
                    'sourceImage': source_disk_image,
                }
            }
        ],

        # Give the instance public Internet access via NAT.
        'networkInterfaces': [{
            'network': 'global/networks/default',
            'accessConfigs': [
                {'type': 'ONE_TO_ONE_NAT', 'name': 'External NAT'}
            ]
        }],

        # Give cloud storage/logging access
        'serviceAccounts': [{
            'email': 'default',
            'scopes': [
                'https://www.googleapis.com/auth/devstorage.read_write',
                'https://www.googleapis.com/auth/logging.write'
            ]
        }],

        'metadata': {
            'items': [{
                # Use startup script to set up the worker, instead of relying
                # on SSH as we did before.
                'key': 'startup-script',
                'value': WORKER_STARTUP_SCRIPT
            }, {
                'key': 'halite-api-key',
                'value': api_key,
            }]
        }
    }

    request = service.instances().insert(project=project, zone=zone,
                                         body=instance_body)
    response = request.execute()
    if verbose:
        print(response)
    return response


def main():
    create_worker("halite-worker-" + str(random.randrange(0, 1000000000)),
                  verbose=True)


if __name__ == "__main__":
    main()