import time
import requests
import json
import os

from google.cloud import storage

PROJECT = 'halite-2'

# Amper docs - https://docs.ampermusic.com/jimmy/

# move to key management soon, create a new key and disable the current one
AMPER_KEY = 'wLntjWsRjS3PpZZsIKvbLvIbWTEdLwVkQ7kznKi3DrU5Y4Ut7mKmxQnMjTpYomLm'

MUSIC_BUCKET = 'ampermusichalite2'

AMPER_BASE_API_URL = 'https://jimmy.ampermusic.com/v1/'

LOCAL_DEBUG = 'false'

def ampergenerate():
    var = 1
    for x in range(0, 5):
        url = AMPER_BASE_API_URL
        payload = "{\n    \"timeline\":\n    {\n\t    \"events\": [\n\t        {\n\t            \"event\": \"region\",\n\t            \"id\": 1,\n\t            \"time\": 0,\n\t            \"descriptor\": \"epic_percussive_high\"\n\t        },\n\t        {\n\t            \"event\": \"silence\",\n\t            \"time\": 30.07\n\t        }\n\t    ]\n    }\n}"
        headers = {
            'authorization': "Bearer " + AMPER_KEY,
            'content-type': "application/json",
            'cache-control': "no-cache",
        }

        response = requests.request("POST", url + 'projects', data=payload, headers=headers)
        print(response.text)
        responsebody = json.loads(response.text)
        projectid = responsebody['id']

        if(responsebody['status'] == 'waiting_create'):
            # wait untill render is done
            while(var == 1):
                statusresponse = requests.request("GET", url + 'projects/' + str(projectid), data=payload, headers=headers)
                print(statusresponse.text)
                postcreatedata = json.loads(statusresponse.text)
                if (postcreatedata['status'] == 'waiting_create'):
                    time.sleep(5)
                    continue
                else:
                    # getting the mp3 version, can change to  wav if needed
                    file = postcreatedata['files'][1]

                    # dict keys are mangled, this is flaky, need to fix before release
                    rid = file['id']
                    response = requests.request("GET", url + 'render_files/' + str(rid), data=payload, headers=headers)
                    content = response.content
                    fileid = str(rid) + '.mp3'

                    # write audio files to disk for debugging.
                    audiofile = open(fileid, 'w')
                    audiofile.write(bytes(content))
                    audiofile.close()


                    # uploading to cloud storage
                    storage_client = storage.Client(PROJECT)
                    bucket_name = MUSIC_BUCKET
                    bucket = storage_client.get_bucket(bucket_name)
                    blob = bucket.blob(fileid)
                    blob.upload_from_filename(fileid)

                    print('File uploaded')

                    if (LOCAL_DEBUG == 'false'):
                        os.remove(fileid)
                    break

if __name__ == '__main__':
    ampergenerate()