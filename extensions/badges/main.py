import time
import requests
import json
import os
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from google.cloud import storage

LOCAL_DEBUG = 'false'

# move to key management soon, create a new key and disable the current one
CREDLY_KEY = ''
CREDLY_SECRET = ''
USER = ''
PASSWORD = ''
AUTH_TOKEN = ''
MEMBER_ID = ''

# google cloud details
PROJECT = ''
BADGE_BUCKET = 'credlybadges'

CREDLY_BASE_API_URL = 'https://api.credly.com/v1.1/'

HEADERS = {
    'x-api-key': CREDLY_KEY,
    'x-api-secret': CREDLY_SECRET,
    'authorization': "Basic aGFsaXRlQGhhbGl0ZS5pbzpTZ205NzVwalQ2djNHdXlqWEUwbQ==",
    'cache-control': "no-cache",
}

CLOUDSQL_USER = ''
CLOUDSQL_PASSWORD = ''
CLOUDSQL_CONNECTION_NAME = 'halite-2:us-east1:halite-db'

# authenticate every time, regnerating token every time given the kehy expires every 3 years
def getAuthToken():
    response = requests.request("POST", CREDLY_BASE_API_URL + 'authenticate', headers=HEADERS)
    responsebody = json.loads(response.text)
    authdata = responsebody['data']
    AUTH_TOKEN = authdata.values()[0]
    return AUTH_TOKEN

def getMemberID():
    response = requests.request("GET", CREDLY_BASE_API_URL + 'me?access_token=' + getAuthToken(), headers=HEADERS)
    responsebody = json.loads(response.text)
    memberdata = responsebody['data']
    MEMBER_ID = memberdata[u'id']
    print(MEMBER_ID)

def cacheBadgeImagesInCloud():
    storage_client = storage.Client(PROJECT)
    bucket = storage_client.get_bucket(BADGE_BUCKET)
    response = requests.request("GET", CREDLY_BASE_API_URL + 'me/badges/created?access_token=' + getAuthToken()+ '&per_page=50', headers=HEADERS)
    responsebody = json.loads(response.text)
    badgedata = responsebody[u'data']
    for x in badgedata:
        imageresponse = requests.request("GET",x[u'image_url'])
        imagefilename = str(x[u'id']) + '.png'
        imagefile = open(imagefilename, 'w')
        imagefile.write(bytes(imageresponse.content))
        imagefile.close()
        blob = bucket.blob(imagefilename)
        blob.upload_from_filename(imagefilename)
        if (LOCAL_DEBUG == 'false'):
            os.remove(imagefilename)

    def giveBadges(userid, badgeid):
        t = 1


    def getbadges(userid):
        t = 1

if __name__ == '__main__':
    cacheBadgeImagesInCloud()
