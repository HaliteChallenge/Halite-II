"""
Grab worker configuration from GCloud instance attributes.
"""
import json
import requests


MANAGER_URL_METADATA_URL = "http://metadata.google.internal/computeMetadata/v1/instance/attributes/halite-manager-url"
SECRET_FOLDER_METADATA_URL = "http://metadata.google.internal/computeMetadata/v1/instance/attributes/halite-secret-folder"

MANAGER_URL = requests.get(MANAGER_URL_METADATA_URL, headers={
    "Metadata-Flavor": "Google"
}).text
SECRET_FOLDER = requests.get(SECRET_FOLDER_METADATA_URL, headers={
    "Metadata-Flavor": "Google"
}).text

with open("config.json", "w") as configfile:
    json.dump({
        "MANAGER_URL": MANAGER_URL,
        "SECRET_FOLDER": SECRET_FOLDER,
    }, configfile)
