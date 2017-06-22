"""
Grab worker configuration from GCloud instance attributes.
"""
import json
import requests


API_KEY_METADATA_URL = "http://metadata.google.internal/computeMetadata/v1/instance/attributes/halite-api-key"
MANAGER_URL_METADATA_URL = "http://metadata.google.internal/computeMetadata/v1/instance/attributes/halite-manager-url"

API_KEY = requests.get(API_KEY_METADATA_URL, headers={
    "Metadata-Flavor": "Google"
}).text
MANAGER_URL = requests.get(MANAGER_URL_METADATA_URL, headers={
    "Metadata-Flavor": "Google"
}).text

with open("config.json", "w") as configfile:
    json.dump({
        "API_KEY": API_KEY,
        "MANAGER_URL": MANAGER_URL,
    }, configfile)