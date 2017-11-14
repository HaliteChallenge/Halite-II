import os
import zstd
import re

import requests
import multiprocessing
from concurrent.futures.thread import ThreadPoolExecutor

from . import client

_ITEMS_KEY = 'items'
_SELFLINK_KEY = 'selfLink'
_REPLAY_KEY = 'replay'
_REPLAY_CLASS_KEY = 'replay_class'
_MEDIA_DOWNLOAD_OPTION = '?alt=media'
_PREFIX_OPTION = '?prefix='

_BUCKET_POSITION = -3
_OBJECT_POSITION = -1

_REPLAY_PREPEND = 'replay-'
_PATH_DELIMITER = '/'


class GameDownloader:
    _GOLD_BUCKET_URI = 'https://www.googleapis.com/storage/v1/b/halite-2-gold-replays/o'
    _SALT_BUCKET_URI = 'https://www.googleapis.com/storage/v1/b/halite-2-replays/o'
    _BUCKET_URIS = [_SALT_BUCKET_URI, _GOLD_BUCKET_URI]

    def __init__(self, destination, buckets, prefix):
        """
        Download replays files
        :param destination: Where to download
        :param buckets: List of bucket(s) to fetch from
        :param prefix: What prefix to fetch from
        """
        if not os.path.isdir(destination):
            raise FileNotFoundError("Directory path does not exist")
        self.destination = destination
        self.objects = []
        for bucket in buckets:
            self.objects += self._parse_objects(requests.get(bucket + _PREFIX_OPTION + prefix).json())

    @staticmethod
    def _parse_objects(bucket_json):
        """
        Parse GCS response to get URIs for objects
        :param bucket_json: The response from GCS
        :return: parse URIs for objects
        """
        response = []
        for bucket_object in bucket_json[_ITEMS_KEY]:
            response.append(bucket_object[_SELFLINK_KEY])
        return response

    @staticmethod
    def _unzip(game_id, game_binary):
        """
        Takes a zstd file and unzips it
        :param game_id: The unique id for the game object (name of resulting file)
        :param game_binary: The zipped binary
        :return: the file unzipped if possible
        """
        try:
            return zstd.loads(game_binary).decode()
        except Exception:
            raise ValueError("Could not unzip file at: {}!".format(game_id))

    @staticmethod
    def _build_object_uri(bucket_class, object_id):
        """
        Creates a GCS URI from the bucket id and object id
        :param bucket_class: The bucket id in GCS
        :param object_id: The object id in GCS
        :return: the constructed GCS URI
        """
        return "{}/{}".format(GameDownloader._BUCKET_URIS[bucket_class], object_id)

    @staticmethod
    def _parse_id_from_url(url):
        """
        Take a GCS URL and transform into a filename
        :param url: the GCS URL
        :return: the constructed filename
        """
        split_url = url.split(_PATH_DELIMITER)
        return "{}_{}".format(split_url[_BUCKET_POSITION], split_url[_OBJECT_POSITION])

    def _get_object(self, url):
        """
        Download a single object from GCS considering the designated URL and save it to de destination
        :param url: The url do download from
        :return: Nothing
        """
        game_id = self._parse_id_from_url(url)
        try:
            with open(os.path.join(self.destination, game_id), 'w') as fout:
                print("downloading {}".format(url))
                fout.writelines(self._unzip(game_id, requests.get(url + _MEDIA_DOWNLOAD_OPTION).content))
        except Exception:
            raise IOError("Could not write file {} to {}".format(game_id, self.destination))

    def get_objects(self):
        """
        Download all desired replays in parallel threads (up to the number of cores the machines has)
        :return: Nothing
        """
        with ThreadPoolExecutor(max_workers=multiprocessing.cpu_count()) as executor:
            for url in self.objects:
                executor.submit(self._get_object, url)


class DatedGameDownloader(GameDownloader):

    def __init__(self, destination, date, all_bots=False):
        """
        Download games for a date
        :param destination: Where to download
        :param date: Which date to download
        :param all_bots: True if you wish to download silver ranked bots as well. False for only gold.
        """
        buckets = [self._GOLD_BUCKET_URI] + ([self._SALT_BUCKET_URI] if all_bots else [])
        super(DatedGameDownloader, self).__init__(destination, buckets, _REPLAY_PREPEND + date)


class UserGameDownloader(GameDownloader):
    _USER_BOT_URI = 'https://api.halite.io/v1/api/user/{}/match?limit={}&offset={}'
    _FETCH_THRESHOLD = 250
    _BUCKETS = []

    def __init__(self, destination, user_id, limit):
        """
        Download games for a user
        :param destination: Where to download
        :param user_id: Which user's replays to fetch
        :param limit: How many replays to fetch (max)
        """
        self.destination = destination
        self.objects = self._parse_user_metadata(self._fetch_metadata(user_id, limit))

    def _fetch_metadata(self, user_id, limit):
        """
        Retrieves paginated game metadata from the halite servers for a specified user up to limit items
        :param user_id: The id of the user to fetch
        :param limit: The maximum number of items to fetch
        :return: The full metadata of items
        """
        print('Fetching Metadata')
        current = 0
        result_set = []
        while current <= limit:
            current_limit = self._FETCH_THRESHOLD if ((limit - current) >= self._FETCH_THRESHOLD) else (limit - current)
            result_set += requests.get(self._USER_BOT_URI.format(user_id, current_limit, current)).json()
            current += self._FETCH_THRESHOLD
        print('Finished metadata fetch. Found {} game files.'.format(len(result_set)))
        return result_set

    @staticmethod
    def _parse_user_metadata(user_json):
        """
        Takes response from API server and parses to get all user replays
        :param user_json: The response from the API server
        :return: the paths to the bucket objects with the replays for the user
        """
        response = []
        for user_object in user_json:
            response.append(GameDownloader._build_object_uri(user_object[_REPLAY_CLASS_KEY], user_object[_REPLAY_KEY]))
        return response


def _valid_date(date):
    """
    Whether the date requested matches the desired format (starts between 1 and 8 digits)
    :param date: The date to check
    :return: True if valid, false otherwise
    """
    return re.compile('^\d{1,8}').search(date)


def download(mode, destination, date, all_bots, default_user_id, user_id, limit):
    """
    Downloads bot replay files matching the designated requirements
    :param mode: Whether to download files matching a date or a user id
    :param destination: Where to download the files to
    :param date: Which date to download the files to, if there is a date
    :param all_bots: If dated, whether to download all bots (silver/gold)
    :param default_user_id: What is the user id of the user making the request
    :param user_id: What is the user id desired if any
    :param limit: How many replays to download (currently only in user mode)
    :return: Nothing
    """
    print('Downloading game files')
    if mode == client.REPLAY_MODE_DATE:
        if not _valid_date(date):
            raise ValueError("Date must match format YYYYMMDD")
        DatedGameDownloader(destination, date, all_bots).get_objects()
    elif mode == client.REPLAY_MODE_USER:
        if not (default_user_id or user_id):
            raise ValueError("Cannot run default mode without authenticating .Please run `client.py --auth` first.")
        UserGameDownloader(destination, default_user_id if not user_id else user_id, limit).get_objects()
    print('Finished writing files to desired location')

