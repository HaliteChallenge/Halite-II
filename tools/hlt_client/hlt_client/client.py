#!/usr/bin/env python3

import re
import os
import sys
import json
import argparse
import requests
from json.decoder import JSONDecodeError

"""client.py: Client for interacting with the Halite II servers."""

__author__ = "Two Sigma"
__copyright__ = "Copyright 2017, Two Sigma"
__credits__ = ["David M. Li", "Jaques Clapauch"]
__date__ = "August 1, 2017"
__email__ = "halite@halite.io"
__license__ = "MIT"
__status__ = "Production"
__version__ = "1.0"


_URI_HALITE_API = 'http://35.190.3.178/v1/api'  # TODO change to cname (api.haliete.io)
_URI_API_CREATE_BOT = _URI_HALITE_API + "/user/{}/bot"
_URI_API_EXISTING_BOT = _URI_HALITE_API + "/user/{}/bot/{}"
_URI_HALITE_WEB_PAGE = 'http://35.185.45.87'
_URI_WEB_API_KEY = "{}/user/api_key".format(_URI_HALITE_WEB_PAGE)

_SUCCESS = 200
_FIRST_BOT_ID = 0
_BOT_FILE_KEY = 'botFile'
_API_KEY_HEADER = 'X-API-KEY'


class Config:
    _key_example = "<username>:<key>"
    _key_delimiter = ':'
    _user_position = 0

    _user_key = 'user'
    _api_key_key = 'api_key'

    def __init__(self, auth=None):
        self._config_folder = self._get_config_folder_path()
        self._auth_file = self._get_auth_file_path()
        if not os.path.exists(self._config_folder):
            os.makedirs(self._config_folder)
        if auth:
            auth = self._parse_api_key(auth)
            self._write_auth(auth)
        else:
            auth = self._get_auth_json()
        self.api_key = auth[self._api_key_key]
        self.user_id = auth[self._user_key]


    @staticmethod
    def _get_config_folder_path():
        """
        Returns system specific folder for config
        :return:  %LOCALAPPDATA%/Halite if windows ~/.config/hlt otherwise
        """
        return "{}/Halite".format(os.getenv('LOCALAPPDATA')) if sys.platform == 'win32' \
            else "{}/.config/hlt".format(os.path.expanduser("~"))

    @staticmethod
    def _get_auth_file_path():
        """
        :return: Auth file location where configs will be written to in JSON format
        """
        return "{}/auth".format(Config._get_config_folder_path())

    def _auth_exists(self):
        """
        Whether tha auth file has been created already
        :return: True if exists, False otherwise
        """
        return os.path.isfile(self._auth_file)

    def _write_auth(self, data):
        """
        Writes to auth file the desired data. Expected that the input be in JSON format.
        :param data: Data to be written to the auth file
        :return: Nothing
        """
        config_file = open(self._auth_file, 'w')
        config_file.writelines(json.dumps(data))

    def _get_auth_json(self):
        """
        Returns the auth JSON object as acquired from the auth file. If none, throws exception asking the user
        to first authenticate.
        :return: The JSON object with the auth information
        """
        if not self._auth_exists():
            raise ValueError("CLI not authenticated. Please run `client.py --auth` first.")
        with open(self._auth_file) as file:
            config_contents = file.read()
        try:
            return json.loads(config_contents)
        except (TypeError, JSONDecodeError):
            raise ValueError("Secret formatting has been mangled. Try re-authenticating (`client.py --auth`).")

    @staticmethod
    def _parse_api_key(api_key):
        """
        Determines if the API key supplied is valid via regex. Returns the parsed contents in a dict (user and key)
        :param api_key: The string containing the API key
        :return: A dict containing the parse contents of the api key (user and key)
        """
        config_result = {}
        key_regex = re.compile("\d+:[0-9a-fA-F]{32}")
        if not api_key or not re.match(key_regex, api_key):
            raise ValueError("Malformed API Key. Expected {}".format(Config._key_example))
        config_result[Config._api_key_key] = api_key
        config_result[Config._user_key] = api_key.split(Config._key_delimiter)[Config._user_position]
        return config_result


def _parse_arguments():
    """
    Simple argparser
    :return: parsed arguments if any. Prints help otherwise
    """
    parser = argparse.ArgumentParser(description="Halite 2.0 CLI")
    parser.add_argument('-a', '--auth', action='store_true',
                        help="Authorize the client to make requests on your behalf")
    parser.add_argument('-b', '--bot-path', dest='bot_path', action='store', type=str,
                        help="The path wherein your bot zip lives.")
    if len(sys.argv) < 2:
        parser.print_help()
    return parser.parse_args()


def _bot_exists(user_id):
    """
    Whether a bot has been previously uploaded. Assumes that a user only has bot 0
    :param user_id: The id of the user making the request
    :return: True if created, False otherwise
    """
    return requests.get(_URI_API_EXISTING_BOT.format(user_id, _FIRST_BOT_ID)).status_code == _SUCCESS


def _upload_bot(user_id, api_key, bot_path):
    """
    Uploads the bot to the Halite servers. If the bot exists, simply update using PUT. Otherwise create it with
    POST. NOTE: Assuming only bot is bot 0
    :param user_id: The ID of the user making the request
    :param api_key: The API key provided by the server (acquired in the auth phase)
    :param bot_path: The path wherein the bot is found (expected file)
    :return: The result of the request to the server
    """
    files = {_BOT_FILE_KEY: open(bot_path, 'rb')}
    headers = {_API_KEY_HEADER: api_key}
    if _bot_exists(user_id):
        return requests.put(_URI_API_EXISTING_BOT.format(user_id, _FIRST_BOT_ID), files=files, headers=headers)
    else:
        return requests.post(_URI_API_CREATE_BOT.format(user_id), files=files, headers=headers)


def authorize():
    """
    Create the config for the user. This will ask the user to visit a webpage and paste the api key encountered.
    :return: Nothing
    """
    api_key = input("Please go to {} to obtain an api_key, and paste here: ".format(_URI_WEB_API_KEY))
    Config(api_key)
    print("Successfully set up user account")


def upload(bot_path):
    """
    Uploads the bot placed under bot_path. May only be called once Config is properly initialized.
    :param bot_path: The path wherein the bot is located
    :return: Nothing
    """
    config = Config()
    if not bot_path or not os.path.isfile(bot_path):
        raise ValueError("Bot path is not valid or does not exist. Try again.")
    print("Uploading bot...")
    result = _upload_bot(config.user_id, config.api_key, bot_path)
    if result.status_code != _SUCCESS:
        raise IOError("Unable to upload bot: {}".format(result.text))
    print("Successfully uploaded bot")


def main():
    """
    Main function gets the args input and determines which method to call to handle. Handles exceptions from
    malformed input.
    :return: Nothing
    """
    try:
        args = _parse_arguments()
        if args.auth:
            authorize()
        elif args.bot_path:
            upload(args.bot_path)
    except (ValueError, IOError) as err:
        print(err)
        exit(-1)

if __name__ == "__main__":
    main()
